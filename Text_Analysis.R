# Load the library
library(tm)
library(textstem)
library(ggplot2)
library(wordcloud2)
library(RColorBrewer)
library(topicmodels)
library(ldatuning)
library(tidytext)
library(dplyr)
library(stringr)

# Create Corpus
docs <- VCorpus(DirSource("txt_docs"))
docs_list <-row.names(summary(docs))

# Inspect a particular document
writeLines(as.character(docs[[1]]),)

# Data preprocessing ---------------------------------------------------------

# Convert to lower cases
docs <- tm_map(docs,content_transformer(tolower))

# Create customised stopwords and remove them from documents
customised_stopwords <- c("x","r","interviewer","interview","interviewee",
                          "respondent","participant","box","part",
                          "uhuum","uhhu","um","Mmh","mhm","mm","mmm","hmm","mmh","erm","uhm","umm","uuuuum","mmmm","hmmm",
                          "uhhu","please","yeah","yea","yeahhhh","yeaaa","pause","thank","thanks","can","just",
                          "you","youre","make","made","makes","get","thing","things","year","years",
                          "number","though","london","something","gonna","one")

docs <- tm_map(docs, removeWords, customised_stopwords)


# Convert "’" to "'" before stopwords are removed

docs <- tm_map(docs, content_transformer(gsub), pattern = "’", replacement = "'")

# Remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuation and replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)

# Create the toSpace content transformer and remove special symbols including "“","”","…","'","-"," –"
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "…")
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "–")
docs <- tm_map(docs, toSpace, " –")

# Strip whitespace
docs <- tm_map(docs, stripWhitespace)

# Strip digits
docs <- tm_map(docs,removeNumbers)

# Inspect output
writeLines(as.character(docs[[10]]))

# Stem document with lemmatization
docs <- tm_map(docs, lemmatize_strings)

# Convert to 'PlainTextDocument' type object
docs <- tm_map(docs, PlainTextDocument)

# DTM with exclusion of highest and lowest frequency -----------------------

#wordlengths: remove very frequent and very rare words
#bounds: include only words that occur in at least / at most n_lower / n_upper docs
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),
                                             bounds = list(global = c(3,40))))

# Get the frequence of new corpus
freqr <- colSums(as.matrix(dtmr))

#create sort order (desc)
ordr <- order(freqr,decreasing=TRUE)

#inspect most frequently occurring terms
freqr[head(ordr)]

#inspect least frequently occurring terms
freqr[tail(ordr)]  

# Find the words with at least 100 times of frequency
findFreqTerms(dtmr,lowfreq=100)

# Find the associated words with most used words
findAssocs(dtmr,"obviously",0.5)
findAssocs(dtmr,"sense",0.5)
findAssocs(dtmr,"medium",0.5)
findAssocs(dtmr,"guess",0.6)
findAssocs(dtmr,"type",0.8)
findAssocs(dtmr,"laugh",0.6)


# Create the graph for most frequent words
wf=data.frame(term=names(freqr),occurrences=freqr)

ggplot(subset(wf,freqr>200),aes(reorder(term,-occurrences),occurrences))+
  geom_bar(stat="identity",fill="skyblue")+
  labs(y="Occurrences",x="")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))


# Set the same seed each time ensures consistent look across clouds
set.seed(1234)


# Create a word cloud with wordcloud2
wordcloud2(data=subset(wf,freqr>100), size=0.3,
           shape = 'circle')

# TF-IDF (term frequency-inverse document frequency)------------------------

# Create document-term matrix
dtm_tfidf <- DocumentTermMatrix(docs,control=list(weighting = weightTfIdf))

# Get the frequency of occurrence of each word in the corpus
freq_tfidf <- colSums(as.matrix(dtm_tfidf))


#length should be total number of terms
length(freq_tfidf)

#create sort order (descending)
ord_tfidf <- order(freq_tfidf,decreasing=TRUE)

#inspect most frequently occurring terms
freq_tfidf[head(ord_tfidf)]

#inspect least frequently occurring terms
freq_tfidf[tail(ord_tfidf)]  

# Find the associated words with most used words
findAssocs(dtm_tfidf,"mum",0.9)
findAssocs(dtm_tfidf,"mom",0.9)
findAssocs(dtm_tfidf,"music",0.7)
findAssocs(dtm_tfidf,"unintelligible",0.7)
findAssocs(dtm_tfidf,"dog",0.8)
findAssocs(dtm_tfidf,"meditation",0.9)

# Create the graph for most frequent words

wf_tfidf=data.frame(term=names(freq_tfidf),occurrences=freq_tfidf)

ggplot(subset(wf_tfidf,freq_tfidf>0.04),aes(reorder(term,-occurrences),occurrences))+
  geom_bar(stat="identity",fill="springgreen3")+
  labs(x="",y="Occurrences")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45,hjust=1))


# Set the same seed each time ensures consistent look across clouds
set.seed(1234)

# Create a word cloud with wordcloud2
wordcloud2(data=subset(wf_tfidf,freq_tfidf>0.03), size=0.3,
           shape = 'circle')

# Topic Modelling ---------------------------------------------------------

# Create document-term matrix
dtm <- DocumentTermMatrix(docs)

# Add the document names
dtm$dimnames$Docs=docs_list

# create models with different number of topics
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 4L,
  verbose = TRUE
)


# create the graphs for optimal topics number K
# minimize - Arun2010 and CaoJuan2009; maximize - Deveaud2014 and Griffiths2004

FindTopicsNumber_plot(result)

# Choose number of topics K=9 based on plot
k <- 10

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,10001,765,9161,123,1200,7542,1256)

nstart <- 10
best <- TRUE

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

# Word-topic probabilities
topics <- tidy(ldaOut,matrix="beta")
topics

top_terms <- topics %>%
  group_by(topic) %>%
  dplyr::slice_max(beta, n = 15) %>% 
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free",nrow = 2) +
  scale_y_reordered()+
  scale_fill_brewer(palette = "Paired")+
  labs(x="",y="")+
  theme_bw()


# Generate word cloud for each topic
topics %>% 
  filter(topic==10) %>% 
  arrange(desc(beta)) %>% 
  slice(16:200) %>% 
  select(-topic) %>% 
  wordcloud2(size=0.5)


#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics

a <- cbind(data.frame(ldaOut.topics[,1],row.names=NULL),row.names(ldaOut.topics))
colnames(a) <- c("topic","document")
a[order(a$topic),]

# Main topic for each document
documents %>% 
  group_by(document) %>% 
  slice_max(gamma, n = 1) %>% 
  ungroup() %>%
  arrange(document,-gamma)

# Document-topic probabilities
documents <- tidy(ldaOut,matrix="gamma")

documents %>% 
  group_by(topic) %>% 
  summarise(ave_gamma=mean(gamma)) %>% 
  arrange(desc(ave_gamma)) %>% 
  ggplot(mapping=aes(x=reorder(topic,-ave_gamma),y=ave_gamma))+
  geom_col(fill="skyblue")+
  labs(x="Topic",y="Average Proportion")+
  theme_bw()


# Add fields age, gender and ethnicity by extracting info from document names
# credit https://stackoverflow.com/questions/8613237/extract-info-inside-all-parenthesis-in-r
# credit https://stackoverflow.com/questions/66534128/extracting-a-string-from-one-column-into-another-in-r
documents_age_gen_eth <- 
  documents %>%
  mutate(age=str_extract(document,"(?<=, )(\\d+)(?=\\,)"),
         gender=str_extract(document,"(?<=- )(\\w+)(?=\\,)"),
         ethnicity=substring(
           str_extract_all(document,"\\([^()]+\\)"),
           2,nchar(str_extract_all(document,"\\([^()]+\\)"))-1)) %>% 
  select(-document)


# Create boxplot to show the topics proportion in different ethnicities
ggplot(data=documents_age_gen_eth,mapping=aes(x=as.factor(topic),y=gamma,fill=ethnicity)) +
  geom_boxplot()+
  # facet_grid(~ethnicity)+
  labs(x="Topics",y="")+
  scale_fill_brewer(palette = "Pastel2")+
  theme_bw()

# Create boxplot to show the topics proportion in different genders
ggplot(data=documents_age_gen_eth,mapping=aes(x=as.factor(topic),y=gamma,fill=gender)) +
  geom_boxplot()+
  # facet_grid(~gender)+
  labs(x="Topics",y="")+
  scale_fill_brewer(palette = "Pastel1")+
  theme_bw()

# Create boxplot to show the topics proportion with different ages
ggplot(data=documents_age_gen_eth,mapping=aes(x=as.factor(topic),y=gamma,fill=as.factor(age))) +
  geom_boxplot()+
  # facet_grid(~as.factor(age))+
  labs(x="Topics",y="",fill="age")+
  scale_fill_brewer(palette = "Pastel1")+
  theme_bw()



