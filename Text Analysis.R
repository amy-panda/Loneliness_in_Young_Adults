# Load the library
library(tm)
library(textstem)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(topicmodels)
library(ldatuning)
# library(SnowballC)

# Create Corpus
docs <- VCorpus(DirSource("txt_docs"))
docs_list <-row.names(summary(docs))

# Inspect a particular document
writeLines(as.character(docs[[1]]))

# Data preprocessing ---------------------------------------------------------

# Convert to lower cases
docs <- tm_map(docs,content_transformer(tolower))

# Create customised stopwords and remove them from documents

customised_stopwords <- c("x","r","interviewer","interview","interviewee",
                          "respondent","participant","box","part",
                          "um","Mmh","mhm","mm","mmm","hmm","mmh","erm","uhm","umm","please","yeah","yea",
                          "pause","thank","thanks","can","just",
                          "you","youre","make","get","thing","things")
                          
docs <- tm_map(docs, removeWords, customised_stopwords)


# Convert "'" to "'" before stopwords are removed

docs <- tm_map(docs, content_transformer(gsub), pattern = "'", replacement = "'")

# Remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuation and replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)

# Create the toSpace content transformer and remove special symbols including """,""",".","'","-"
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, """)
docs <- tm_map(docs, toSpace, """)
docs <- tm_map(docs, toSpace, ".")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "-")

# Strip whitespace
docs <- tm_map(docs, stripWhitespace)

# Strip digits
docs <- tm_map(docs,removeNumbers)

# Inspect output
writeLines(as.character(docs[[10]]))

# Stem document with lemmatization
# docs <- tm_map(docs,stemDocument)
docs <- tm_map(docs, lemmatize_strings)

# Convert to 'PlainTextDocument' type object
docs <- tm_map(docs, PlainTextDocument)

# #Stem document
# docs <- tm_map(docs,stemDocument)


# DTM with exclusion of highest and lowest frequency -----------------------

# Create document-term matrix
dtm <- DocumentTermMatrix(docs)

# Get the frequency of occurrence of each word in the corpus
freq <- colSums(as.matrix(dtm))


#length should be total number of terms
length(freq)

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)

#inspect most frequently occurring terms
freq[head(ord)]

#inspect least frequently occurring terms
freq[tail(ord)]  

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
findAssocs(dtmr,"guess",0.5)

# Create the graph for most frequent words

wf=data.frame(term=names(freqr),occurrences=freqr)

ggplot(subset(wf,freqr>200),aes(reorder(term,-occurrences),occurrences))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1))


# Set the same seed each time ensures consistent look across clouds
set.seed(1234)

# Create a wordcloud 
wordcloud(names(freqr),freqr,min.freq = 100,random.order=FALSE
          ,scale=c(3,0.25),rot.per = 0.35,colors=brewer.pal(n=9,name="Blues"))

# Create a word cloud with wordcloud2
wordcloud2(data=subset(wf,freqr>100), size=0.3,
           shape = 'pentagon')

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

findAssocs(dtm_tfidf,"music",0.8)
findAssocs(dtm_tfidf,"unintelligible",0.7)
findAssocs(dtm_tfidf,"dog",0.8)
findAssocs(dtm_tfidf,"meditation",0.9)

# Create the graph for most frequent words

wf_tfidf=data.frame(term=names(freq_tfidf),occurrences=freq_tfidf)

ggplot(subset(wf_tfidf,freq_tfidf>0.04),aes(reorder(term,-occurrences),occurrences))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45,hjust=1))


# Set the same seed each time ensures consistent look across clouds
set.seed(1234)

# Create a wordcloud 
wordcloud(names(freq_tfidf),freq_tfidf,min.freq = 0.01,random.order=FALSE,max.words = 100
          # ,scale=c(3,0.25),rot.per = 0.35
          ,colors=brewer.pal(n=9,name="Blues"))

# Create a word cloud with wordcloud2
wordcloud2(data=subset(wf_tfidf,freq_tfidf>0.03), size=0.3,
           shape = 'pentagon')


# Topic Modelling ---------------------------------------------------------

# Create document-term matrix
dtm <- DocumentTermMatrix(docs
                          # ,control=list(bounds = list(global = c(3,Inf)))
                          )
# Add the document names
dtm$dimnames$Docs=docs_list


# create models with different number of topics
result <- FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 10, by = 1),
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
k <- 9

# # set random number generator seed
# set.seed(9161)

#Set parameters for Gibbs sampling
burnin <- 0
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765,9161,123,1200,4567)
nstart <- 9
best <- TRUE

#Run LDA using Gibbs sampling
ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

# Look at the 10 most likely terms under each topic
terms(ldaOut,15)

# top5termsPerTopic <- terms(ldaOut, 5)
# topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")


#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics

a <- cbind(data.frame(ldaOut.topics[,1],row.names=NULL),row.names(ldaOut.topics))
colnames(a) <- c("topic","document")
a[order(a$topic),]


# #write out results
# #docs to topics
# ldaOut.topics <- as.matrix(topics(ldaOut))
# write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,12))
write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))








