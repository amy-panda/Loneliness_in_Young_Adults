# Load the library
library(tm)
library(textstem)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)

# Create Corpus
docs <- VCorpus(DirSource("txt_docs"))

# Inspect a particular document
writeLines(as.character(docs[[30]]))

# Data cleansing  ------------------------------------------------------------

# Convert to lower cases
docs <- tm_map(docs,content_transformer(tolower))

# Create customised stopwords and remove them from documents

customised_stopwords <- c("x","r","interviewer","interview","interviewee",
                          "respondent","participant","box","part",
                          "um","Mmh","mhm","mm","mmm","hmm","mmh","erm","uhm","please","yeah","yea",
                          "pause","thank","thanks","can","just",
                          "you","youre")
                          
docs <- tm_map(docs, removeWords, customised_stopwords)


# Convert "’" to "'" before stopwords are removed

docs <- tm_map(docs, content_transformer(gsub), pattern = "’", replacement = "'")

# Remove stopwords using the standard list in tm
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove punctuation and replace punctuation marks with " "
docs <- tm_map(docs, removePunctuation)

# Create the toSpace content transformer
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))})
docs <- tm_map(docs, toSpace, "‘")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "”")
docs <- tm_map(docs, toSpace, "…")
docs <- tm_map(docs, toSpace, "–")

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
wordcloud(names(freq_tfidf),freq_tfidf,min.freq = 0.03,random.order=FALSE
          ,scale=c(3,0.25),rot.per = 0.35,colors=brewer.pal(n=9,name="Blues"))

# Create a word cloud with wordcloud2
wordcloud2(data=subset(wf_tfidf,freq_tfidf>0.03), size=0.3,
           shape = 'pentagon')



