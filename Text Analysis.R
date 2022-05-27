# Load the library
library(tm)
library(textstem)

# Create Corpus
docs <- VCorpus(DirSource("txt_docs"))

# Inspect a particular document
writeLines(as.character(docs[[30]]))

# Convert to lower cases
docs <- tm_map(docs,content_transformer(tolower))

# Create customised stopwords and remove them from documents

customised_stopwords <- c("x","r","interviewer","interview","interviewee",
                          "respondent","participant","box","part",
                          "um","Mmh","mm","hmm","please","yeah",
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

#Create document-term matrix
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




