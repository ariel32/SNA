rm(list = ls())
source("methods.R")

library(tm)
library(wordcloud)

a <- getWallPosts(id0)
d = vector()
for(x in 1:length(a)) d = append(d, a[[x]]$text)

d <- gsub("<br>", " ", d)
d <- gsub("[[:punct:]]", " ", d)
d <- gsub("^ *|(?<= ) | *$", "", d, perl = TRUE)

myCorpus <- Corpus(VectorSource(d))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))

myCorpus <- tm_map(myCorpus, removeNumbers)
mystopwords <- c(stopwords('russian'))
myCorpus <- tm_map(myCorpus, removeWords, mystopwords)

myCorpus <- tm_map(myCorpus, stripWhitespace)

myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))



findFreqTerms(myDtm, lowfreq=10)
findAssocs(myDtm, 'r', 0.30)










