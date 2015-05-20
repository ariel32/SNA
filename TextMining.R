rm(list = ls())
source("methods.R")

library(tm)


a <- getWallPosts(id0)
d = vector()
for(x in 1:length(a)) d = append(d, a[[x]]$text)

myCorpus <- Corpus(VectorSource(d))
myCorpus <- tm_map(myCorpus, tolower)
#myCorpus <- tm_map(myCorpus, removePunctuation)

d <- gsub("[[:punct:]]", " ", d)
d <- gsub("^ *|(?<= ) | *$", "", d, perl = TRUE)

myCorpus <- tm_map(myCorpus, removeNumbers)
mystopwords <- c(stopwords('russian'), )
#myCorpus <- tm_map(myCorpus, removeWords, mystopwords)

dictCorpus <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument)
