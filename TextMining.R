rm(list = ls())
source("methods.R")

library(jsonlite)
library(tm)
library(wordcloud)

a <- getWallPosts(-62338399)
d = vector()
for(x in 1:length(a)) d = append(d, a[[x]]$text)
rm(a)
<<<<<<< HEAD

=======
>>>>>>> 57422d2af8287687a3c21995384f6bbc71f09e8c
d <- gsub("<br>", " ", d)
d <- gsub("[[:punct:]]", " ", d)
d <- gsub("^ *|(?<= ) | *$", "", d, perl = TRUE)

myCorpus <- Corpus(VectorSource(d))
myCorpus <- tm_map(myCorpus, content_transformer(tolower), lazy=T)
myCorpus <- tm_map(myCorpus, removeNumbers, lazy=T)

mystopwords = readLines("stop-words.txt")
#mystopwords <- sort(unique(mystopwords)); write(mystopwords, "stop-words.txt")

myCorpus <- tm_map(myCorpus, removeWords, mystopwords, lazy=T)
myCorpus <- tm_map(myCorpus, stripWhitespace, lazy=T)



myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
findFreqTerms(myDtm, lowfreq=10)
findAssocs(myDtm, 'пиво', 0.50)

ap.tdm <- TermDocumentMatrix(myCorpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
pal2 <- brewer.pal(8,"Dark2")

png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
