rm(list = ls())
source("methods.R")

library(jsonlite)
library(tm)
library(wordcloud)


a <- getWallPosts(-62338399)
d = vector()
for(x in 1:length(a)) d = append(d, paste("", a[[x]]$text, ""))
rm(a)
# IT IS WORK! gsub("\\b[[:alnum:]]{1,6}\\b", "", a)
d <- gsub("(?<=[[:blank:]])[[:graph:]]{7,}(?=[[:blank:]])", " ", d ,perl = T) # удаляем слова длиной больше 7
d <- gsub("\\b[[:alnum:]]{1,3}\\b", " ", d) # удаляем слова с длиной меньше 3
d <- gsub("\\b[[:alnum:]]{7,}\\b", " ", d) # удаляем слова длиной больше 7
d <- gsub("^\\s+|\\s+$", "", d) # удаляем пробелы в начале и конце - мы их до этого героически впендюрили
d <- gsub("<br>", " ", d)
d <- gsub("[[:punct:]]", " ", d) # удаляем пунктуацию
d <- gsub("^ *|(?<= ) | *$", "", d, perl = TRUE) # удаляем множественные пробелы

#myCorpus <- Corpus(VectorSource(enc2native(d)))
myCorpus <- Corpus(VectorSource(d))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
#myCorpus <- tm_map(myCorpus, removeNumbers)

mystopwords = readLines("stop-words.txt")
#mystopwords <- sort(unique(mystopwords)); write(mystopwords, "stop-words.txt")

myCorpus <- tm_map(myCorpus, removeWords, mystopwords)




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
