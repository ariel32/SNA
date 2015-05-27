rm(list = ls())
source("methods.R")

library(jsonlite)
library(Unicode)
library(tm)
library(wordcloud)


a <- getWallPosts(-62338399)
d = vector()
for(x in 1:length(a)) d = append(d, a[[x]]$text)
rm(a)

d <- gsub('<.+?>',' ',d) # Тэги? В моя корпус?
d <- gsub('http:/[^[:space:]<>]+',' ',d) # ссылки -- вон!
d <- gsub("\\d", " ", d) # удаляем цифры
d <- gsub("[[:punct:]]", " ", d) # удаляем пунктуацию
d <- gsub("\\b[[:alnum:]]{1,4}\\b", " ", d) # удаляем слова с длиной меньше 3
d <- gsub("\\b[[:alnum:]]{8,}\\b", " ", d) # удаляем слова длиной больше 7

d <- gsub("^ +|[[:space:]]+| +$", " ", d, perl = TRUE) # удаляем множественные пробелы
for(x in 1:length(d)) {if(nchar(d[x]) != 0) {d[x] = u_to_lower_case(d[x])}} # переводим в нижний регистр

myCorpus <- Corpus(VectorSource(d))

mystopwords = readLines("stop-words.txt")
#mystopwords <- sort(unique(mystopwords)); write(mystopwords, "stop-words.txt")
myCorpus <- tm_map(myCorpus, removeWords, mystopwords)


myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
findFreqTerms(myDtm, lowfreq=1000)
findAssocs(myDtm, 'пиво', 0.50)

ap.tdm <- TermDocumentMatrix(myCorpus)
ap.tdm <- myDtm
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
pal2 <- brewer.pal(8,"Dark2")

png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
