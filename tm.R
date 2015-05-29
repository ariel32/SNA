rm(list = ls())
source("methods.R")

library(jsonlite)
library(Unicode)
library(tm)
library(wordcloud)
library(magrittr)
#
#https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud

a <- getWallPosts(-62338399)
d = vector()
for(x in 1:length(a)) d = append(d, a[[x]]$text)
rm(a)

d %<>% gsub('<.+?>',' ',.) %>% # Тэги? В моя корпус?
  gsub('https?:/[^[:space:]<>]+',' ',.) %>% # ссылки -- вон!
  gsub("\\d", " ", .)  %>% # удаляем цифры
  gsub("[[:punct:]]", " ", .)  %>% # удаляем пунктуацию
  gsub("\\b[[:alnum:]]{1,4}\\b", " ", .)  %>% # удаляем слова с длиной меньше 3
  gsub("\\b[[:alnum:]]{7,}\\b", " ", .) %>% # удаляем слова длиной больше 7
  gsub("^\\s+|\\s+$", "", .) %>%
  gsub("^ +|[[:space:]]+| +$", " ", ., perl = TRUE )# удаляем множественные пробелы

# d <- gsub('<.+?>',' ',d) # Тэги? В моя корпус?
# d <- gsub('http:/[^[:space:]<>]+',' ',d) # ссылки -- вон!
# d <- gsub('https:/[^[:space:]<>]+',' ',d) # ссылки -- вон!
# d <- gsub("\\d", " ", d) # удаляем цифры
# d <- gsub("[[:punct:]]", " ", d) # удаляем пунктуацию
# d <- gsub("\\b[[:alnum:]]{1,3}\\b", " ", d) # удаляем слова с длиной меньше 3
# d <- gsub("\\b[[:alnum:]]{8,}\\b", " ", d) # удаляем слова длиной больше 7
# d <- gsub("^\\s+|\\s+$", "", d)
# d <- gsub("^ +|[[:space:]]+| +$", " ", d, perl = TRUE) # удаляем множественные пробелы


for(x in 1:length(d)) {if(nchar(d[x]) != 0) {d[x] = u_to_lower_case(d[x])}} # переводим в нижний регистр

mystopwords = readLines("stop-words.txt")
d <- removeWords(d, mystopwords)

# удаляем элементы без текста
d = d[-which(d == " " | d == "")]

d <- gsub("^ +|[[:space:]]+| +$", " ", d, perl = TRUE) # удаляем множественные пробелы
myCorpus <- Corpus(VectorSource(d))

myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
findFreqTerms(myDtm, lowfreq=200)
findAssocs(myDtm, 'война', 0.1)

ap.tdm <- myDtm
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
pal2 <- brewer.pal(8,"Dark2")

png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
