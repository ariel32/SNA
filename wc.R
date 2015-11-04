source("methods.R")
library(jsonlite)
library(Unicode)
library(tm)
library(wordcloud)

uid <- getUserInfo("salut_ice")$uid
url = sprintf("https://api.vk.com/method/messages.getHistory?count=%s&user_id=%s&access_token=%s",
              100, uid, ACCESS_TOKEN)

res <- fromJSON(url)$response

postCount = res[[1]]
posts = res[2:101]

for(x in 1:floor(postCount/100)) {
  url = sprintf("https://api.vk.com/method/messages.getHistory?user_id=%s&count=100&offset=%s&access_token=%s",
                uid, x*100, ACCESS_TOKEN)
  res <- fromJSON(url)$response
  posts = append(posts, res[2:101])
  print(sprintf("%s/%s", x, floor(postCount/100)))
}

a <- Filter(Negate(is.null), posts)
d = vector()
for(x in 1:length(a)) d = append(d, a[[x]]$body)


d <- gsub('<.+?>',' ',d) # Тэги? В моя корпус?
d <- gsub('http:/[^[:space:]<>]+',' ',d) # ссылки -- вон!
d <- gsub('https:/[^[:space:]<>]+',' ',d) # ссылки -- вон!
d <- gsub("\\d", " ", d) # удаляем цифры
d <- gsub("[[:punct:]]", " ", d) # удаляем пунктуацию
d <- gsub("\\b[[:alnum:]]{1,4}\\b", " ", d) # удаляем слова с длиной меньше 5
d <- gsub("\\b[[:alnum:]]{7,}\\b", " ", d) # удаляем слова длиной больше 6
mystopwords = readLines("stop-words.txt")
d <- removeWords(d, mystopwords)
d <- gsub("^ +|[[:space:]]+| +$", " ", d, perl = TRUE) # удаляем множественные пробелы
d <- gsub("^\\s+|\\s+$", "", d) # удаляем пробелы в начале и конце строки
d <- d[-which(d == " " | d == "")]
for(x in 1:length(d)) {if(nchar(d[x]) != 0) {d[x] = u_to_lower_case(d[x])}} # переводим в нижний регистр


myCorpus <- Corpus(VectorSource(d))
#myCorpus <- tm_map(myCorpus, stemDocument)

save(myCorpus, file = "data/myCorpus"); rm(list = ls()); load("data/myCorpus")

ap.tdm <- TermDocumentMatrix(myCorpus)
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
pal2 <- brewer.pal(8,"Dark2")

png("wordcloud_packages.png", width=1280,height=800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
