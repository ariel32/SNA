library(jsonlite)
setwd("/home/capsula/work/SNA/")

# подгружаем токен
# https://oauth.vk.com/authorize?client_id=4917761&
#                        scope=friends,offline,wall,groups,messages&
#                        redirect_uri=https://oauth.vk.com/blank.html&display=page&v=5.21&response_type=token

id0 = 6529847
APP_ID = 4917761
source("secret.R") # токен находится в переменной ACCESS_TOKEN

getUserInfo <- function(uid, fields = "all") {
  if (fields == "all"){
    fields = "sex,bdate,city,country,contacts,education,universities,schools,relation,activities,last_seen"
  }
  
  url = sprintf("http://api.vk.com/method/users.get?uids=%s&fields=%s", uid, fields)
  
  res <- fromJSON(url)$response
  return(res)
}
getFriends <- function(uid) {
  url = sprintf("http://api.vk.com/method/friends.get?user_id=%s&fields=uid", getUserInfo(uid)$uid)
  res <- fromJSON(url)
  res <- res$response[1:3]
  
  return(res)
}
getSubscriptions <- function(uid) {
  url = sprintf("http://api.vk.com/method/users.getSubscriptions?user_id=%s&extended=1"
                , getUserInfo(uid)$uid)
  res <- fromJSON(url)$response[1:5]
  
  return(res)
}
getWallPost <- function(uid, query) {
  # функция вернет лист, в [[1]] которого лежит количество постов, а в [[x]] - информация о постах
  url = sprintf("http://api.vk.com/method/wall.search?owner_id=%s&query=%s", getUserInfo(uid)$uid, query)
    print(url)
  res <- fromJSON(url)$response[1:5]
  return(res)
}






getStatus <- function(uid) {
  url = sprintf("http://api.vk.com/method/users.get?uids=%s&fields=status", uid)
  res <- fromJSON(url)
  
  time <- as.numeric(Sys.time())
  if(!is.null(res$response$deactivated)) {
    res$response$status <- "DELETED"
    res$response$first_name <- "DELETED"
    res$response$last_name <- "DELETED"
  }
  
  res <- data.frame(time
                    , res$response$uid
                    , res$response$first_name
                    , res$response$last_name
                    , res$response$status
                    , stringsAsFactors = F)
  
  colnames(res) <- c("time", "uid", "first_name", "last_name", "status")
  return(res)
}
StatusUpdate <- function(uid) {
  # получаем обновление статуса
  status.update = getStatus(uid)
  
  if(file.exists("data.csv")) {
    # читаем БД, заменяем NA пустыми строками
    d = read.csv("data.csv", sep = ";"); d$status[is.na(d$status)]<-""
    
    # если последний статус в БД и текущий статус пользователя различаются, обновляем
    if (status.update$status != d$status[length(d$status)] | length(d$status) == 0)
      write.table(x = status.update, file = "data.csv", sep = ";", append = T, col.names = F, row.names = F)
  } else {
    # проверяем, есть ли БД со статусами - если нет, создаем структуру таблицы и обновляем
    mud = cbind("time", "uid", "first_name", "last_name", "status")
    write.table(x = mud, file = "data.csv", sep = ";", row.names = F, col.names = F)
    write.table(x = status.update, file = "data.csv", sep = ";", append = T, col.names = F, row.names = F)  
  }
}

StatusUpdate("salut_ice")
