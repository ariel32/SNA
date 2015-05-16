library(jsonlite)
setwd("/home/capsula/work/SNA/")

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
