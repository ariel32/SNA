library(jsonlite)
setwd("/home/capsula/work/SNA/")
source("secret.R") # токен находится в переменной ACCESS_TOKEN

id0 = 6529847
APP_ID = 4917761
VERSION_API = 5.32

# MAKE A FUNCTION FOR CAPTCHA PROCESSING

getUserInfo <- function(uid, fields = "all") {
  if (fields == "all"){
    fields = "sex,bdate,city,country,contacts,education,universities,schools,relation,activities,last_seen"
  }
  
  url = sprintf("https://api.vk.com/method/users.get?uids=%s&fields=%s&access_token=%s", uid, fields, ACCESS_TOKEN)
  res <- fromJSON(url)$response
  return(res)
}
getFriends <- function(uid) {
  url = sprintf("https://api.vk.com/method/friends.get?user_id=%s&fields=uid&access_token=%s", getUserInfo(uid)$uid, ACCESS_TOKEN)
  res <- fromJSON(url)
  res <- res$response[1:3]
  
  return(res)
}
getSubscriptions <- function(uid) {
  url = sprintf("https://api.vk.com/method/users.getSubscriptions?user_id=%s&extended=1&access_token=%s", getUserInfo(uid)$uid, ACCESS_TOKEN)
  res <- fromJSON(url)$response[1:5]
  
  return(res)
}
getWallPost <- function(uid, query) {
  # функция вернет лист, в [[1]] которого лежит количество постов, а в [[x]] - информация о постах
  url = sprintf("https://api.vk.com/method/wall.search?owner_id=%s&query=%s&access_token=%s", getUserInfo(uid)$uid, query, ACCESS_TOKEN)
  res <- fromJSON(url)$response[1:5]
  return(res)
}
getGropus <- function(uid) {
  url = sprintf("https://api.vk.com/method/groups.get?user_id=%s&extended=1&access_token=%s",
                getUserInfo(uid)$uid, ACCESS_TOKEN)
  res <- fromJSON(url)$response
  
  return(res)
}

sendMessage <- function(uid, message) {
  url = sprintf("https://api.vk.com/method/messages.send?user_id=%s&message=%s&access_token=%s",
                getUserInfo(uid)$uid, message, ACCESS_TOKEN)
  res <- fromJSON(url)$response
  
  return(res)
}















