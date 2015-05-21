library(jsonlite)
setwd("/home/capsula/work/SNA/")
source("data/secret.R") # токен находится в переменной ACCESS_TOKEN

id0 = 6529847
APP_ID = 4917761
VERSION_API = 5.32


getUserInfo <- function(uid, fields = "all") {
  if (fields == "all"){
    fields = "sex,bdate,city,country,contacts,education,universities,schools,relation,activities,personal,last_seen"
  }
  Sys.sleep(0.5)
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
getWallSearch <- function(uid, query) {
  # функция вернет лист, в [[1]] которого лежит количество постов, а в [[x]] - информация о постах
  url = sprintf("https://api.vk.com/method/wall.search?owner_id=%s&query=%s&access_token=%s", getUserInfo(uid)$uid, query, ACCESS_TOKEN)
  res <- fromJSON(url)$response
  return(res)
}
getWallPosts <- function(uid) {
  if(!is.numeric(uid) | uid > 0) {uid <- getUserInfo(uid)$uid}
  url = sprintf("https://api.vk.com/method/wall.get?owner_id=%s&count=100&access_token=%s",
                uid, ACCESS_TOKEN)
  res <- fromJSON(url)$response
  postCount = res[[1]]
  posts = res[2:101]
  for(x in 1:floor(postCount/100)) {
    url = sprintf("https://api.vk.com/method/wall.get?owner_id=%s&count=100&offset=%s&access_token=%s",
                  uid, x*100, ACCESS_TOKEN)
    res <- fromJSON(url)$response
    posts = append(posts, res[2:101])
    print(sprintf("%s/%s", x, floor(postCount/100)))
  }
  
  return(Filter(Negate(is.null), posts))
}
getGropus <- function(uid) {
  url = sprintf("https://api.vk.com/method/groups.get?user_id=%s&extended=1&access_token=%s",
                getUserInfo(uid)$uid, ACCESS_TOKEN)
  res <- fromJSON(url)$response
  
  return(res)
}

sendMessage <- function(uid, message) {
  message = gsub(" ", "%20", message)
  url = sprintf("https://api.vk.com/method/messages.send?user_id=%s&message=%s&access_token=%s",
                getUserInfo(uid)$uid, message, ACCESS_TOKEN)
  res <- fromJSON(url)$response

  return(res)
}
getMessage <- function(uid="", count=1) {
  # если задан конкретный пользователь, возвращаем историю переписки с ним
  if(uid != "") {
    url = sprintf("https://api.vk.com/method/messages.getHistory?count=%s&user_id=%s&access_token=%s",
                  count, getUserInfo(uid)$uid, ACCESS_TOKEN)
    res <- fromJSON(url)$response
    for(x in 1:count) {
      print(sprintf("%s %s: %s",
                    getUserInfo(res[[x+1]]$uid)$first_name,
                    getUserInfo(res[[x+1]]$uid)$last_name,
                    res[[x+1]]$body))
      }
    } else {
    url = sprintf("https://api.vk.com/method/messages.get?count=%s&access_token=%s",
                  count, ACCESS_TOKEN)
    res <- fromJSON(url)$response
    for(x in 1:count) {
      if (res[[x+1]]$read_state != 1) {
        print(sprintf("%s %s (%s): %s",
                      getUserInfo(res[[x+1]]$uid)$first_name,
                      getUserInfo(res[[x+1]]$uid)$last_name,
                      res[[x+1]]$uid,
                      res[[x+1]]$body))
      } else {
        print("------------------")
        break
      }
    }
  }
}
getError <- function(object) {
  # обрабатываем ошибки объекта user
  if("deactivated" %in% names(object) && object$deactivated == "deleted") return("DELETED")
  if("deactivated" %in% names(object) && object$deactivated == "banned") return("BANNED")  
}


