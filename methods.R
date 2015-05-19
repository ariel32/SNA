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



searchVGMUstudents <- function(age1=0, age2=0, fields="", sex=0) {
    url = sprintf("https://api.vk.com/method/users.search?
                  university=2918
                  &count=1000
                  &age_from=%s
                  &age_to=%s
                  &fields=%s
                  &sex=%s
                  &access_token=%s"
                  , age1
                  , age2
                  , fields
                  , sex
                  , ACCESS_TOKEN)
    
    url <- gsub("\n                  ", "", url) #колдовство, чтобы url был и вебочитаем и не в одну строку
    res <- fromJSON(url)$response
    return(res)
}
######
c(
  searchVGMUstudents(10, 16),
  searchVGMUstudents(17, 17),
  searchVGMUstudents(18, 18),
  searchVGMUstudents(19, 19, "", 1),
  searchVGMUstudents(19, 19, "", 2),
  searchVGMUstudents(20, 20, "", 1),
  searchVGMUstudents(20, 20, "", 2),
  searchVGMUstudents(21, 21, "", 1),
  searchVGMUstudents(21, 21, "", 2),
  searchVGMUstudents(22, 22, "", 1),
  searchVGMUstudents(22, 22, "", 2),
  searchVGMUstudents(23, 23, "", 1),
  searchVGMUstudents(23, 23, "", 2),
  searchVGMUstudents(24, 24, "", 1),
  searchVGMUstudents(24, 24, "", 2),
  searchVGMUstudents(25, 25, "", 1),
  searchVGMUstudents(25, 25, "", 2),
  searchVGMUstudents(26, 26),
  searchVGMUstudents(27, 27),
  searchVGMUstudents(28, 28),
  searchVGMUstudents(29, 29),
  searchVGMUstudents(30, 30),
  searchVGMUstudents(31, 33),
  searchVGMUstudents(34, 39),
  searchVGMUstudents(40, 49),
  searchVGMUstudents(50, 100),
  searchVGMUstudents(101, 120)) -> a Экстракция данных
#####

all_uids = vector()
# собираем все данные с uid (были склеены объекты вк, первый элемент которых содержит сумму следующих)
for(x in 1:16252) { if("uid" %in% names(a[[x]])) all_uids = append(all_uids, a[[x]]$uid) }



save(file = "all_uids", all_uids)
save(file = "all_personal", all_personal)
load("all_uids")

all_personal = data.frame()
for(x in all_uids) {
  pattern = data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA)
  names(pattern) <- c("uid", "political", "langs", "religion", "inspired_by", "people_main", "life_main", "smoking", "alcohol")
  
  a <- getUserInfo(x)
  Sys.sleep(0.5)
  if("personal" %in% names(a) & length(a$personal[[1]]) > 0) {
    pattern$uid = x
    for(y in names(a$personal)) pattern[y] <- a$personal[y]
    all_personal = rbind(all_personal, pattern)
  }
}


