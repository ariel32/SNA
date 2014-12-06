library(rjson)
library(igraph)
library(data.table)
library(magrittr)
library(dplyr)

remove(users
       ,bounds
       ,public.users.info)

# Получает данные по фрагменту `url`
# подставляет значения в переменные в стиле `sprintf`
json.get <- function(url.part,...){
  
  url.root  <- 'http://api.vk.com/method'  
  fromJSON(file=sprintf(url.part,url.root,...)
                  , method='C')
}

# Получает детальные данные о пользователе по `uid`у,
# отдает одноуровневый `list`
get.userinfo = function(uid){  

  # Причесывает набор полей:
  # * делает структуру плоской
  # * изменяет значения `NULL` на `[NA]` для совместимости 
  #   c функцией `rbindlist` из пакета `data.tables`
  list.select = function(lst){
    return(list(uid = lst$uid,
           last_name = lst$last_name,
           last_seen.time = ifelse(is.null(lst$last_seen$time),c(NA),lst$last_seen$time),
           country = ifelse(is.null(lst$country),c(NA),lst$country)))
  }  
  
  frends.info <-
  json.get('%s/friends.get?user_id=%s&fields=first_name,country,last_seen'
          ,uid)$response %>% 
    lapply(FUN=function(resp){list.select(resp)}) %>%
    rbindlist(fill=TRUE,use.names=TRUE)
  
  # Пропускаем бездрузейных участников
  # боты, задроты, моциопаты -- вышли вон!
  if(nrow(frends.info) == 0){return(NULL)}
  
  return(list(
    user.info = rbindlist(list(list.select(json.get('%s/users.get?user_id=%s&fields=first_name,country,last_seen'
                                                    ,uid)$response[[1]])
                              ,frends.info)
                          ,fill=TRUE,use.names=TRUE)
    ,bounds = rbindlist(list(list(src=rep(uid,nrow(frends.info))
                                  ,rsv=frends.info$uid))
                        ,fill=TRUE,use.names=TRUE)
    )
  )

}

# Получает список членов группы, отдает в виде вектора `uid`ов
users <- json.get('%s/groups.getMembers?group_id=1907855')$response$users

# Получает детальные записи о пользователях,
# складывает в data.table
public.users.info <- users[1:150] %>% 
    lapply(FUN=get.userinfo)

userinfo.all <- public.users.info %>% 
  lapply(FUN = function(user){user$user.info}) %>% 
  rbindlist() %>%
  mutate(last_seen.time = as.Date(as.POSIXct(last_seen.time, origin="1970-01-01"))) %>%
  filter(last_seen.time > "2014-10-01" & country == 3) %>%
  distinct(uid)

bounds.all <- public.users.info %>% 
  lapply(FUN = function(user){user$bounds}) %>% 
  rbindlist()

g <- graph.data.frame(bounds.all, directed=FALSE)

summary(g)
g$layout <- layout.fruchterman.reingold(g)
plot(g)

clo <- closeness(g) # rescale values to match the elements of a color vector
clo.score <- round( (clo - min(clo)) * length(clo) / max(clo) ) + 1 # create color vector, use rev to make red "hot"
clo.colors <- rev(heat.colors(max(clo.score)))
V(g)$color <- clo.colors[ clo.score ]
plot(g)
