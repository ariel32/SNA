library(rjson)
library(igraph)

setwd("E:/Work/Projects/SocialNetworkAnalysis/")
url.users <- 'https://api.vk.com/method/groups.getMembers?group_id=1907855'
users <- fromJSON(file=url.users, method='C')

url.users.info = vector()
users.info = vector()
public.users.info = vector()


# получаем информацию о пользователях
i = 1
for(x in users$response$users) {
  url.users.info[i] <- paste('https://api.vk.com/method/users.get?user_id=',
                             x,
                             '&fields=first_name,country,last_seen', sep = "")
  public.users.info[i] <- fromJSON(file=url.users.info[i], method='C')
  print(i); i = i+1
}
source.public.users.info <- public.users.info
# source.public.users.info -> public.users.info

# чистим от удаленных - ХЗ почему, иногда надо прогнать пару раз
for (x in 1:length(public.users.info)) {
  if(!is.null(public.users.info[[x]][[1]]$deactivated)) {
    public.users.info[[x]] <- NULL
  }
}

for (x in 1:length(public.users.info)) {
  if(as.Date(as.POSIXct(public.users.info[[x]][[1]]$last_seen$time, origin="1970-01-01")) < "2014-10-01") {
    public.users.info[[x]] <- NULL
  }
}

for (x in 1:length(public.users.info)) {
  if(public.users.info[[x]][[1]]$country != 3) {
    public.users.info[[x]] <- NULL
  }
}

# получаем информацию о друзьях пользователей
i = 1
for(x in 1:length(public.users.info)) {
  url.users.info[i] <- paste('https://api.vk.com/method/friends.get?user_id=',
                              public.users.info[[x]][[1]]$uid,
                              '&fields=first_name,country,last_seen', sep = "")
  users.info[i] <- fromJSON(file=url.users.info[i], method='C')
  print(i); i = i+1
}



# all data stored in file 'data'
# 
# load("data")
# as.Date(as.POSIXct(users.info[[1]][[1]]$last_seen$time, origin="1970-01-01"))
# as.Date(as.POSIXct(public.users.info[[x]][[1]]$last_seen$time, origin="1970-01-01")) < "2014-07-01"

active.usersID = vector()
for(x in 1:length(users.info)) {
  if(
    is.null(users.info[[x]]$error_code) # проверяем, не заблокирован ли пользователь
      &
    length(users.info[[x]]) != 0) # и есть ли у него друзья
    
    { active.usersID = append(active.usersID, x) }
}

u.id = vector(); f.id = vector()
for (x in active.usersID) { for(y in 1:length(users.info[[x]])) {
    u.id = append(u.id, users$response$users[x]);
    f.id = append(f.id, users.info[[x]][[y]]$uid)
  }
}

d = as.data.frame(cbind(u.id, f.id))
write.table(d, "data.csv", sep=";", row.names=F)


g <- graph.data.frame(d, directed=FALSE)

summary(g)
g$layout <- layout.fruchterman.reingold(g)
plot(g)

clo <- closeness(g) # rescale values to match the elements of a color vector
clo.score <- round( (clo - min(clo)) * length(clo) / max(clo) ) + 1 # create color vector, use rev to make red "hot"
clo.colors <- rev(heat.colors(max(clo.score)))
V(g)$color <- clo.colors[ clo.score ]
plot(g)








