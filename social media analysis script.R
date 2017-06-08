library(tidyverse)
library(stringr)
library(lubridate)
library(MASS)
library(tidytext)

d <- read_csv("sessions_40plus_metadata.csv")
tsdata.d <- dplyr::select(d, `_unit_id`, `cptn_time`,likes,`shared media`,`followed_by`,follows,cyberaggression,cyberbullying)
b <- d[,10:204]

for(i in 1:nrow(b)){
  for(j in 1:ncol(b)){
    b[i,j] <-
      b[i,j] %>% 
      str_replace_all(.,"empety","") %>%
      str_replace_all(.,"<font color=\"#0066CC\">[A-z _+@+d0-9+.]+</font>", "") %>% 
      str_replace_all(.,"<font color=\"#0066CC\">", "" ) %>% 
      str_replace_all(.,"</font>", "") %>% 
      str_replace_all(., "()","")
  }
}

b$id <- d$`_unit_id`

##      str_replace_all(., "\(created at:[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}\)","") 
b_plus <- gather(b, "clmn","comment",1:195)
b_plus <- filter(b_plus, b_plus$comment != "")

time <- b_plus$comment %>% str_extract_all(., "[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}")
b_plus$time <- time

b_plus$time <- b_plus$time %>% 
  str_trim() %>% 
  ymd_hms(., tz = "America/New_York")

b_plus$comment <- b_plus$comment %>% 
  str_replace_all(.,"(created at:[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2})","")

b_plus$comment <- b_plus$comment %>% 
  str_to_lower()
b_plus <- dplyr::select(b_plus,id,comment,time)

##b_plus[,2] <- b_plus[,2] %>% 
##str_replace_all(., "[!,.?()_-]", " ") %>%
#  str_replace_all(., "^", " ") %>%
#  str_replace_all(., "[ ]{2,}", " ") %>%
#  str_trim()
for(i in 1:nrow(b_plus)){
     c <- b_plus[i,2][[1]]
     c <- c %>% str_replace_all(., "[!,.?#|*:;()_-]", " ") %>%
           str_replace_all(., "^", " ") %>%
          str_replace_all(., "[ ]{2,}", " ") %>%
           str_trim()
     b_plus$comment[i] <- c
}

write_csv(b_plus, path = "comment")
for(i in 1:nrow(b_plus)){
  word <- b_plus[i,2]
  word_split <- 
    word[[1]] %>%
    strsplit(., split = " ") %>%
    as.data.frame()
  names(word_split) <- "word" 
  word_split <- 
    word_split %>%
    left_join(get_sentiments("bing"), by = "word") %>%
    left_join(get_sentiments("afinn"), by = "word")
  b_plus$index1[i] <- sum(word_split[which(word_split[,3]!="NA"),3])
  b_plus$index2[i] <- count(word_split, sentiment)[1,2] - count(word_split, sentiment)[2,2]
} 
plot(b_plus$time,b_plus$index1)
plot(b_plus$time,b_plus$index2)
plot(b_plus$index1,b_plus$index2)
for(i in 4:5){
  for(j in 1:nrow(b_plus)){
    if(b_plus[j,5] == "NA"){
      b_plus[j,i] == 0
    }
  }
}
word2 <- b_plus[533,2]
duh2 <- word2[[1]] %>% strsplit(., split = " ")
duh2 <- duh2 %>% as.data.frame() 
names(duh2) <- "word"
duh2 <- duh2 %>% 
  left_join(get_sentiments("afinn"), by = "word")
duh2
sum(duh2[which(duh2[,3]!="NA"),3])
count(duh2, sentiment)[1,2] - count(duh2, sentiment)[2,2]

b_temp <- as.data.frame(matrix(NA, nrow(b), 2))
colnames(b_temp)[1] <- "comment"
colnames(b_temp)[2] <- "num_of_commets"

for(i in 1:nrow(b)){
  b_temp$`num_of_commets`[i] <- sum(b[i,]!="")
  temp_list <- as.vector(which(b[i,] !=""))
  b_temp$comment[i] <- 
    b[i,temp_list] %>% 
    paste(., collapse = " ") %>% 
    str_replace_all(., " {2,}"," ") %>% 
    tolower()
}

## Data cleaning
tsdata.d$likes <- 
  tsdata.d$likes %>%
  str_replace(.,"likes","") %>%
  as.integer()

tsdata.d$cptn_time <-
  tsdata.d$cptn_time %>%
  str_replace_all(., "[A-z]+","") %>%
  str_trim() %>%
  paste0("2",.) %>%
  ymd_hms(., tz = "America/New_York")

## rename and manipulate some of the varaibles as so to perform analysis efficiently 
tsdata.d$time <- tsdata.d$cptn_time
tsdata.d$shares <- tsdata.d$`shared media`
tsdata.d$follower <- tsdata.d$followed_by
tsdata.d$id <- tsdata.d$`_unit_id`

# the fame index was calculated by dividing the number of followers by the number of followings
# Notice that there were 8 entries which follow `0` account, thus lead to some erros (infinity) as results
filter(tsdata.d, follows == 0)
which(tsdata.d$follows == 0)
# Thus I mannually changed them to 1 instead of 0  
tsdata.d[which(tsdata.d$follows == 0),]$follows <- 1
tsdata.d$fame <- round(tsdata.d$follower/tsdata.d$follows, digits = 2)

# And time is splited so we can evaluate the time effects by years, months and hours 
tsdata.d$date <- date(tsdata.d$time)
tsdata.d$year <- year(tsdata.d$time) 
tsdata.d$month <- month(tsdata.d$time)
tsdata.d$day <- day(tsdata.d$time)
tsdata.d$hour <- hour(tsdata.d$time)

# Finally form them into the readable table
tsdata.d <- 
  tsdata.d %>% 
  dplyr::select(., id, date, year, month, day, hour , likes, shares, fame, cyberaggression, cyberbullying)

tsdata.d <- cbind(tsdata.d, b_temp$num_of_commets)
## Preliminary Data Plot to visualize the relations between variables  
plot(tsdata.d[,c(2,7:11)])
ggplot(data= tsdata.d) + geom_line(aes(cptn_time, cyberbullying))

datahour <- 
  group_by(tsdata.d, year, month, hour) %>% 
  mutate(., ave_aggression = mean(cyberaggression)) %>%
  mutate(., ave_cyberbullying = mean(cyberbullying))
ggplot(data = datahour) + geom_line(aes(date, ave_cyberbullying)) 

dataday <- 
  group_by(tsdata.d, year, month,day) %>% 
  mutate(., ave_aggression = mean(cyberaggression)) %>%
  mutate(., ave_cyberbullying = mean(cyberbullying))
ggplot(data = dataday) + geom_line(aes(date, ave_cyberbullying)) 

datamonth <- 
  group_by(tsdata.d, year, month) %>% 
  mutate(., ave_aggression = mean(cyberaggression)) %>%
  mutate(., ave_cyberbullying = mean(cyberbullying))
ggplot(data = datamonth) + geom_line(aes(date, ave_cyberbullying))

datayear <- 
  group_by(tsdata.d, year) %>% 
  mutate(., ave_aggression = mean(cyberaggression)) %>%
  mutate(., ave_cyberbullying = mean(cyberbullying))
ggplot(data = datayear) + geom_line(aes(date, ave_cyberbullying))
## sentiments analysis 

OLM <- polr(as.factor(cyberbullying) ~ year+ month+ hour+likes+shares+fame+b_temp$num_of_commets, data= tsdata.d, Hess  = T)
summary(OLM)

library(nnet)
mn <- multinom(as.factor(cyberbullying) ~ year+ month+ hour+likes+shares+fame+b_temp$num_of_commets, data= tsdata.d)
summary(mn)

lmod <- lm(cyberbullying ~ year+ month+ hour+likes+shares+fame+b_temp$num_of_commets, data= tsdata.d)
summary(lmod)

lmod <- lm(cyberaggression ~ year+ month+ hour+likes+shares+fame+b_temp$num_of_commets, data= tsdata.d)
summary(lmod)

summary(lm(b_temp$num_of_commets~ likes+ shares+ fame, data = tsdata.d))