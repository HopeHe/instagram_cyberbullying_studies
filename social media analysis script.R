library(tidyverse)
library(stringr)
library(lubridate)

d <- read_csv("sessions_40plus_metadata.csv")
tsdata.d <- select(d, `_unit_id`, `cptn_time`,likes,`shared media`,`followed_by`,follows,cyberaggression,cyberbullying)

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

# And time is splited so we can evaluate the timex effects by years, months and hours 
tsdata.d$date <- date(tsdata.d$time)
tsdata.d$year <- year(tsdata.d$time) 
tsdata.d$month <- month(tsdata.d$time)
tsdata.d$day <- day(tsdata.d$time)
tsdata.d$hour <- hour(tsdata.d$time)

# Finally form them into the readable table
tsdata.d <- 
  tsdata.d %>% 
  select(., id, date, year, month, day, hour , likes, shares, fame, cyberaggression, cyberbullying)

## Preliminary Data Plot to visualize the relations between variables  
plot(tsdata.d[,c(2,7:11)])
ggplot(data= tsdata.d) + geom_line(aes(cptn_time, cyberbullying))

datahour <- 
  group_by(tsdata.d, hour) %>% 
  mutate(., ave_aggression = mean(cyberaggression)) %>%
  mutate(., ave_cyberbullying = mean(cyberbullying))
ggplot(data = datahour) + geom_line(aes(hour, ave_cyberbullying)) 

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
## sentiments analysis 
library(tidytext)
sentiments