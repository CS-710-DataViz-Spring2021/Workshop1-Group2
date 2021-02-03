packages<-c("ggplot2","tibble","dplyr")
install.packages(setdiff(packages, rownames(installed.packages())))
library(ggplot)
library(tibble)
library(dplyr)
d <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/nba-elo/nbaallelo.csv")
d<- tibble(d)
d <- filter(d,game_location=="H")
results <- d %>% select(game_id,elo_i,opp_elo_i,pts,opp_pts,game_result)%>%
  mutate(point_diff=pts-opp_pts)%>%
  mutate(elo_diff=elo_i-opp_elo_i) %>%
  mutate(game_result=recode(game_result,`L`=0,`W`=1))
