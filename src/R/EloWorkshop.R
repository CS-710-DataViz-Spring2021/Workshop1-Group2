packages<-c("ggplot2","tibble","dplyr")
install.packages(setdiff(packages, rownames(installed.packages())))
library(ggplot2)
library(tibble)
library(dplyr)
#downlads the file and reads filters out all away games to avoid duplicate data and forced symmetry
d <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/nba-elo/nbaallelo.csv")
d<- tibble(d)
d <- filter(d,game_location=="H")

#reformats the data selecting only relevant columns performs some transformations
#creates point_diff column home team minus away team points
#creates the elo_diff column. Home team minus away team elo
#Creates a column for the predicted result
#recodes the game result in binary variables with 1 being a win
#creates a column that evaluates whether or not elo accurately predicts the the result of the game
#puts the data into bins for the sake of graphing
bin_width = 50

results <- d %>% select(game_id,elo_i,opp_elo_i,pts,opp_pts,game_result) %>%
  mutate(point_diff=pts-opp_pts) %>%
  mutate(elo_diff=elo_i-opp_elo_i) %>%
  mutate(game_result=recode(game_result,`L`=0,`W`=1)) %>%
  mutate(pred_result=elo_diff>0) %>%
  mutate(pred_accurate= pred_result==game_result) %>%
  mutate(elo_bin=cut(elo_diff,seq(min(elo_diff)%/%bin_width*bin_width-bin_width,
                                  max(elo_diff)%/%bin_width*bin_width+bin_width,by=bin_width),
                     right=FALSE))

results %>%
  ggplot +
  aes(elo_diff,fill=pred_accurate)+
  geom_histogram(color="black",binwidth=bin_width,position="identity")+
  scale_fill_manual(values = c("5dade2aa", "#e68762aa") )
  labs(title= "Accuracy of Prediction based on Elo")
ggsave("pred_plot.jpg")

results %>% ggplot +
  aes(x=elo_diff,y=point_diff,color=elo_i)+
  scale_color_steps()+
  geom_point()+
  labs(title= "Home Team Point Difference vs Elo Difference")
ggsave("point_plot.jpg")
