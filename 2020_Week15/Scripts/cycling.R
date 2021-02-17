# Week 15 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday
library(tidyverse)
library(ggplot2)
library(patchwork)

# Week 15 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday

# Get the Data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

library(tidyverse)
library(tdf) # install at: https://github.com/alastairrushworth/tdf

View(tdf_winners)
tdf_winners<-tdf_winners %>%
  mutate(rate_cycling=distance/time_overall)
avg_time_df<-tdf_winners %>%
  select(c(start_date,winner_name,winner_team,distance,time_overall,rate_cycling,height,weight,age))
View(avg_time_df)


time_plot<-ggplot(data=avg_time_df, aes(x=start_date,y=time_overall))+
  geom_point()+
  geom_line()+
  theme_bw()
distance_plot<-ggplot(data=avg_time_df, aes(x=start_date,y=distance))+
  geom_point()+
  geom_line()+
  theme_bw()
time_plot + distance_plot

age_plot<-ggplot(data=most.wins, aes(y=age,x=rate_cycling,fill=birth_country))+
  geom_point(aes(colour=birth_country))+
  geom_smooth(method="lm", se=FALSE, aes(colour=birth_country))+
  labs(x='Rate of Cycling (km/hr)',y='Cyclist Age')+
  theme_bw()+
  ggsave("Week15/Ave_v_Rate.png")
age_plot


 most.wins<-tdf_winners %>%
  filter(stage_wins>3)
View(most.wins)

stagewins<-ggplot(data=most.wins, aes(x=nationality, y=stage_wins))+
  geom_histogram(stat="identity", aes(colour='nationality'))+
  labs(x='Nationality', y='Number of Stages Won')+
  theme_bw()
stagewins

plot<-ggplot(data=most.wins, aes(x=stages_led,y=stage_wins,fill=nationality))+
  geom_point()+
  labs(x='Number of Stages Led',y='Number of Stages Won', fill='Nationality')+
  theme_bw()
plot



