# Week 16 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday
library(tidyverse)
library(ggplot2)
rm(list=ls())

polls <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')
View(polls)
View(rankings)
# merge dataframes by song title and remove redundant columns
rank_polls<-polls %>%
  left_join(rankings, by="title") %>%
  select(-c(critic_country2,ID,artist.y,year.y,gender.y)) %>%
  rename(artist=artist.x,gender=gender.x,year=year.x)
View(rank_polls)

# select for rank=1
top_artist<-polls %>%
  filter(rank==1)
# count number of votes for each gender by country
women<-top_artist %>%
  group_by(critic_country, gender) %>%
  count()

boo_men<-rank_polls %>%
  group_by(critic_country, gender) %>%
  count()
# need to add in 0 value data
View(boo_men)

# histogram showing rank=1 sums for each gender by country
gender_hist<-ggplot(data=women, aes(x=critic_country, y=n, fill=gender))+
  geom_histogram(stat="identity", position='dodge')+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x="Critic Country", y="Total Number of #1 Artists", fill="Gender")+
  theme_bw()+
  ggsave("Week16/Top_Ranking_Artist_Votes.png")
gender_hist

# histogram showing votes sums for each gender by country
boo_men_hist<-ggplot(data=boo_men, aes(x=critic_country, y=n, fill=gender))+
  geom_histogram(stat="identity", position='dodge')+
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)+
  labs(x="Critic Country", y="Total Votes", fill="Gender")+
  theme_bw()+
  ggsave("Week16/Critic_Votes.png")
boo_men_hist
