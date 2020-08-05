# Week 29

library(tidytuesdayR)
library(tidyverse)
library(magick)
library(ggplot2)
library(ggimage)
library(jcolors)

#load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 29)
astronauts <- tuesdata$astronauts
View(astronauts)

#select data for plotting
mydata<- astronauts%>%
  select(sex,nationality,military_civilian,year_of_mission,total_number_of_missions,occupation,mission_title,hours_mission)
fem<-mydata%>%
  filter(sex=="female")%>%
  group_by(year_of_mission,nationality)%>%
  count()
totalast<-mydata%>%
  group_by(year_of_mission,nationality)%>%
  count()

#plot
plot<-ggplot()+
  geom_bar(data=totalast,stat="identity",aes(x=year_of_mission,y=n, fill=nationality),position = "stack")+ 
  geom_line(data=fem,aes(x=year_of_mission,y=n),size=1.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "white"))+
  scale_color_jcolors(palette="pal12") + # color palette
  xlab(color="white")
  labs(y="Total Astronauts", 
       x="Year of Mission", 
       title="Astronauts Involved in Space Launches",
       subtitle = "Number of Women Overlain")
ggbackground(plot,"images/nightsky.png")
