### Tidy Tuesday Week 7, 2021
# Created by Danielle Barnas
# Created on 2021-02-09
######################################################

### Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(here)
library(plotly)
library(scales)
library(htmlwidgets)

rm(list=ls())

### Bring in Data ###
tuesdata <- tt_load(2021, week = 7)

lifetime_earn <- tuesdata$lifetime_earn
student_debt <- tuesdata$student_debt
retirement <- tuesdata$retirement
home_owner <- tuesdata$home_owner
race_wealth <- tuesdata$race_wealth
income_time <- tuesdata$income_time
income_limits <- tuesdata$income_limits
income_aggregate <- tuesdata$income_aggregate
income_distribution <- tuesdata$income_distribution
income_mean <- tuesdata$income_mean


### Data Analysis ###
glimpse(race_wealth)
View(race_wealth)

race_wealth %>% 
  ggplot(aes(x=year, y=wealth_family,
             color=race, fill=race)) +
  geom_bar(stat="identity", position="dodge", aes(color = race)) + 
  labs(color="Race", fill = "Race",
       x="Year", y="Average Family Wealth per Year") + 
  theme_bw()


p1<-race_wealth %>% 
  plot_ly(x=~year, y=~wealth_family,
          color=~race, fill=~race,
          type = "bar",
          text = ~paste0("Annual Family Income: $", # customize hover label text
                        comma(round(wealth_family),decimals = 2)),
          hovertemplate = paste('%{text}')) %>% 
  layout(title = "Annual Family Income", #change plot title
         xaxis = list(title = "Year"), #change x-axis title
         yaxis = list(title = "Annual Income (USD)")) #change y-axis title





