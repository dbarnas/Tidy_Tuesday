### Tidy Tuesday Week 7, 2021
# Created by Danielle Barnas
# Created on 2021-02-09
######################################################

### Libraries ###
library(tidyverse)
library(tidytuesdayR)


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

ggplot(data=race_wealth,
       aes(x=year,
           y=wealth_family,
           color=race,
           fill=race)) +
  geom_bar(stat="identity",
           position="dodge",
           color="black") + 
  labs(color="Race",
       x="Year",
       y="Average Family Wealth per Year") + 
  theme_bw()
  









