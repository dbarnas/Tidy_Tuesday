# Week 16 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday
library(tidyverse)
library(ggplot2)
library(lubridate)
library(plyr)
library(scales)
library(zoo)
rm(list=ls())

gdpr_violations <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
#gdpr_text <- read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

# Calendar Heatmap
# Violations by date - do violations or certain violations occur at higher frequencies certain times of the year?
# http://margintale.blogspot.in/2012/04/ggplot2-time-series-heatmaps.html
gdpr_violations$date<-gdpr_violations$date%>%
  parse_date(format="%m/%d/%Y",na=character(),locale=default_locale(),trim_ws = TRUE) # format date
# Create Month Week
gdpr_violations<-gdpr_violations%>% # create new columns extracting portions of the date
  mutate(Year=year(date))%>% # extract the year
  mutate(Month=month(date))%>% # extract the month
  mutate(Week=week(date))%>% # extract the week
  mutate(Monthf=months(date))%>% # extract month name as.character
  mutate(Weekdayf=weekdays(date)) # extract weekday name as.character
gdpr_violations$yearmonth <- as.yearmon(gdpr_violations$date) # create yearmonth column
gdpr_violations$yearmonthf <- factor(gdpr_violations$yearmonth) # create yearmonth column as.factor
gdpr_violations<-gdpr_violations %>%
  mutate(price.div=price/1000)%>% # divided price by $1K euros for simpler figure legend
  filter(Year >= 2018)%>%  # filter read years
  filter(price.div > 0) # filter for actual fines paid

gdpr_violations <- ddply(gdpr_violations,.(yearmonthf), transform, monthweek=1+Week-min(Week))  # compute week number of month
gdpr_violations <- gdpr_violations%>%
  select("Year", "yearmonthf", "Monthf", "Week", "monthweek", "Weekdayf", "price.div","article_violated") # only keep necessary columns
head(gdpr_violations) # sample view of dataframe

#View(gdpr_violations)

# Plot
ggplot(gdpr_violations, aes(monthweek, Weekdayf, fill = price.div)) + 
  geom_tile(colour = "white") + 
  facet_grid(Year~Monthf) + 
  scale_fill_gradient(low="red", high="green", trans="log") +
  labs(x="Week of Month",
       y="",
       title = "Time-Series Calendar Heatmap", 
       subtitle="Violation Fines Through the Year in Europe", 
       fill="Fines divided by 1K Euros")+
  ggsave("Data/Week17/Calendar_Heatmap_Fines.png")

