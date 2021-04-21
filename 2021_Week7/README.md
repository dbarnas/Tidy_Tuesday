# Week 7, 2021 Static plotly figure

## Code

### Libraries  
library(tidyverse)  
library(tidytuesdayR)  
library(here)  
library(plotly)  
library(scales)  
library(htmlwidgets)  

rm(list=ls())  

### Bring in Data 
tuesdata <- tt_load(2021, week = 7)  

race_wealth <- tuesdata$race_wealth  


### Data Analysis 
glimpse(race_wealth)  


race_wealth %>%   
  plot_ly(x=~year, y=~wealth_family,  
          color=~race, fill=~race,  
          type = "bar",  
          text = ~paste0("Annual Family Income: $", # customize hover label text  
                        comma(round(wealth_family),decimals = 2)),  
          hovertemplate = paste('%{text}')) %>%   
  layout(title = "Annual Family Income", #change plot title  
         xaxis = list(title = "Year"), #change x-axis title  
         yaxis = list(title = "Annual Income (USD)")) #change y-axis title  


![Wealth and Income Figure](https://github.com/dbarnas/Tidy_Tuesday/blob/master/2021_Week7/Output/wealth_and_income_plotly_static.png)


