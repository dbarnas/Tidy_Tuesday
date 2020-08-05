# Week 14 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-03-31/readme.md

library(tidyverse)
library(ggplot2)

# Get Data
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')
# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE tidytuesdayR version ? from GitHub
# Either ISO-8601 date or year/week works!
# Install via devtools::install_github("thebioengineer/tidytuesdayR")
tuesdata <- tidytuesdayR::tt_load('2020-03-31')
tuesdata <- tidytuesdayR::tt_load(2020, week = 14)
brewing_materials <- tuesdata$brewing_materials

View(brewing_materials)
View(beer_states)
View(beer_taxed)
View(brewer_size)

basic_beer_plot<- ggplot(data=beer_states, mapping=aes(x=state,y=year, size=barrels)) +
  geom_point() + 
  ggsave(path='Week14/Output',filename='state_barrel_size_by_year.png',device=png)

basic_beer_plot

barrel_data<-beer_states %>%
  group_by(year) %>%
  mutate(relative_barrels=barrels/sum(barrels))
View(barrel_data)
