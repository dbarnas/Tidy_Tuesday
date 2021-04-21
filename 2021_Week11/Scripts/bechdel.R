### Tidy Tuesday Week 11, 2021
# Created by Danielle Barnas
# Created on 2021-03-10
######################################################

### Libraries ###

library(tidyverse)
library(tidytuesdayR)
library(here)
library(patchwork)

rm(list=ls())

### Bring in Data ###

tuesdata <- tidytuesdayR::tt_load(2021, week = 11)

raw_bechdel<-tuesdata$raw_bechdel
View(raw_bechdel)

movies<-tuesdata$movies
View(movies)

### Data Analysis ###

# count total movies evaluated each year
totals<-raw_bechdel %>% 
  group_by(year) %>% 
  count(name = "counts")
# mean rating each year
mean_rating<-raw_bechdel %>% 
  group_by(year) %>%
  summarise(mean_rating = mean(rating)) %>% 
  left_join(totals)

# bechdel rating counts by year
bechdel_counts<-raw_bechdel %>% 
  group_by(year, rating) %>% 
  count(name = "counts") %>% 
  filter(year < 2021)

# join files together and select files
bechdel<-right_join(raw_bechdel, movies) %>% 
  select(year, title, rating, binary, budget, rated, genre) %>% 
  drop_na()
View(bechdel)



View(mean_rating)

### Plotting ###

# Plotting budget grouped by rating
violin_plot<-ggplot(bechdel,
                    aes(x = year,
                        y = budget)) +
  geom_violin(fill = "#7E499C") +
  theme_bw() +
  labs(y = "Movie Budget (USD)",
       x = "",
       title = "Movie budgets since 1970") +
  scale_y_continuous(labels = scales::comma)

# total evaluated movies over the years grouped by score
count_plot<-ggplot(bechdel_counts,
       aes(x = year,
           y = counts,
           color = as.factor(rating))) +
  geom_line() +
  labs(title = "Bechdel scores since 1888") +
  theme_bw() +
  labs(x = "",
       y = "Movie Count",
       color = "Bechdel Score")

# Mean bechdel scores by year
rating_plot<-ggplot(mean_rating,
                    aes(x = year,
                        y = mean_rating)) +
  geom_line() +
  theme_bw() +
  labs(title = "Mean Bechdel score trend since 1988",
       x = "Release Year",
       y = "Mean Bechdel Score")

# Stitch plots together
violin_plot + (rating_plot / count_plot) +
  ggsave(here("2021_Week11","Output","bechdel_plot.pdf"), width = 13, height = 10)



