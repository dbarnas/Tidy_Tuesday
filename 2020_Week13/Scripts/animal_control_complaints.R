# Week 30

library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(treemapify)
rm(list=ls())


#load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 30)
animal_outcomes <- tuesdata$animal_outcomes
animal_complaints <- tuesdata$animal_complaints
brisbane_complaints <- tuesdata$brisbane_complaints

complaints <- animal_complaints %>%
  rename(animal_type='Animal Type',date='Date Received',complaint_type='Complaint Type') %>%
  group_by(animal_type,date) %>%
  count(complaint_type) %>%
  ungroup()
View(complaints)

# plot

treeMapPlot <- ggplot(complaints, layout = "fixed", aes(area = n, fill = animal_type, subgroup = complaint_type, group = animal_type, label=complaint_type)) +
  geom_treemap() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Accent")

treeMapPlot

############################
# alternatively
outcomes <- animal_outcomes %>%
  select(animal_type,year,outcome,Total)
outcomes
treeMapPlot <- ggplot(outcomes, index=c("subgroup","subgroup2"), fontcolor.labels=c("white","white"), border.col=c("black","white"), layout = "fixed", aes(area = Total, fill = animal_type, subgroup = outcome, subgroup2 = animal_type)) +
  geom_treemap() +
  geom_treemap_subgroup_text() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup2_border() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_brewer(palette = "Accent")

treeMapPlot
