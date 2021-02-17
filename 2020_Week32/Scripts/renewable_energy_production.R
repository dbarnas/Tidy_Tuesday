# Week 32

# Load packages
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(treemapify)
rm(list=ls())

# Load data
tuesdata <- tidytuesdayR::tt_load(2020, week = 32)
energy_types <- tuesdata$energy_types

energy_types<-energy_types%>%
  filter(level=="Level 1")%>%
  select(-c(level,country_name))%>%
  pivot_longer(cols=c('2016','2017','2018'),names_to = 'Year',values_to = 'Energy')

# Plotting a treemap
treeMapPlot <- ggplot(energy_types, aes(area = Energy, fill = type, label=type, subgroup=country)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "black", size = 3) +
  geom_treemap_subgroup_text(place = "centre", grow = T, alpha = 0.3, colour = "black",  #grow: if True, text will grow or shrink to fill the box
                             fontface = "italic", min.size = 0) +
  geom_treemap_text(colour = "white", place = "topleft", reflow = T)+ #reflow: if True, text will be wrapped to better fit the box
  scale_fill_brewer(palette = "Paired") +
  facet_wrap(ncol=2,~Year) +
  labs(title = "Annual Net Energy Production",subtitle = "Subcategorized by country",fill="Energy Type") +
  ggsave("images/week32_energytypes.png", width = 10, height = 7)
treeMapPlot

# resources: https://cran.r-project.org/web/packages/treemapify/vignettes/introduction-to-treemapify.html