### Tidy Tuesday Week 15, 2021
# Created by Danielle Barnas
# Created on 2021-04-06
######################################################

### Libraries ###

library(tidyverse)
library(tidytuesdayR)
library(here)
library(patchwork)
library(scales)
library(ggrepel)
library(PNWColors)

rm(list=ls())

### Bring in Data ###

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

brazil <- tuesdata$brazil_loss
#View(brazil)

soybean <- tuesdata$soybean_use
#View(soybean)

vegoil <- tuesdata$vegetable_oil
#View(vegoil)

forest_area <- tuesdata$forest_area
#View(forest_area) # percentage of global forest area

### Data Analysis ###

## Vegatable Oil production
vegoil$production[which(is.na(vegoil$production))] <- 0 # replace NA's with 0
veg <- vegoil %>% 
  group_by(entity, year) %>% 
  summarise(total.oil.tonnes = sum(production))
#View(veg)

## Forest area % over time
forest <- forest_area %>% 
  rename(area_percent = forest_area) %>% 
  select(-code)
#View(forest)

## Join dataframes
area_oil <- full_join(veg, forest) %>% 
  drop_na() %>% 
  filter(entity != 'World') %>% 
  group_by(year) %>% 
  mutate(percentage = total.oil.tonnes / sum(total.oil.tonnes) * 100) 

# Identify highest producing countries
area_oil %>%
  filter(year == 1990,
         percentage > 4) %>% 
  select(entity)

area_oil <- area_oil %>% 
  filter(entity == 'Asia' | entity == 'Eastern Asia' | 
         entity == 'Europe' | entity == 'Northern America' | 
        entity == 'South America' | entity == 'United States')

# set palette
pal <- pnw_palette("Cascades")

# greatest oil production countries: line plot
p1<-area_oil %>% 
  ggplot(aes(x = year, y = total.oil.tonnes, colour = entity)) +
  geom_line(size = 1) +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = unit(c(1,6,1,1), "cm"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  geom_label(data = subset(area_oil, year == max(year)), 
            aes(label = paste0(entity, ": ", comma(total.oil.tonnes)), #alpha = 0.9,
                fill = factor(entity)), 
            colour = c("lavender","black","lavender","lavender","black","lavender"), 
            fontface = "bold",
            hjust = c(-0.26,-0.17,-0.22,-0.15,-0.16,-0.17), 
            vjust = c(0,0,0,0.4,0.2,0.7)) +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Total Vegetable Oil Production (Tonnes)",
       title = paste0("Top Producers of Vegetable Oil","\n with Change in % Forested Area Cover")) +
  coord_cartesian(clip = 'off') +
  scale_colour_manual(values = pal[c(1,3,2,5,4,6)]) +
  scale_fill_manual(values = pal[c(1,3,2,5,4,6)]) +
  ggsave(here("2021_Week15","Output","Figure1.pdf"), width = 10.5, height = 7.2)
p1

# bar plot
p2<-area_oil %>% 
  ggplot(aes(x = year, y = area_percent, color = fct_reorder(entity, entity, .desc = F), 
             fill = fct_reorder(entity, entity, .desc = F))) +
  geom_bar(stat = "identity") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  theme_bw() +
  labs(x = "Year", y = "Percent of Global Forested Area",
       title = "Total Global Forested Area",
       color = "Country", fill = "Country") +
  scale_colour_manual(values = pal[c(1,3,2,5,4,6)]) +
  scale_fill_manual(values = pal[c(1,3,2,5,4,6)]) +
  ggsave(here("2021_Week15","Output","Figure2.pdf"), width = 7, height = 7)
p2

p3<-area_oil %>% 
  ggplot(aes(x = year, y = area_percent, color = fct_reorder(entity, entity, .desc = F), 
             fill = fct_reorder(entity, entity, .desc = F))) +
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "", y = "% Forested Area",
       color = "Country", fill = "Country") +
  scale_x_continuous(breaks = pretty_breaks(3)) +
  scale_colour_manual(values = pal[c(1,3,2,5,4,6)]) +
  scale_fill_manual(values = pal[c(1,3,2,5,4,6)])

p1 + annotation_custom(ggplotGrob(p3), xmin = 1989, xmax = 2003, 
                       ymin = 57000000, ymax = 98000000) +
  ggsave(here("2021_Week15","Output","Figure3.png"), width = 7, height = 7)




