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
  ggsave(here("2021_Week15","Output","Figure3.pdf"), width = 7, height = 7)







# observe change in forest area
area_oil %>% 
  ggplot(aes(x = year, y = area_percent, color = entity)) +
  geom_line() +
  theme(legend.position = "none") +
  geom_text(data = subset(area_oil, year == max(year)), aes(label = entity))

area10up <- area_oil %>% 
  filter(area_percent > 10,
         #entity != 'Americas', # North and South America are represented, so remove 'Americas'
         #entity != 'Asia', # skewing plot 
         entity == 'Net Food Importing Developing Countries' |
           entity == 'Least Developed Countries') 

coef <- 0.000005
## double y axis
area10up %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = total.oil.tonnes, color = entity)) +
  geom_line(aes(y = area_percent/coef, color = entity), linetype = 2) +
  labs(x = "Year") +
  scale_y_continuous(name = "Vegetable Oil (Tonnes)", # Features of the first axis
                     sec.axis = sec_axis(trans = ~.*coef, name="% Global Forest Area"), # Add a second axis and specify its features
                     labels = comma,
                     limits = c(0,10000000)) +
  theme(legend.position = "none") +
  geom_text(data = subset(area10up, year == max(year)), aes(y = total.oil.tonnes, label = entity), hjust = 0.95, vjust = 0.2) +
  geom_text(data = subset(area10up, year == min(year)), aes(y = area_percent/coef, label = entity), hjust = 0.09, vjust = c(1.5,-1.3)) +
  coord_cartesian(clip = 'on')


## Soybean production
soy <- soybean %>% 
  drop_na() %>% 
  pivot_longer(cols = c(human_food,animal_feed,processed), 
               names_to = "Types", values_to = "Values")

### observe countries with greatest growth
# Argentina, Brazil, China, United States

# Only countries with greatest growth
soy <- soy %>% 
  filter(Types == 'animal_feed') %>% 
  filter(Values > 0)
  #filter(entity == 'World' | entity == 'Argentina' | entity == 'Brazil' | 
           #entity == 'China' | entity == 'United States')

soy_percent <- soy %>% 
  filter(entity != 'World',
         year == 2013) %>%
  mutate(percent = Values / sum(Values) * 100) %>% 
  filter(percent > 0.5)
  


soy.value <- soy %>% 
  group_by(entity) %>% 
  filter(Values == max(Values))

soy.label<-tibble(year = 2013,
                  Values = soy.value$Values,
                  entity = soy.value$entity)

ggplot(soy, aes(x = year, y = Values, colour = entity)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = unit(c(1,3,1,1), "cm")) +
  scale_x_discrete(limits = c(1960:2015), breaks = scales::pretty_breaks(5), expand = c(0, 2)) +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Soybean Harvested (Tonnes)",
       title = "Soybean Harvested for Animal Feed",
       subtitle = "Top four counties with greatest growth in harvesting") +
  geom_text(data = soy.label, aes(label = entity), hjust = c(-0.27, -0.45, -0.42, -0.19, -0.42), 
            vjust = c(1.5,0,0,0,0), stat = "identity", position = "identity") +
  coord_cartesian(clip = 'off') # Allow labels to bleed past the canvas boundaries
  


ggplot(soy, aes(x = year, y = Values)) +
  geom_bar(data = subset(soy, entity != 'World'), stat = "identity", aes(color = entity, fill = entity), alpha = 0.3) +
  geom_line(data = subset(soy, entity == 'World'), size = 1.5) +
  theme_bw() +
  theme(plot.margin = unit(c(1,3,1,1), "cm")) + #legend.position = "none",
  scale_x_discrete(limits = c(1960:2015), breaks = scales::pretty_breaks(5), expand = c(0, 2)) +
  scale_y_continuous(labels = comma) +
  labs(x = "Year", y = "Soybean Harvested (Tonnes)",
       title = "Soybean Harvested for Animal Feed",
       subtitle = "Top four counties with greatest growth in harvesting") +
  geom_text(data = subset(soy.label, entity == 'World'), aes(label = 'World'), 
            stat = "identity", position = "identity", hjust = 1) +
  coord_cartesian(clip = 'off') # Allow labels to bleed past the canvas boundaries






