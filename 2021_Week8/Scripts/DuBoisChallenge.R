### Tidy Tuesday Week 8, 2021
# Created by Danielle Barnas
# Created on 2021-02-16
######################################################

### Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(PNWColors)

### Bring in Data ###
tuesdata <- tidytuesdayR::tt_load('2021-02-16')

census<-tuesdata$census
View(census)

### Data Analysis ###

# pivot longer
long_census <- census %>%
  pivot_longer(cols = c(total, white, black, black_free, black_slaves), # columns to grab data from
               names_to = "demographic", # column containing chosen column headings
               values_to = "census") # column containing the census counts from each column heading

# set demographic to factor type
long_census$demographic <- factor(long_census$demographic, 
                                  levels = c("total", "white", "black", "black_slaves","black_free")) # reorder levels

# filter out USA total
long_census <- long_census %>%
  filter(region == "USA Total")

# prepare color palette
pal <- pnw_palette("Starfish")[c(3,7,1,4,5)] # specifying which and order of colors form palette

# stacked area graph
ggplot(data = long_census,
       aes(x = year,
           y = census,
           fill = demographic)) + 
  geom_line(color = "black") + # outline the area plots in black
  geom_area(position = "identity") + # overlays area plots, instead of stacked
  labs(fill = "Demographic", # rename legend
       title = "USA Census of the 1800's",
       x = "Year", # relabel axes
       y = "Population Totals") +
  theme(axis.title = element_text(size = 20), # change text size for both axes
        legend.position = c(0, 1)) + # values should be between 0 and 1. c(0,0) corresponds to the “bottom left” and c(1,1) corresponds to the “top right” position.)
  scale_fill_manual(values = pal,  # set color palette
                    labels=c("Total","White","Black","Black, Enslaved","Black, Free")) + # rename legend items
  geom_label(data = subset(long_census, year > 1860),
             aes(label = census),
             show.legend = F,
             size = 5,
             hjust = c(1.2, 1.2, 1.2, 1.2, 2),
             vjust = c(0.5, 0, 0, -1.2, -0.5)) +
  theme_bw() # black-white theme




