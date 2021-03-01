### Tidy Tuesday Week 9, 2021
# Created by Danielle Barnas
# Created on 2021-02-23
######################################################

### Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(here)
library(PNWColors)

### Bring in Data ###
tuesdata <- tidytuesdayR::tt_load('2021-02-23')

earn<-tuesdata$earn
employed<-tuesdata$employed

View(earn)
View(employed)

### Data Analysis ###

# View unique categories w/n columns
categories<-employed %>% 
  distinct(industry)

# filter data and drop na's
employed_tidy<- employed %>% 
  filter(industry!="Women"& # remove race and gender from Industry column
           industry!="White"&
           industry!="Black or African American"&
           industry!="Asian") %>% 
  filter(race_gender!="TOTAL") %>% # remove TOTAL from race_gender column
  drop_na(industry) # remove NA's from the industry column

# view mining, quarrying, and oil and gas extraction
subset<- employed_tidy %>% 
  filter(industry != "Mining, quarrying, and oil and gas extraction")
subset2<- employed_tidy %>% 
  filter(industry == "Mining, quarrying, and oil and gas extraction")
# there's a labeling issue I can't see in the spreadsheet

midyears<-employed_tidy %>% 
  filter(year == "2017" | year == "2018") %>% 
  stringr::str_subset(string = employed_tidy$inudstry, pattern = "Mining")

# set color palette
pal <- pnw_palette("Shuksan2")

ggplot(data = employed_tidy,
       aes(x = employ_n,
           y = industry,
           colour = race_gender,
           fill = race_gender)) +
  geom_col(position = "stack") +
  facet_wrap(~ year) +
  labs(color = "Race and Gender",
       fill = "Race and Gender",
       x = "Total Employed",
       y = "Employment Industry") +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  theme_bw() + 
  ggsave("2021_Week9/Output/employment.pdf", width = 15, height = 15)






