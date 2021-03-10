### Tidy Tuesday Week 9, 2021
# Created by Danielle Barnas
# Created on 2021-02-23
######################################################

### Libraries ###

library(tidyverse)
library(tidytuesdayR)
library(here)
library(PNWColors)
library(patchwork)
library(lubridate)

rm(list=ls())

### Bring in Data ###

tuesdata <- tidytuesdayR::tt_load('2021-02-23')

earn<-tuesdata$earn
employed<-tuesdata$employed

View(earn)
View(employed)

### Data Analysis ###

# filter data and drop na's
employed_tidy<- employed %>% 
  filter(industry!="Women"& # remove race and gender from Industry column
           industry!="White"&
           industry!="Black or African American"&
           industry!="Asian") %>% 
  filter(race_gender!="TOTAL") %>% # remove TOTAL from race_gender column
  drop_na(industry) # remove NA's from the industry column

# subset only management positions and summarize employment between men and women
manage <- employed_tidy[str_which(employed_tidy$minor_occupation, pattern = "Manage"),] %>%  # select management positions
  filter(race_gender == "Men" | race_gender == "Women") %>% # filter out only men and women
  select(-industry_total) %>% # remove unnecessary column
  group_by(industry,year) %>% 
  mutate(management_total = sum(employ_n)) %>% # sum total employees in management by year and industry
  mutate(percentage = employ_n / management_total * 100) # percent men vs women in each management role

# resolve issues with "Mining..." discrepancies between years
manage_fix<-manage[str_which(manage$industry, pattern = "Mining"),] %>% # extract Mining industry rows
  mutate(industry = "Mining, quarrying, and oil and gas extraction") # relabel extracted rows uniformly
manage_nofix<-manage[-str_which(manage$industry, pattern = "Mining"),] # extract all other rows but Mining

manage<-rbind(manage_fix,manage_nofix) %>%  # bind rows from new Mining and non-mining industries
  arrange(desc(year),industry) # arrange by year and industry

# summarize earnings for men and women age 25 and older
earn_tidy<-earn %>% 
  filter(age == "25 years and over") %>% # filter out age group
  filter(sex == "Men" | sex == "Women") %>% 
  filter(race != "All Races") %>% 
  group_by(sex,race,year) %>% 
  summarize(monthly_earn = sum(median_weekly_earn))  # take the mean of annual weekly earnings by sex, race, and year

earn_tidy$year<-year(earn_tidy$year) # parse to date type

### Plotting ###

# set color palette
pal <- pnw_palette("Shuksan2",2)

# management percentage plot
p1<-ggplot(data = manage,
       aes(x = percentage,
           y = industry,
           colour = race_gender,
           fill = race_gender)) +
  geom_col() +
  labs(color = "Gender",
       fill = "Gender",
       x = "Percentage Employed",
       y = "Employment Industry",
       title = "Management positions held by men and women") +
  # scale_fill_manual(values = pal) +
  # scale_color_manual(values = pal) +
  theme_bw() + 
  facet_wrap(~year, nrow = 1)
  #ggsave("2021_Week9/Output/employment.pdf", width = 15, height = 15)

# earnings plot
p2<-ggplot(earn_tidy,
       aes(y = monthly_earn,
           x = year,
           colour = sex)) + 
  geom_line() +
  geom_smooth(method = "lm") +
  labs(color = "Gender",
       fill = "Gender",
       x = "Year",
       y = "Median Monthly Earnings (USD)",
       title = "Median monthly income") +
  theme_bw() +
  facet_wrap(~race, nrow = 1)

p1 / p2



