### Tidy Tuesday Week 18, 2021
# Created by Danielle Barnas
# Created on 2021-04-27
######################################################

### Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(here)
library(patchwork)

# clear environment
rm(list=ls())

### Bring in Data ###

tuesdata <- tidytuesdayR::tt_load('2021-04-27')
departures <- tuesdata$departures

# create departure_type dataframe with code type descriptor
departure_type <- tibble(departure_code = as.factor(c(1, 2, 3, 4, 5, 6, 7, 8, 9)),
                         type = as.character(c("Involuntary: CEO Death", "Involuntary: CEO Illness",
                                               "Involuntary: CEO Dismissed for Job Performance",
                                               "Involuntary: CEO Dismissed for Legal Violations or Concerns",
                                               "Voluntary: CEO Retired", "Voluntary: New Opportunity",
                                               "Other", "Missing", "System Error")))

### Data Analysis###

# merge dataframe with code descriptors, remove outlier year, select only relevant columns
departures <- departures %>% 
  mutate(departure_code = factor(departure_code)) %>% 
  left_join(departure_type) %>% 
  select(code = departure_code, type, year_gone = fyear_gone) %>% 
  drop_na() %>% 
  filter(year_gone <= 2021) %>% 
  mutate(code_num = as.numeric(code))

# two dataframees: involuntary leave vs voluntary leave
involuntary <- departures %>% 
  filter(str_detect(type, "Involuntary"))
  
voluntary <- departures %>% 
  filter(str_detect(type, "Voluntary"))

# grab one subset of data using a random year
departure_text<-departures %>% 
  filter(year_gone == 2010) %>% 
  distinct() %>% 
  mutate(Code = code, 'Code Description' = type) %>% 
  arrange(code)


### Plotting ###

# color palettes
pal2<-c("#56135a", "#05878a")
pal4<-c("#dd9933", "#075c56", "#56135a", "#05878a")

p1<-involuntary %>% 
  ggplot(aes(x = year_gone, fill = code)) +
  geom_bar() +
  scale_y_reverse() +
  theme_bw() +
  coord_flip() +
  scale_x_continuous(breaks = scales::pretty_breaks(5), limits = c(1990,2021)) +
  scale_fill_manual(values = pal4) +
  labs(y = "Total Involuntary Departures", fill = "Code") +
  theme(legend.position = "left",
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        panel.grid = element_blank())


p2<-voluntary %>% 
  ggplot(aes(x = year_gone, fill = code)) +
  geom_bar() +
  coord_flip()  +
  theme_bw() +
  scale_x_continuous(breaks = c(1990,2021), limits = c(1990,2021))+
  scale_fill_manual(values = pal2) +
  labs(x = "", y = "Total Voluntary Departures", fill = "Code") +
  theme(axis.ticks = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 13),
        panel.grid = element_blank())



(p1 | p2) / gridExtra::tableGrob(departure_text[1:6,'Code Description']) +
  plot_annotation(
    title = 'Voluntary and Involuntary Departures of CEO Position-Holders \n from 1990 to Present', # can also have a line for subtitle =
    caption = 'CEO listings from S&P 1500 Firms'
    ) +
  plot_layout(widths = c(3,1), heights = unit(c(10, 1), c("cm",'null'))) + # first row will occupy 3cm width and 10cm height, and second row will fill in remaining space
  ggsave(here("2021_Week18","departure_plot.png"), height = 10, width = 14)




