### Tidy Tuesday Week 6, 2022
# Created by Danielle Barnas
# Created on 2022-02-08
######################################################

### Libraries ###

library(tidyverse)
library(tidytuesdayR)
library(here)
library(PNWColors)
library(plotly)
library(ggmap)
library(sp)
library(maps)
library(maptools)

#rm(list=ls())

API<-names(read_table("API.txt"))
register_google(key = API) ### uses my API in separate txt file

### Bring in Data ###
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

glimpse(airmen)

### Process data ###

# create character vector to use for geocode
stateID<-airmen %>%
  distinct(state) %>%
  drop_na() %>%
  as_vector

# geocode looks for lat and lon (with current output) of the state locations
stateIDloc <- geocode(location = stateID, output = "latlon", source = "google")
# bind geocode to stateID
stateID <- stateID %>%
  cbind(stateIDloc) %>%
  rename(state = '.')


# join lat lon to df
air_full <- airmen %>%
  count(state) %>%
  as_tibble() %>%
  right_join(airmen) %>% # rejoin after count()
  left_join(stateID, by = 'state') %>%
  drop_na(state, rank_at_graduation, name) %>% # remove na's from columns of interest
  filter(rank_at_graduation != "N/A") %>%
  mutate(rank_at_graduation = str_replace_all(string = rank_at_graduation,
                                              pattern = "Capt$", # the dollar sign indicates an exact match
                                              replacement = "Captain"))  # replace values for consistency



### Map data ###

# create google map
USmap <- get_map("US", maptype = "watercolor", zoom = 4)

ggmap(USmap) +
  geom_point(data = air_full,
             aes(x = lon,
                 y = lat),
             size = 4,
             color = "#a49393")

## create text column to use with plotly later
# want to show graduate names by graduation rank when I hover over a gps point
air_full <- air_full %>%
  select(-c(pilot_type, aerial_victory_credits, number_of_aerial_victory_credits, reported_lost, reported_lost_date, reported_lost_location, web_profile)) %>%
  separate(graduation_date, into = c('graduation_year','month','day'), sep = "-", remove = T) %>%
  select(-c(month,day))

nstates <- stateID %>% count() %>% as.numeric # total distinct states in dataframe

# create empty dataframe to store information
gradText <- tibble('state' = as.character(NA), # create empty state column
                   'text' = as.character(NA), # create empty new column
                   'rank_at_graduation' = as.character(NA)) # create empty rank at graduation column

# create df of single vector names by state and graduation rank
for (i in 1:nstates) {

  temp<-air_full %>% # create placeholder dataframe with filtered data
    filter(state == stateID[i,1]) %>%
    group_by(rank_at_graduation) %>%
    mutate(text = str_flatten(string = as.vector(name), collapse = "__")) %>%
    ungroup() %>%
    select(state,text,rank_at_graduation) %>%
    distinct()

  gradText <- gradText %>% # add new rows to dataframe
    rbind(temp) %>%
    drop_na()
}

# create df with all names grouped by graduation rank per state
air_text <- gradText %>%
  mutate(text = str_replace_all(string = text, pattern = "__", replacement = "\n ")) %>%  # repalce double underscore with new line
  left_join(stateID) %>%   # add lat and lon columns
  unite(col = rank_text, rank_at_graduation, text, sep= ": \n", remove = T) %>%
  group_by(state) %>%
  mutate(rank_text = str_flatten(string = as.vector(rank_text), collapse = "\n \n")) %>% # bring all rows together to unite full state graduates
  ungroup() %>%
  unite(col = state_rank_text, state, rank_text, sep = "\n \n", remove = T) %>%
  distinct() # remove duplicates

staticMap <- ggmap(USmap) +
  geom_point(data = air_text,
             aes(x = lon,
                 y = lat,
                 text = state_rank_text), # adds hover_text when converted to plotly
             size = 2,
             color = "#67595e")

ggplotly(staticMap, tooltip = "text")



