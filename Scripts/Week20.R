# Week 20 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday

# clear working directory
rm(list=ls())

# load libraries
library(tidyverse)
library(ggmap)
library(jcolors)

# bring in data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')

# filter for underwater volcanoes
seacano<-volcano%>%
  filter(elevation<0)

# preparing map
myLocation=c(-179.9,-70.0,179.9,70.0) #left,bottom,right,top bounding box
myMap<-get_map(location=myLocation,
               source="stamen",maptype = "watercolor",color="bw", #map type
               force=TRUE) #search anew for map images every time this is run

lon=seacano$longitude # use lat and long data from df
lat=seacano$latitude
Rock.Types<-seacano$major_rock_1 # my map fill

# mapping major rock types of underwater volcanoes
ggmap(myMap,extent='panel',fill=Rock.Types)+
  geom_point(data=seacano,aes(x = lon, y = lat, colour=Rock.Types),alpha=1) + # alpha=transparency
  scale_color_jcolors(palette="pal6") + # color palette
  labs(x = 'Longitude', y = 'Latitude') +
  ggtitle("Major Rock Types of Underwater Volcanoes")+
ggsave("Data/Week20/VolcanoMap.png")


