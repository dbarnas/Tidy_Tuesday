# Week 22 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday

# clear working directory
rm(list=ls())

# load libraries
library(tidyverse)
library(ggplot2)

# Get the Data
cocktails <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
#boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

# unique ingredients to choose what I have in stock
unique_ing<-cocktails%>%
  distinct(ingredient)

# filter ingredients for those I have in stock
myCabinet<-cocktails%>%
  filter(ingredient=="Rum"|ingredient=="Dark rum"|ingredient=="Spiced rum"|ingredient=="White rum"|ingredient=="Light rum"|ingredient=="Vodka"|ingredient=="Kahlua"|ingredient=="Bailys irish cream"|ingredient=="Amaretto"|ingredient=="Ginger beer"|ingredient=="Prosecco"|ingredient=="Wine"|ingredient=="Red wine"|ingredient=="Whiskey"|ingredient=="Ginger Beer"|ingredient=="Dark Rum"|ingredient=="Ouzo"|ingredient=="White Rum"
         |ingredient=="Pineapple juice"|ingredient=="Milk"|ingredient=="Orange juice"|ingredient=="Lemon juice"|ingredient=="Lemon peel"|ingredient=="Ice"|ingredient=="Soda water"|ingredient=="Lemon"|ingredient=="Lemon juice"|ingredient=="Tonic water"|ingredient=="Orange"|ingredient=="Cherry"|ingredient=="Sugar"|ingredient=="Fresh lemon juice"|ingredient=="Water"|ingredient=="Fresh lime juice"|ingredient=="Sugar syrup"|ingredient=="Club soda"|ingredient=="Lime"|ingredient=="Coffee"|ingredient=="Cream"|ingredient=="Ginger"|ingredient=="Tea"|ingredient=="Orange peel"|ingredient=="Allspice"|ingredient=="Cinnamon"|ingredient=="Coriander"|ingredient=="Maraschino cherry"|ingredient=="Berries"|ingredient=="Brown sugar"|ingredient=="Cloves"|ingredient=="Nutmeg"|ingredient=="Carrot"|ingredient=="lemon juice"|ingredient=="Banana"|ingredient=="Honey"|ingredient=="Pineapple Juice"|ingredient=="Lime Juice"|ingredient=="Carbonated water"|ingredient=="Tabasco sauce"|ingredient=="Vanilla extract"|ingredient=="Olive Brine"|ingredient=="Olive"|ingredient=="Pepper"|ingredient=="pineapple juice"|ingredient=="Orange spiral"|ingredient=="Carbonated soft drink"|ingredient=="Butter"|ingredient=="Cumin seed"|ingredient=="Cayenne Pepper"|ingredient=="Black pepper"|ingredient=="Espresso"|ingredient=="Coconut milk"|ingredient=="Lime peel"|ingredient=="Agave syrup"|ingredient=="Maple Syrup")

# count required ingrdients to make drinks for which I have at least one ingredient
needed<-cocktails %>%
  group_by(drink) %>%
  count(drink,name="Needed") %>%
  ungroup()

# count stocked ingredinets, join stocked tally with requried tally for each drink, pivot to long format
ReadyCabinet<-myCabinet %>%
  group_by(drink) %>%
  count(drink,name="Stocked") %>%
  ungroup() %>%
  left_join(needed, by="drink") %>%
  filter(grepl("Beach",drink)|grepl("Shark",drink)|grepl("Fish",drink)|grepl("Lagoon",drink)|grepl("Snapper",drink)|grepl("Sea",drink)|grepl("Skipper",drink)|grepl("Florida",drink)|grepl("Island",drink)|grepl("Summertime",drink)|grepl("Sunrise",drink)|grepl("Sun",drink)|grepl("Sunny",drink)|grepl("Aloha",drink)|grepl("Hawaiian",drink)|grepl("Jamaica",drink)|grepl("Jamaican",drink)) %>% # filter for drinks that make me feel like I'm by the sea
  mutate(Unstocked=Needed-Stocked) %>% #create column for number of missing ingredients to make drink
  select(-c(Needed)) %>%
  pivot_longer(cols=c(Unstocked,Stocked),names_to = "Availability",values_to = "Number_ingredients")
View(ReadyCabinet)

# create horizontal stacked bar plot
plot<-ggplot(data=ReadyCabinet, aes(x=drink, y=Number_ingredients, fill=Availability))+
  geom_area(aes(group=Availability,color=Availability))+
  labs(y="Number of Drink Ingredients",x="Island Holiday Drinks")+
  coord_flip()+
  ggtitle("Quarantine Island Holiday Drinks")
# view plot
plot
