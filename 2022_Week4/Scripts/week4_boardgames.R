### Tidy Tuesday Week 4 Jan 25, 2022
# Created by Danielle Barnas
# Created on 2022-01-25
######################################################

### Libraries ###
library(tidyverse)
library(tidytuesdayR)
library(here)
library(patchwork)
library(plotly)

# clear environment
rm(list=ls())

### Bring in Data ###

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
ratings <- tuesdata$ratings
details <- tuesdata$details


# Filter top 100 ranked games
# ratings<-ratings %>%
#   filter(rank <= 100) %>%
#   arrange(rank)

# Average play time by the year published
playdetails <- details %>%
  select(primary,yearpublished,playingtime,minplaytime,maxplaytime) %>%
  mutate(summary.playtime = ifelse(playingtime < 30, "< 30", playingtime)) %>%
  mutate(summary.playtime = ifelse(playingtime >= 30 & playingtime <= 60, "30-60", summary.playtime)) %>%
  mutate(summary.playtime = ifelse(playingtime > 60 & playingtime <= 90, "61-90", summary.playtime)) %>%
  mutate(summary.playtime = ifelse(playingtime > 90 & playingtime <= 120, "91-120", summary.playtime)) %>%
  mutate(summary.playtime = ifelse(playingtime > 120, "> 120", summary.playtime))
# set play time as factors in order
playdetails$summary.playtime <- playdetails$summary.playtime %>% factor(c("< 30", "30-60", "61-90", "91-120", "> 120"))



# total games tallied by expected play time from 1960 to 2023
# playdetails %>%
#   filter(yearpublished>1960) %>%
#   ggplot(aes(x = yearpublished)) +
#   geom_bar(aes(fill = summary.playtime, color = summary.playtime)) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   scale_x_continuous(breaks = seq(1960,2023, by = 10)) +
#   scale_color_manual(values = mypalette) +
#   scale_fill_manual(values = mypalette) +
#   labs(x = "Launch Year",
#        y = "Total Tallied Games",
#        fill = "Game Play Time",
#        color = "Game Play Time")
#
# sumUR<-sum(ratings$users_rated) # total ratings by users
#
# playdetails %>%
#   rename(name = primary,
#          year = yearpublished) %>%
#   left_join(ratings) %>%
#   select(name, year, playingtime, summary.playtime, rank, users_rated) %>%
#   mutate(user_percent = users_rated/sumUR*100) %>%
#   filter(rank <= 5)

### make a plotly showing 1990 to 2022: total games published each year and best ranked game of the year

# join details with rank df
rankdetails <- playdetails %>%
  rename(name = primary,
         year = yearpublished) %>%
  left_join(ratings) %>%
  select(name, year, playingtime, summary.playtime, rank, users_rated) %>%
  filter(year >=1990 & year < 2023)

# count total published games per year
yearN <- rankdetails %>%
  group_by(year) %>%
  count(year) %>%
  rename(totalannual = n)
rankdetails<-rankdetails %>% # join count with rank df
  left_join(yearN) %>%
  rename(gamerank = rank) %>%
  group_by(year) %>%
  slice(which.min(gamerank)) # only keep the best ranked game from each year


# ggplot(rankdetails,
#        aes(y = totalannual,
#            x = year)) +
#   geom_col() +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank()) +
#   scale_x_continuous(breaks = seq(1990,2022, by = 5)) + # jumps by 5 years
#   labs(x = "Launch Year",
#        y = "Total Annual Games Launched") +
#   coord_flip()


### Plotly Graph


plot_ly(
  data = rankdetails,

  x = ~year,

  y = ~totalannual,

  type = "bar",

  text = ~paste(name,"\n", gamerank), # board game name displayed in hover caption, as well as ranking

  marker = list(color = "#647C90"), # color of bars and hover box

  hovertemplate = paste("Best Game of the Year:\n", # text within hover box
                        "<b>%{text}</b>", # emboldens game name
                        "<i>Current Rank</i>",
                        "<extra></extra>"), # removes "trace 0" extra hover info

  textposition = 'none', # name along the bar. 'auto' displays on bar, 'none' removes

  showlegend = FALSE) %>%

  add_markers() %>%

  layout(xaxis = list(title = "<b>Release Year<b>",
                      color = '#ffffff'),
         yaxis = list(title = "<b>Total Annual Game Releases<b>",
                      showgrid = FALSE,
                      color = '#ffffff'),
         title = "<b>Total Game Releases since 1990</b> \n and the Top Rated Game of the Year",
         titlefont = list(color = '#ffffff'),
         paper_bgcolor = '#4E4F50', # change background color
         plot_bgcolor = '#4E4F50',

         images = list( # add static image of most highly ranked game
           source = base64enc::dataURI(file = "2022_week4/Gloomhaven.png"),
           x = 0.1, y = 0.6,
           sizex = 0.3, sizey = 0.3,
           xref = "paper", yref = "paper",
           xanchor = "left", yanchor = "bottom"),

         shapes = list(list(type = "rect",line = list(color = "white"),

                            x0 = 1991.5, x1 = 2002.5, # puts white box around image
                            y0 = 845, y1 = 1180)),

         annotations = list(x = rankdetails$year[28], # only annotate arrow for the highest rated game point
                            y = rankdetails$totalannual[28],
                            text = "",
                            xref = "x",
                            yref = "y",
                            showarrow = TRUE,
                            arrowcolor = "#ffffff",
                            ax = -255, # sets coordinate of arrow tail about the arrow head and lengthens tail
                            ay = 60
                            ))




