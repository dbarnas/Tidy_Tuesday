# Week 18 of TIdy Tuesday
# https://github.com/rfordatascience/tidytuesday

# clear working directory
rm(list=ls())

library(tidyverse)

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')

View(grosses)
View(synopses)

autobio<-synopses%>%
  filter(grepl("autobiograph",synopsis))
gross_syn<-inner_join(grosses,autobio,by="show")
gross_syn<-gross_syn%>%
  select(week_number,show,weekly_gross,theatre,synopsis)

gross_syn<-gross_syn%>%
  group_by(show)%>%
  mutate(RelWeekGross=weekly_gross/max(weekly_gross)) # create column for relative weekly gross as a perfentage of the max gross for each show
View(gross_syn)

# Time series plot
# labels and breaks for x-axis text
brks <- gross_syn$week_number[seq(1, length(gross_syn$week_number),53)]

# plot
ggplot(gross_syn, aes(x=week_number,y=RelWeekGross,fill=show)) + 
  geom_line(aes(color=show)) + # adjust the number of columns with ncol=x
  labs(title="Weekly Gross Sales by Week", 
       subtitle="Relative percent difference of gross sales per show", 
       caption="Source: Broadway Theatre", 
       x="Show running week",
       y="Relative Gross % per show", 
       color="Autobiographical Shows") + # title and caption
 # scale_x_date(labels = gross_syn$week_number, breaks = brks) +  # change to weekly ticks and labels
 # scale_color_manual(labels = c("psavert", "uempmed"), values = c("psavert"="#00ba38", "uempmed"="#f8766d")) +  # line color
  guides(guide_legend(ncol=2))+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8))#,  # rotate x axis text
      #  panel.grid.minor = element_blank())  # turn off minor grid


# Let's look at the top grossing theatres

myShows<-grosses%>%
  select(week_number,show,theatre,weekly_gross,avg_ticket_price)
DistinctShows<-myShows%>%
  distinct(show)
myShows<-myShows%>%
  str_subset(show,c("Cats","Les Miserables","Into The Woods","Cabaret","The Phantom of the Opera"))

View(DistinctShows)
View(myShows)

# plot
ggplot(theatreGross, aes(x=week_number,y=avg_ticket_price,fill=show)) + 
  geom_line(aes(color=show)) + # adjust the number of columns with ncol=x
  labs(title="Avg Ticket Price by theatre", 
       subtitle="Relative percent difference of gross sales per show", 
       caption="Source: Broadway Theatre", 
       x="Show running week",
       y="Relative Gross % per show", 
       color="Autobiographical Shows") + # title and caption
  facet_wrap(~theatre)+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8))#,  # rotate x axis text
