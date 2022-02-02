### Tidy Tuesday Week 15, 2022
# Created by Danielle Barnas
# Created on 2022-02-01
######################################################

### Libraries ###

library(tidyverse)
library(tidytuesdayR)
library(here)
library(patchwork)
library(scales)
library(ggrepel)
library(PNWColors)
library(hrbrthemes)
library(plotly)


rm(list=ls())

### Bring in Data ###

breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
#trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
#breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')



### Process for Heatmap ###

breed_traits <- breed_traits %>%
  # Only keep numeric variables
  select(-c('Coat Type',
            'Coat Length')) %>%
  pivot_longer(cols = 2:15, names_to = 'RankType', values_to = 'Ranking')


## Subset dogs by groups

retriever <- breed_traits %>%
  filter(grepl("Retriever",Breed)) %>%
  mutate(set = 'Retrievers') %>%
  mutate(text = paste0("Breed: ",Breed, "\n",RankType,": ", Ranking))
terrier <- breed_traits %>%
  filter(grepl("Terrier",Breed)) %>%
  mutate(set = 'Terriers') %>%
  mutate(text = paste0("Breed: ",Breed, "\n",RankType,": ", Ranking))
hound <- breed_traits %>%
  filter(grepl("hound",Breed)|grepl("Hound",Breed)|grepl("hund",Breed)) %>%
  mutate(set = 'Hounds') %>%
  mutate(text = paste0("Breed: ",Breed, "\n",RankType,": ", Ranking))
shepherd <- breed_traits %>%
  filter(grepl("heep",Breed)|grepl("hepherd",Breed)|grepl("Cattle",Breed)) %>%
  mutate(set = 'Shepherds') %>%
  mutate(text = paste0("Breed: ",Breed, "\n",RankType,": ", Ranking))
spaniel <- breed_traits %>%
  filter(grepl("Spaniel",Breed)) %>%
  mutate(set = 'Spaniels') %>%
  mutate(text = paste0("Breed: ",Breed, "\n",RankType,": ", Ranking))


# sequentially remove to observe remaining breeds as I subset
other<-breed_traits %>%
  filter(!grepl("Retriever", Breed)) %>%
  filter(!grepl("Terrier",Breed)) %>%
  filter(!grepl("hound",Breed)&!grepl("Hound",Breed)&!grepl("hund",Breed)) %>%
  filter(!grepl("heep",Breed)&!grepl("hepherd",Breed)&!grepl("Cattle",Breed)) %>%
  filter(!grepl("Spaniel",Breed))

dother<-other %>% distinct(Breed)
View(dother)

sub_breeds <- retriever %>%
  rbind(shepherd,
        spaniel)


## Graph in plotly


pretriever <- ggplot(retriever,
                     aes(x = RankType,
                         y = Breed,
                         fill = Ranking, text=text)) +
  geom_tile() +
  scale_fill_gradient(low="#a4e8e0", high="#2f5061") +
  theme_ipsum() +
  #theme(axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_blank()) +
  labs(title = "Retrievers")
p1 <- ggplotly(pretriever, tooltip="text")


pshepherd <- ggplot(shepherd,
                   aes(x = RankType,
                       y = Breed,
                       fill = Ranking, text=text)) +
  geom_tile() +
  scale_fill_gradient(low="#a4e8e0", high="#2f5061") +
  theme_ipsum() +
  #theme(axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=1)) +
  theme(axis.text.x = element_blank())+
  labs(title = "Shepherds")
p2 <- ggplotly(pshepherd, tooltip="text")


pspaniel <- ggplot(spaniel,
                   aes(x = RankType,
                       y = Breed,
                       fill = Ranking, text=text)) +
  geom_tile() +
  scale_fill_gradient(low="#a4e8e0", high="#2f5061") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 35, vjust = 0.5, hjust=1)) +
  labs(title = "Spaniels")
p3 <- ggplotly(pspaniel, tooltip="text")

# subplot(p2, p3, p4, p5, p1, nrows=3,
#         heights = c(0.4, 0.3, 0.3)) # dimensions must add to 1

subplot(p1, p2, p3, nrows = 3) %>%
        layout(title = list(text = "Ranking Some Good Bois"))

## Alternative plot in heatmap

library(superheat)

sub_breeds <- sub_breeds %>%
  select(-c(set,text)) %>%
  pivot_wider(names_from = RankType,
              values_from = Ranking) %>%
  column_to_rownames(var = 'Breed')

superheat(sub_breeds,
          scale = F,
          # add row dendrogram
          row.dendrogram = TRUE,
          # add text matrix
          X.text = round(as.matrix(sub_breeds), 1),
          X.text.col = "white",
          X.text.size = 4,
          # change the angle of the label text
          bottom.label.text.angle = 90,
          left.label.text.alignment = "left",
          bottom.label.text.alignment = "right",
          # change the size of the label text
          left.label.text.size = 3,
          bottom.label.text.size = 3,
          # change the color (#d8a7b1 = rosewater, teal and #05445e = navy)
          heat.pal = c("#d8a7b1", "#29a0b1", "#05445e"))




