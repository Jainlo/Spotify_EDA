# Uploading important libraries # 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(janitor)
library(tidytuesdayR)
library(ggcorrplot)
library(ggpubr)
# -------------------------------- #
source("0-dataMunging.R")
# -------------------------------- #

# Top 5 Artists
Top_Artists <- Clean_data %>%
  group_by(track_artist) %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 100) %>%
  arrange(desc(n_apperance))

Top_Artists$track_artist <- factor(Top_Artists$track_artist, levels = Top_Artists$track_artist [order(Top_Artists$n_apperance)])

ggplot(Top_Artists, aes(x = track_artist, y = n_apperance)) +
  geom_bar(stat = "identity", width = 0.6,fill = "grey", color="black") +
  labs(title = "Popular Artists", x = "Artists", y = "Number of Apperances in the list") +
  theme(plot.title = element_text(size=15, hjust=0, face = "bold"), axis.title = element_text(size=12)) +
  geom_text(aes(label=n_apperance), hjust=2, size = 3, color = 'white') +
  coord_flip()

# Top 5 Albums
Top_Albums <- Clean_data %>%
  group_by(track_album_name) %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance >= 29) %>%
  arrange(desc(n_apperance))

Top_Albums$track_album_name <- factor(Top_Albums$track_album_name, levels = Top_Albums$track_album_name [order(Top_Albums$n_apperance)])

ggplot(Top_Albums, aes(x = track_album_name, y = n_apperance)) +
  geom_bar(stat = "identity" , width = 0.6,fill = "grey", color="black") +
  labs(title = "Popular Albums", x = "Albums", y = "Number of Apperances in the list") +
  theme(plot.title = element_text(size=15, hjust=0, face = "bold"), axis.title = element_text(size=12)) +
  geom_text(aes(label=n_apperance), hjust=2, size = 3, color = 'white') +
  coord_flip()

# Top Genre listened to
ggplot(Clean_data) + 
  geom_bar(mapping = aes(x = playlist_genre), fill = "grey", color="black") + 
  labs(title = "The Most Popular Genre", x = "Playlist Genre", y = "Number of Genre")+
  theme(plot.title = element_text(size=15), axis.text = element_text(size = 15), axis.title = element_text(size = 15))

table(Clean_data$playlist_genre)
# Credit to "https://www.kaggle.com/code/nuyangrai/spotify-songs-analysis"

ggplot(Clean_data, aes(x=energy, y=playlist_genre)) + 
  geom_boxplot() +
  geom_boxplot(fill='grey', color="black")+
  theme_classic()

# Top Genres using the popularity feature

spotify_songs %>%
  group_by(playlist_genre)%>%
  summarise(avg = mean(track_popularity))%>%
  ggplot(aes(playlist_genre, avg))+
  geom_col() 

popularGenres <- c("pop", "latin")


