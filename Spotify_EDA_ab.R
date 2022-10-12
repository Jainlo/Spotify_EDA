# Install tidytuesdayR
install.packages("tidytuesdayR")
# Install GGally
install.packages("GGally")
library(tidyverse)
library(GGally)
library(ggridges)
library(lubridate)
# Download all data files
tuesdata <- tidytuesdayR::tt_load('2020-01-21')

# Individual dataframe
spotify_songs <- tuesdata$spotify_songs

# Check the structure of the data
glimpse(spotify_songs)

# Check column names
names(spotify_songs)

# Check for missing data
summary(spotify_songs)

# Check the different genres
table(spotify_songs$playlist_genre)
table(spotify_songs$playlist_subgenre)
spotify_songs[rowSums(is.na(spotify_songs)) > 0,]


# Converting the time to more meaningful unit Minute:Second
spotify_songs %>% 
  mutate(duration_s=duration_ms/1000,
         sec = duration_s %% 60,
         min = (duration_s / 60) |> floor(),
         duration_minsec = paste(min, sec),
         duration_minsec = ms(duration_minsec)) %>%  
  select(-sec, -min) -> spotify_songs

# Check for duplicate songs within the data
sum(duplicated(spotify_songs$track_name))

# the result is too large = 9383 it could b edue too some names used a lot fot song's names

# Combining the name of the artist and the song to make sure we are looking for one entry per distinct song

spotify_songs$track_name_artist = paste(spotify_songs$track_name ,spotify_songs$track_artist)
spotify_songs[duplicated(spotify_songs$track_name_artist),] ->dups
# There are 6603 dublicates songs by the same artist 

#removing the   duplicates 
spotify_songs %>% distinct(track_name_artist, .keep_all = TRUE) -> spotify_songs_nodup



songs_before <-
  table(spotify_songs$playlist_genre)

songs_after <-
  table(spotify_songs_nodup$playlist_genre)
songs_b_a <-tibble(songs_before, songs_after)
songs_b_a %>% 
  mutate(genre = names(songs_after)) ->
  songs_b_a

songs_b_a %>% 
  pivot_longer(!genre,) -> songs_b_a
songs_b_a$value <- as.numeric(songs_b_a$value)

songs_b_a %>%  # Try to flip the position of the bars
  ggplot(aes(x = genre, y = value , fill = name))+
  geom_bar(stat = "identity", position = 'dodge' )


# Ask questions
# The popularity by genre? also comparing before and after the duplicates
spotify_songs %>%
  group_by(playlist_genre)%>%
    ggplot(aes( x = track_popularity, y = playlist_genre ))+
    geom_density_ridges()+
    scale_x_continuous(limits = c(0, 100)) # it does not make sense to have a negative or above 100 popularity



spotify_songs_nodup %>%
  group_by(playlist_genre)%>%
  ggplot(aes( x = track_popularity, y = playlist_genre ))+
  geom_density_ridges2()+
  scale_x_continuous(limits = c(0, 100)) # it does not make sense to have a negative or above 100 popularity
#results: less popular song. Possible reason: popular songs are released multiple times 

spotify_songs %>%
  group_by(playlist_genre)%>%
  ggplot(aes( x = duration_minsec, y = track_popularity, alpha = 0.5 ))+
  geom_point()+
  scale_x_time() #important to set the x-axis as time format


# what is the most important factor for each genre

spotify_clean<- spotify_songs_nodup %>% 
  select(track_popularity, playlist_genre, danceability, energy, key, loudness, mode,speechiness,acousticness, instrumentalness, liveness, valence, tempo,duration_s)


# Dividing songs per genre


songs_rock <-spotify_clean %>% filter(playlist_genre == "rock")
songs_rock %>% 
  select(-playlist_genre) %>% 
  pivot_longer(!track_popularity) ->songs_rock

songs_rock %>% 
  ggplot(aes(x = value, y = track_popularity))+
  geom_point()+
  facet_wrap(vars(name),scales = "free")


songs_pop <-spotify_clean %>% filter(playlist_genre == "pop")
songs_pop %>% 
  select(-playlist_genre) %>% 
  pivot_longer(!track_popularity) ->songs_pop
songs_pop %>% 
  ggplot(aes(x = value, y = track_popularity))+
  geom_point()+
  facet_wrap(vars(name),scales = "free")


songs_edm <-spotify_clean %>% filter(playlist_genre == "edm")
songs_edm %>% 
  select(-playlist_genre) %>% 
  pivot_longer(!track_popularity) ->songs_edm
songs_edm %>% 
  ggplot(aes(x = value, y = track_popularity, alpha = 0.25))+
  geom_point()+
  facet_wrap(vars(name),scales = "free")


songs_latin<-spotify_clean %>% filter(playlist_genre == "latin")
songs_latin %>% 
  select(-playlist_genre) %>% 
  pivot_longer(!track_popularity) ->songs_latin
songs_latin %>% 
  ggplot(aes(x = value, y = track_popularity, alpha = 0.25))+
  geom_point()+
  facet_wrap(vars(name),scales = "free")


songs_latin<-spotify_clean %>% filter(playlist_genre == "latin")
songs_latin %>% 
  select(-playlist_genre) %>% 
  pivot_longer(!track_popularity) ->songs_latin
songs_latin %>% 
  ggplot(aes(x = value, y = track_popularity, alpha = 0.25))+
  geom_point()+
  facet_wrap(vars(name),scales = "free")


songs_rb<-spotify_clean %>% filter(playlist_genre == "r&b")
songs_rb %>% 
  select(-playlist_genre) %>% 
  pivot_longer(!track_popularity) ->songs_rb
songs_rb %>% 
  ggplot(aes(x = value, y = track_popularity, alpha = 0.25))+
  geom_point()+
  facet_wrap(vars(name),scales = "free")


songs_rb<-spotify_clean %>% filter(playlist_genre == "r&b")
songs_rb %>% 
  select(-playlist_genre) %>% 
  pivot_longer(!track_popularity) ->songs_rb
songs_rb %>% 
  ggplot(aes(x = value, y = track_popularity, alpha = 0.25))+
  geom_point()+
  facet_wrap(vars(name),scales = "free")

songs_rap<-spotify_clean %>% filter(playlist_genre == "rap")
songs_rap %>% 
  select(-playlist_genre) %>% 
  pivot_longer(!track_popularity) ->songs_rap
songs_rap %>% 
  ggplot(aes(x = value, y = track_popularity, alpha = 0.25))+
  geom_point()+
  facet_wrap(vars(name),scales = "free")




#EDA on each genre





