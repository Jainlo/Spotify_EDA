# Install tidytuesdayR
install.packages("tidytuesdayR")
# Install GGally
install.packages("GGally")
library(tidyverse)
library(GGally)

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

# Ask questions
# Q1. Which genre is more popular? # Pop & Latin
spotify_songs %>%
  group_by(playlist_genre)%>%
  summarise(avg = mean(track_popularity))%>%
  ggplot(aes(playlist_genre, avg))+
  geom_col() #use ridge for popularity of every genre 

popularGenres <- c("pop", "latin")

# Isolate the 12 features
spotify_songs %>%
  select(playlist_genre, !track_id:playlist_subgenre) -> featuresOnly

# Q2. Which variable is highest in each of the two popular genres
# Break down the features into smaller groups (?)
featuresOnly %>%
  filter(playlist_genre %in% popularGenres)%>%
  pivot_longer(!c(playlist_genre, duration_ms), names_to = "features")%>%
  ggplot(aes(value, features))+
  geom_point()+
  facet_wrap(vars(playlist_genre), scales="free")

# Can we say that if Latin is most popular => and latin is charactrized by its danceability => danceability is a popularity feature?
# Q3. Which OTHER variable is more correlated with danceability
ggcorr(select(spotify_songs,danceability:duration_ms), method = c("everything", "pearson")) # valence
