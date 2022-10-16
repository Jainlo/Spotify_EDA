
library(tidyverse)
library(lubridate)
library(tidytuesdayR)
library(GGally)



# Download all data files
tuesdata <- tidytuesdayR::tt_load('2020-01-21')

# Individual dataframe
spotify_songs <- tuesdata$spotify_songs

# Import the data set
# spotify_songs= read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv"))

#summary of the data set
summary(spotify_songs)

# Check NA values and print them to the console
spotify_songs[rowSums(is.na(spotify_songs)) > 0, ]
# There is no need to remove them since the only thing is missing is the track name
# , track artist and album name

# Check for duplicate songs' names within the data
sum(duplicated(spotify_songs$track_name))

# The number is too large
# Check for duplicates bu id:
sum(duplicated(spotify_songs$track_id))

#Saving the duplicates to a data frame to investigate 
songs_duplicates <- spotify_songs[duplicated(spotify_songs$track_id),] 
#The duplicate exist because songs are on different playlists 

#changing the class of the key and mode
spotify_songs$mode <- as_factor(spotify_songs$mode)
spotify_songs$key  <- as_factor(spotify_songs$key)
class(spotify_songs$key)
class(spotify_songs$mode)


#Removing duplicates
clean_data <- spotify_songs %>% 
  distinct(track_id,
           .keep_all = TRUE) 





# Check the structure of the data
glimpse(clean_data)

# Check column names
names(clean_data)

# Check the different genres
table(clean_data$playlist_genre)


