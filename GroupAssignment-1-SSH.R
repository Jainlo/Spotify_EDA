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
# Import the data set
spotify_songs= read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv"))

#------------------------ # Data Wrangling # ---------------------#

# Check NA values
which(is.na(spotify_songs), arr.ind=TRUE)

# Getting  the names of the R data frame columns that contain missing values.
spotify_songs1 = as.data.frame(cbind(lapply(lapply(spotify_songs, is.na), sum)))
rownames(subset(spotify_songs1, spotify_songs1$V1 != 0))

# Removing and keeping the clean data without NA values
Clean_data=na.omit(spotify_songs)

# ------------------------ # Data Summary # ---------------------#
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

# The Most Popular Genre
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

# The plot shows that EDM genre has songs with highest energy.

Clean_data %>% 
  group_by(playlist_genre) %>% 
  summarise(mean = mean(energy)) %>% 
  arrange(desc(mean))


ggplot(Clean_data, aes(x=danceability, y=playlist_genre)) + 
  geom_boxplot() +
  geom_boxplot(fill='grey', color="black")+
  theme_classic()

Clean_data %>% 
  group_by(playlist_genre) %>% 
  summarise(mean = mean(danceability)) %>% 
  arrange(desc(mean))

#As seen in the graph, Rap genre has the highest danceability factor.

ggplot(Clean_data, aes(x=valence, y=playlist_genre)) + 
  geom_boxplot() +
  geom_boxplot(fill='grey', color="black")+
  theme_classic()

Clean_data %>% 
  group_by(playlist_genre) %>% 
  summarise(mean = mean(valence)) %>% 
  arrange(desc(mean))

#As seen above, Latin genre has a higher valence than others

energy_only <- cut(Clean_data$energy, breaks = 5)
Clean_data %>%
  ggplot( aes(x = energy_only )) +
  geom_bar( fill = "grey", colour = "black") +
  scale_x_discrete(name = "Energy")

# This plot shows that higher energy songs are popular among Spotify listeners.

speech_only <- cut(Clean_data$speechiness, breaks = 5)
Clean_data %>%
  ggplot( aes(x = speech_only )) +  
  geom_bar(fill = "grey", colour = "black") +
  scale_x_discrete(name = "Speechiness") 

## This plot shows that less speechy songs are more favoured by maximum Spotify listeners.

#credit to https://rpubs.com/nairrj/data-wrangling-on-spotify-data

### Let's test some hypothesis and see if the variables are indeed correlated.

Clean_data_numeric= Clean_data %>% dplyr::select(where(is.numeric))
corr <- round(cor(Clean_data_numeric),3)
ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)


# loudness & Energy.
val_nrgy <-Clean_data%>%ggplot(aes(x=loudness,y=energy))+geom_point()+ggtitle("Loudness/Energy")+theme_light()+geom_smooth(se=FALSE)+facet_wrap(~playlist_genre)
val_nrgy # More energetic song means the more loudness nearly in all genre

# acousticness & Energy.
val_nrgy1 <-Clean_data%>%ggplot(aes(x=acousticness,y=energy))+geom_point()+ggtitle("Acousticness/Energy")+theme_light()+geom_smooth(se=FALSE)+facet_wrap(~playlist_genre)
val_nrgy1 ## Energy & Acoustic. The lower the energy the more the acoustic.

#########################################
Y = Clean_data$acousticness
X = as.numeric(substr(Clean_data$track_album_release_date, 1,4))
by_years =data.frame(X,Y)
by_years <- aggregate(Y~X, data = by_years, mean)

# Changes by years
g= ggplot(by_years, aes(x = X, y = Y)) +
  geom_line(size=1.4) +
  labs(title = "Acousticness - Years Graph",
       x = "Years",
       y = "Avg. Acousticness")

#########################################
Y1 = Clean_data$loudness
X1 = as.numeric(substr(Clean_data$track_album_release_date, 1,4))
by_years1 =data.frame(X1,Y1)
by_years1 <- aggregate(Y1~X1, data = by_years1, mean)
g1= ggplot(by_years1, aes(x = X1, y = Y1)) +
  geom_line(size=1.4) +
  labs(title = "Loudness - Years Graph",
       x = "Years",
       y = "Avg. Loudness")
#########################################
Y2 = Clean_data$danceability
X2 = as.numeric(substr(Clean_data$track_album_release_date, 1,4))
by_years2 =data.frame(X2,Y2)
by_years2 <- aggregate(Y2~X2, data = by_years2, mean)
g2 = ggplot(by_years2, aes(x = X2, y = Y2)) +
  geom_line(size=1.4) +
  labs(title = "Danceability - Years Graph",
       x = "Years",
       y = "Avg. Danceability")
#########################################
Y3 = Clean_data$speechiness
X3 = as.numeric(substr(Clean_data$track_album_release_date, 1,4))
by_years3 =data.frame(X3,Y3)
by_years3 <- aggregate(Y3~X3, data = by_years3, mean)
g3 = ggplot(by_years3, aes(x = X3, y = Y3)) +
  geom_line(size=1.4) +
  labs(title = "Speechiness - Years Graph",
       x = "Years",
       y = "Avg. Speechiness")
#########################################
Y4 = Clean_data$track_popularity
X4 = as.numeric(substr(Clean_data$track_album_release_date, 1,4))
by_years4 =data.frame(X4,Y4)
by_years4 <- aggregate(Y4~X4, data = by_years4, mean)
g4 = ggplot(by_years4, aes(x = X4, y = Y4)) +
  geom_line(size=1.4) +
  labs(title = "Popularity - Years Graph",
       x = "Years",
       y = "Avg. popularity")
#########################################
Y5 = Clean_data$energy
X5 = as.numeric(substr(Clean_data$track_album_release_date, 1,4))
by_years5 =data.frame(X5,Y5)
by_years5 <- aggregate(Y5~X5, data = by_years5, mean)
g5 = ggplot(by_years5, aes(x = X5, y = Y5)) +
  geom_line(size=1.4) +
  labs(title = "Energy - Years Graph",
       x = "Years",
       y = "Avg. Energy")
#########################################
figure <- ggarrange(g, g1, g2,g3, g4,g5, ncol = 2, nrow = 3)
figure
#########################################
## References 
#1# "https://www.kaggle.com/code/fpolcari/spotify-songs-popularity-analysis"
#2# "https://www.linkedin.com/pulse/spotify-telugu-playlist-analysis-using-r-amrutha-killada/"
#3#  "https://github.com/kayamoon/spotify-music-data-analysis"
#4#  "https://www.kaggle.com/code/trixietacung/spotify-2010-2019-insight-analysis-with-r/report"

# ------------------------------------------------------------------------------- #