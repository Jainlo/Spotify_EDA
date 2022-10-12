# Uploading important libraries # 
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)
library(janitor)
library(tidytuesdayR)
library(ggcorrplot)
library(ggpubr)
install.packages("GGally")
library(GGally)

# -------------------------------- #
source("1-dataExploration.R")
# -------------------------------- #

# Isolate the 12 features
spotify_songs %>%
  select(playlist_genre, !track_id:playlist_subgenre) -> featuresOnly

# How do features compare between genres
featuresOnly %>%
  #filter(playlist_genre %in% popularGenres)%>%
  #group_by(playlist_genre)%>%
  pivot_longer(!c(playlist_genre), names_to = "features")%>%
  ggplot(aes(value))+
  geom_density(aes(color = playlist_genre), alpha = 0.5)+
  facet_wrap(vars(features), scales="free")

# Break down the features into smaller groups to find correlation(?)

# Q3. Which OTHER variable is more correlated with danceability
ggcorr(select(spotify_songs,danceability:duration_ms), method = c("everything", "pearson")) # valence

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