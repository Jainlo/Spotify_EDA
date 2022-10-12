# Download all data files
tuesdata <- tidytuesdayR::tt_load('2020-01-21')

# Individual dataframe
spotify_songs <- tuesdata$spotify_songs

# Import the data set
# spotify_songs= read_csv(url("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv"))

# Check NA values
summary(spotify_songs)
which(is.na(spotify_songs), arr.ind=TRUE)

# Getting  the names of the R data frame columns that contain missing values.
spotify_songs1 <- as.data.frame(cbind(lapply(lapply(spotify_songs, is.na), sum)))
rownames(subset(spotify_songs1, spotify_songs1$V1 != 0))

# Removing and keeping the clean data without NA values
Clean_data=na.omit(spotify_songs)

# Check the structure of the data
glimpse(Clean_data)

# Check column names
names(Clean_data)

# Check the different genres
table(Clean_data$playlist_genre)

# Check pairwise plot
# Exclude some columns to avoid errors related to cardinality
Clean_data %>%
  select(!track_id:playlist_subgenre)%>%
  ggpairs()