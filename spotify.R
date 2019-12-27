#load the data
getwd()
setwd("D://R//data//")
songs_data <- read.csv("top10s.csv")

head(songs_data)

#summary of the data
summary(songs_data)
str(songs_data)
songs_data$Song_no <- factor(songs_data$Song_no)
songs_data$Year<- factor(songs_data$Year)
str(songs_data)

#check for null values
table(is.na.data.frame(songs_data))

#changing column names
names(songs_data)
colnames(songs_data) <- c("Song_no","Song_title","Artist_name","Genre","Year","Beats_per_minute","Energy","Danceability","Loudness_decibles","Liveness","Valence","Duration","Acousticness","Speechiness","Popularity")



# Generalising the genres
vector = c()
for(v in songs_data$Genre)
{
  if(grepl("hip hop",v)){
    vector <- c(vector, "hip hop")
  } else if(grepl("pop",v)){
    vector <- c(vector,"pop")
  } else if(grepl("raggae",v)){
    vector <- c(vector,"raggae")
  } else if(grepl("boy band",v)){
    vector <- c(vector,"pop")
  } else if(grepl("edm",v)){
    vector <- c(vector,"electronic")
  } else if(grepl("electro",v)){
    vector <- c(vector,"electronic")
  } else if(grepl("rap",v)){
    vector <- c(vector,"rap")
  } else if(grepl("r&b",v)){
    vector <- c(vector,"r&b")
  } else{
    vector <- c(vector,"others")
  }
}


#add the general genres vector to the data frame
key <- data.frame(Song_no=songs_data$Song_no, GeneralGenre=vector)
songs_data <- merge(songs_data,key, by.x = "Song_no", by.y = "Song_no")

head(songs_data)

# plotting the data

library(ggplot2)

#plot popularity
table(songs_data$GeneralGenre)
diag1 <- ggplot(data=songs_data,aes(x=Popularity)) 
diag1 +  
  geom_histogram(binwidth = 10, aes(fill=GeneralGenre),color="Black") +
  facet_grid(GeneralGenre~Year) +
  ggtitle("Popularity Histogram by Genre and year")  

# Top 5 artists with maximum songs 2010- 2019

artist <- table(songs_data$Artist_name)
artist <- artist[rev(order(artist))][1:5]
newdf <- as.data.frame(artist)
diag2 <- ggplot(data=newdf,aes(x=Var1, y=Freq))
diag2 + 
  geom_bar(stat = "identity") + xlab("Name of artists") + ylab("Number of songs released")+
  ggtitle("Top5 artists - number of songs")


# scatterplots
diag3 <- ggplot(data=songs_data,aes(x=Beats_per_minute,y=Energy,color=GeneralGenre))
diag3 + geom_point(alpha=0.2,aes(size=Popularity)) + ggtitle("Beats per minute and energy")+
  facet_grid(GeneralGenre~Year)

#Beats per minute and popularity

diag4 <- ggplot(data=songs_data,aes(x=Beats_per_minute,y=Popularity))
diag4 + geom_point(aes(color=GeneralGenre, shape=GeneralGenre)) +
  ggtitle("Beats per minute and Popularity")

#Beats per minute and popularity over the years
diag4 <- ggplot(data=songs_data,aes(x=Beats_per_minute,y=Popularity))
diag4 + 
  geom_point(aes(color=GeneralGenre, shape=GeneralGenre)) +
  ggtitle("Beats per minute and Popularity") + facet_grid(~Year)

#Acousticness and Valence

diag4 <- ggplot(data=songs_data,aes(x=Acousticness,y=Valence))
diag4 + 
  geom_point(alpha=0.5,aes(color=GeneralGenre,shape=GeneralGenre)) +
  geom_smooth(method="lm") + 
  ggtitle("Acousticness and Valence")

#Most popular songs and artist
Maxpopular <- songs_data[rev(order(songs_data$Popularity)),]
Maxpopular <- Maxpopular[1:5,]
diag5 <- ggplot(data=Maxpopular,aes(x=paste(Song_title,"-",Artist_name), y=Popularity))
diag5 + 
  geom_bar(stat = "identity") + xlab("Name of artists") + 
  ylab("Popularity (0-100)")+
  ggtitle("Top5 artists - Popularity of songs")


#Boxplots

diag6 <- qplot(y=songs_data$Acousticness, x= 1, geom = "boxplot")
diag6


