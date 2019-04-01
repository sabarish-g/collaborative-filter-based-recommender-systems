#ML Project
#Sabarish G
setwd("C:/Users/Sabarish/Desktop/MS Studies/1. Machine Intelligence/Project")

library(data.table)
library(RColorBrewer)
library(ggplot2)
library(recommenderlab)
library(recosystem)
library(sqldf)
library(dplyr)


#Reading the files
#1. Users
users <- read.delim("users.dat", header = FALSE,sep = ":")
users <- users[,c("V1","V3","V5","V7","V9")]
colnames(users) <- c("UserID","Gender", "Age","Occupation","Pincode" )

#2. Movies
movies <- read.csv("movies.csv", header = T)


#3. Ratings
ratings <- read.delim("ratings.dat", header = FALSE,sep = ":")
ratings <- ratings[,c("V1","V3","V5", "V7")]
colnames(ratings) <- c("UserID","MovieID", "Rating", "Timestamp" )



head(ratings)
#Distributon of ratings
ggplot(data = ratings,  aes(Rating))+geom_bar()

#Distribution of user ratings
average_users_rating <- data.frame()
average_users_rating <- ratings %>%
  group_by(UserID) %>%
  summarize(Mean = mean(Rating, na.rm=TRUE))

average_movie_rating <- data.frame()
average_movie_rating <- ratings %>%
  group_by(MovieID) %>%
  summarize(Mean = mean(Rating, na.rm=TRUE))
average_movie_rating$Mean_rounded <- round(average_movie_rating$Mean,0)

ggplot(data = average_users_rating,  aes(Mean))+geom_histogram(bins = 75)+labs(x = "User Rating", y = "Frequency", title = "Distribution Of User Average Ratings")
ggplot(data = average_movie_rating,  aes(Mean))+geom_histogram(bins = 25)+labs(x = "Movie Rating", y = "Frequency", title = "Distribution Of Average Movie Ratings")

  
sparse_ratings <- sparseMatrix(i = ratings$UserID, j = ratings$MovieID, x = ratings$Rating)

real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings



set.seed(1)

e <- evaluationScheme(real_ratings, method="split", train=0.8, given=-5)
#5 ratings of 20% of users are excluded for testing

set.seed(1)
#User Based
model <- Recommender(getData(e, "train"), method = "UBCF", 
                     param=list(normalize = "center", method="Cosine", nn=120))

prediction <- predict(model, getData(e, "known"), type="ratings")

rmse_ubcf <- calcPredictionAccuracy(prediction, getData(e, "unknown"))[1]
rmse_ubcf


#Item Based
model1 <- Recommender(getData(e, "train"), method = "IBCF", param=list(normalize = "center", method="Pearson", k=850))

prediction1 <- predict(model1, getData(e, "known"), type="ratings")

rmse_ibcf <- calcPredictionAccuracy(prediction1, getData(e, "unknown"))[1]
rmse_ibcf
  

user_based_results <- read.csv("user_based_results.csv",header = T)
item_based_results <- read.csv("item_base_results.csv",header = T)
colnames(user_based_results) <- c("NN", "Pearson RMSE", "Cosine RMSE")
colnames(item_based_results) <- c("NN", "Pearson RMSE", "Cosine RMSE")

user_based_data_long <- melt(user_based_results, id="NN")
ggplot(data = user_based_data_long,  aes(x= NN, y = value, color = variable))+geom_line()+labs(x = "Nearest Neighbour", y = "RMSE", title = "User Based Collaborative Filtering")

item_based_data_long <- melt(item_based_results, id="NN")
ggplot(data = item_based_data_long,  aes(x= NN, y = value, color = variable))+geom_line()+labs(x = "Nearest Neighbour", y = "RMSE", title = "Item Based Collaborative Filtering")


