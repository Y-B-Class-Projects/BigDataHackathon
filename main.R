#install.packages("RSQLite")
#install.packages("tidyverse")
#install.packages("cgwtools")

start_time <- Sys.time()

library("RSQLite")
library("tidyverse")
library("recommenderlab")
library("stringr")
library("dplyr")
library("caret")
library("cgwtools")




books.db <- dbConnect(RSQLite::SQLite(), "BX-Books_hkv1.db")
books <- dbReadTable(books.db, "bx-books")
books <- distinct(books)
rm(books.db)  # clean RAM


ratings.db <- dbConnect(RSQLite::SQLite(), "BX-Ratings_hkv1.db")
ratings <- dbReadTable(ratings.db, "bx-book-ratings")
ratings <- filter(ratings, ratings$Book.Rating > 1)
#ratings <- arrange(ratings, ratings$User.ID)
rm(ratings.db)  # clean RAM

users.db <- dbConnect(RSQLite::SQLite(), "BX-Users_hkv1.db")
users <- dbReadTable(users.db, "bx-users")
rm(users.db)  # clean RAM



ratings <- ratings[(ratings$ISBN %in% books$ISBN),] #remove unknowns ISBNs from ratings.
ratings <- ratings[(ratings$User.ID %in% users$User.ID),] #remove unknowns ISBNs from ratings.

# To only save users that gived grades to more than N books and return in rating_test2
min.rating.user <- 2
min.rating.book <- 3


real.rating.matrix <- as(ratings, "realRatingMatrix")


while(min(rowCounts(real.rating.matrix)) < min.rating.user || min(colCounts(real.rating.matrix)) < min.rating.book){
  real.rating.matrix <- real.rating.matrix[rowCounts(real.rating.matrix) >= min.rating.user, colCounts(real.rating.matrix) >= min.rating.book]
  cat(".")
}

cat(paste("\nMin rating user", min(rowCounts(real.rating.matrix)), "\n" ,collapse = " "))
cat(paste("Min rating book", min(colCounts(real.rating.matrix)) , "\n" ,collapse = " "))
cat(paste("Dim of rating matrix is:", real.rating.matrix@data@Dim[1], ":", real.rating.matrix@data@Dim[2], "(", object.size(real.rating.matrix)/1000000000 , "Gb)\n" ,collapse = " "))

gc()

sets <- evaluationScheme(data = real.rating.matrix, method = "split",
                         train = 0.8, given = min.rating.user,
                         goodRating = 5, k = 5)


UB_recommender <- Recommender(data = getData(sets, "train"),
                              method = "UBCF", parameter = list(method='cosine'))


UB_prediction <- predict(UB_recommender,
                         newdata = getData(sets, "known"),
                         n = 10,
                         type = "ratings")


R.UB <- round(UB_prediction@data, digits = 1 )
colnames(R.UB) <- sapply( colnames(R.UB), function(c) books[books$ISBN == c,]$Book.Title )
R.UB <- as.matrix(R.UB)

accuracy.R.UB <- calcPredictionAccuracy(x = UB_prediction, given = min.rating.user,
                                        data = getData(sets, "unknown"), byUser=TRUE)

V.RMSE <- list()
V.RMSE[['UBCF']] <- accuracy.R.UB[,"RMSE"]

save(real.rating.matrix, sets, R.UB, file = "model.rdata")
save(UB_recommender,UB_prediction, accuracy.R.UB,  file = "temp.rdata")
rm(real.rating.matrix, R.UB, UB_prediction, UB_recommender, accuracy.R.UB)
gc()


IB_recommender <- Recommender(data = getData(sets, "train"),
                              method = "IBCF", parameter = list(method='cosine'))

IB_prediction <- predict(IB_recommender,
                            newdata = getData(sets, "known"),
                            n = 10,
                            type = "ratings")


R.IB <- round(IB_prediction@data, digits = 1 )
colnames(R.IB) <- sapply( colnames(R.IB), function(c) books[books$ISBN == c,]$Book.Title )
R.IB <- as.matrix(R.IB)



accuracy.R.IB <- calcPredictionAccuracy(x = IB_prediction, given = min.rating.user,
                                        data = getData(sets, "unknown"), byUser=TRUE)


V.RMSE[['IBCF']] <- accuracy.R.IB[,"RMSE"]

resave(R.IB, V.RMSE, file = "model.rdata")

#3a
#nb_users <- nrow(users[!duplicated(users$User.ID),])
#3b
#nb_books <- nrow(books[!duplicated(books$ISBN),])
#3c
#nb_ratings <- nrow(ratings[ratings$Book.Rating,])

#3d
#N <- 50
#user_ratings <- data.frame(table(ratings$User.ID))
#freq.user <- filter(user_ratings, Freq >= N )
#freq.user <- arrange(freq.user, desc(Freq))
#colnames(freq.user)<- c("ID", "Frequence")

#filter(ratings, User.ID %in% freq.user$ID)


#3e
#N2 <- 7
#book.freq.ratings <- data.frame(table(ratings$ISBN))
#filter(book.freq.ratings, Freq >= N2 )

#3f
#arrange(book.freq.ratings, desc(Freq))

#3g
#arrange(user_ratings, desc(Freq))
#order_df<-df[order(df$Freq, decreasing <- TRUE),]


end_time <- Sys.time()
dif_time <- end_time - start_time

end_time - start_time

#load("temp.rdata")
#load("model.rdata")

