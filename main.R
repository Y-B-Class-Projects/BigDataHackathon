#install.packages("RSQLite")
#install.packages("tidyverse")

start_time <- Sys.time()

library("RSQLite")
library("tidyverse")
library("recommenderlab")
library("stringr")
library("dplyr")
library("caret")


my.predict <- function (model, newdata, n = 10, data = NULL,
                        type = c("topNList", "ratings", "ratingMatrix"), ...)
{
  type <- match.arg(type)
  newdata_id <- NULL
  if (is.numeric(newdata)) {
    if (model$sample)
      stop("(EE) User id in newdata does not work when sampling is used!")
    newdata_id <- newdata
    newdata <- model$data[newdata, ]
  }
  else {
    if (ncol(newdata) != ncol(model$data))
      stop("(EE) number of items in newdata does not match model.")
    if (!is.null(model$normalize))
      newdata <- normalize(newdata, method = model$normalize)
  }

  cat('(II) running similarity() calculation\n')
  sim <- similarity(newdata, model$data, method = model$method,
                    min_matching = model$min_matching_items,
                    min_predictive = model$min_predictive_items)
  cat('(II) similarity() done\n')

  if (!is.null(newdata_id))
    sim[cbind(seq(length(newdata_id)), newdata_id)] <- NA

  cat(paste('(II) creating knn with', model$nn ,'neighbors\n'))
  neighbors <- .knn(sim, model$nn)
  cat('(II) knn done\n')

  if (model$weighted) {
    cat ('(II) weigh the ratings by similarities\n')
    s_uk <- sapply(1:nrow(sim), FUN = function(i) sim[i, neighbors[[i]]])
    if (!is.matrix(s_uk)) s_uk <- as.matrix(t(s_uk))
    ratings <- t(sapply(1:nrow(newdata), FUN = function(i) {
      r_neighbors <- as(model$data[neighbors[[i]]], "dgCMatrix")
      Cols.1 <- drop(as(crossprod(r_neighbors, as.numeric(as.data.frame(s_uk[, i])[,])), "matrix"))
      Cols.2 <- drop(as(crossprod(!dropNAis.na(r_neighbors), as.numeric(as.data.frame(s_uk[, i])[,])), "matrix"))
      Cols.1 / Cols.2
    }))
    ratings[!is.finite(ratings)] <- NA
    cat ('(II) done weigh the ratings\n')
  }
  else {
    cat ("(II) copy the ratings across the user's knn\n")
    ratings <- t(sapply(1:nrow(newdata), FUN = function(i) {
      r_neighbors <- as(model$data[neighbors[[i]]], "dgCMatrix")
      Cols.1 <- colSums(r_neighbors)
      Cols.2 <- colSums(!dropNAis.na(r_neighbors))
      Cols.1 / Cols.2
    }))
    ratings[!is.finite(ratings)] <- NA
    cat ("(II) copy the ratings ... done\n")
  }
  rownames(ratings) <- rownames(newdata)
  ratings <- new("realRatingMatrix", data = dropNA(ratings),
                 normalize = getNormalize(newdata))

  cat ('(II) de-normalize the ratings (back to rating scale)\n')
  ratings <- denormalize(ratings)
  cat ('(II) de-normalize done\n')

  returnRatings(ratings, newdata, type, n)
}
.knn <- function(sim, k)
  lapply(1:nrow(sim), FUN = function(i)
    head(order(sim[i,], decreasing = TRUE, na.last = NA), k))

#########################################################################


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
min.rating.user <- 10
min.rating.book <- 10


#book.freq.ratings <- data.frame(table(ratings$ISBN))
#freq.book <- filter(book.freq.ratings, Freq >= min.rating.book )
#colnames(freq.book)<- c("ISBN", "Frequence")
#ratings <- filter(ratings, ISBN %in% freq.book$ISBN)

#user.freq.ratings <- data.frame(table(ratings$User.ID)) # give frequence of ID_User
#freq.user <- filter(user.freq.ratings, Freq >= min.rating.user ) # take only user that appears more of N
#freq.user <- arrange(freq.user, desc(Freq)) # arrange with big freq at the beginning
#colnames(freq.user)<- c("ID", "Frequence")
#ratings<- filter(ratings, User.ID %in% freq.user$ID)



real.rating.matrix <- as(ratings, "realRatingMatrix")


while(min(rowCounts(real.rating.matrix)) < min.rating.user || min(colCounts(real.rating.matrix)) < min.rating.book){
  real.rating.matrix <- real.rating.matrix[rowCounts(real.rating.matrix) >= min.rating.user, colCounts(real.rating.matrix) >= min.rating.book]
  cat(".")
}

cat(paste("\nMin rating user", min(rowCounts(real.rating.matrix)), "\n" ,collapse = " "))
cat(paste("Min rating book", min(colCounts(real.rating.matrix)) , "\n" ,collapse = " "))
cat(paste("Dim of rating matrix is:", real.rating.matrix@data@Dim[1], ":", real.rating.matrix@data@Dim[2], "(", object.size(real.rating.matrix)/1000000000 , "Gb)\n" ,collapse = " "))

sets <- evaluationScheme(data = real.rating.matrix, method = "split",
                         train = 0.8, given = min.rating.user,
                         goodRating = 5, k = 5)


UB_recommender <- Recommender(data = getData(sets, "train"),
                              method = "UBCF", parameter = list(method='cosine'))

IB_recommender <- Recommender(data = getData(sets, "train"),
                              method = "IBCF", parameter = list(method='cosine'))


UB_prediction <- predict(UB_recommender,
                              newdata = getData(sets, "known"),
                              n = 10,
                              type = "ratings")

IB_prediction <- predict(IB_recommender,
                            newdata = getData(sets, "known"),
                            n = 10,
                            type = "ratings")

R.UB <- round(UB_prediction@data, digits = 1 )
colnames(R.UB) <- sapply( colnames(R.UB), function(c) books[books$ISBN == c,]$Book.Title )
R.UB <- as.matrix(R.UB)

R.IB <- round(IB_prediction@data, digits = 1 )
colnames(R.IB) <- sapply( colnames(R.IB), function(c) books[books$ISBN == c,]$Book.Title )
R.IB <- as.matrix(R.IB)

accuracy.R.UB <- calcPredictionAccuracy(x = UB_prediction, given = min.rating.user,
                                        data = getData(sets, "unknown"), byUser=TRUE)

accuracy.R.IB <- calcPredictionAccuracy(x = IB_prediction, given = min.rating.user,
                                        data = getData(sets, "unknown"), byUser=TRUE)

V.RMSE <- list()
V.RMSE[['UBCF']] <- accuracy.R.UB[,"RMSE"]
V.RMSE[['IBCF']] <- accuracy.R.IB[,"RMSE"]

save(real.rating.matrix, sets, R.UB, R.IB, V.RMSE, file = "model.rdata")

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



