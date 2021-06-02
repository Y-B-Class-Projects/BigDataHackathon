#install.packages("RSQLite")
#install.packages("tidyverse")

library("RSQLite")
library("tidyverse")


books.db <- dbConnect(RSQLite::SQLite(), "BX-Books_hkv1.db")
books <- dbReadTable(books.db, "bx-books")
str(books)

ratings.db <- dbConnect(RSQLite::SQLite(), "BX-Ratings_hkv1.db")
ratings <- dbReadTable(ratings.db, "bx-book-ratings")
str(ratings)

users.db <- dbConnect(RSQLite::SQLite(), "BX-Users_hkv1.db")
users <- dbReadTable(users.db, "bx-users")
str(users)


#3a
nb_users <- nrow(users[!duplicated(users$User.ID),])
#3b
nb_books <- nrow(books[!duplicated(books$ISBN),])
#3c
nb_ratings <- nrow(ratings[ratings$Book.Rating,])

#3d
N <- 5
user_ratings <- data.frame(table(ratings$User.ID))
filter(user_ratings, Freq>=N )

#3e
N2 <- 7
book_ratings <- data.frame(table(ratings$ISBN))
filter(book_ratings, Freq>=N2 )

#3f
arrange(book_ratings, desc(Freq))

#3g
arrange(user_ratings, desc(Freq))
#order_df=df[order(df$Freq, decreasing = TRUE),]

