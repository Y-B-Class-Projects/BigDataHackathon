#install.packages("RSQLite")

library("RSQLite")

books.db <- dbConnect(RSQLite::SQLite(), "BX-Books_hkv1.db")
books <- dbReadTable(books.db, "bx-books")
head(books)

ratings.db <- dbConnect(RSQLite::SQLite(), "BX-Ratings_hkv1.db")
ratings <- dbReadTable(ratings.db, "bx-book-ratings")
head(ratings)

users.db <- dbConnect(RSQLite::SQLite(), "BX-Users_hkv1.db")
users <- dbReadTable(users.db, "bx-users")
head(users)



