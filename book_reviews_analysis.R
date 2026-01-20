
getwd()

library(tidyverse)
library(readr)
library(tibble)
library(dplyr)
library(tidyr)


# Load in the csv file
book_reviews <- read.csv("book_reviews.csv")

glimpse(book_reviews)

col_names <- colnames(book_reviews)
unqiue_values <- lapply(book_reviews, unique)


for (n in col_names) {
  print(n)
  col_NAs[n] <- sum(!is.na(book_reviews[[n]]))
}
print(col_NAs)


# Dealing with columns with missing data

# Remove any row with NA in the entire dataframe
filtered_book_reviews <- na.omit(book_reviews)

# Check the size of the dataset after removing NAs
dim(filtered_book_reviews)



# Dealing with inconsistent state column labels

filtered_book_reviews <- filtered_book_reviews%>% 
  mutate(state_Abrev = recode( state,
                      "Florida" = "FL",
                      "California" = "CA",
                        "New York" = "NY",
                          "Texas" = "TX"))


# Transforming the review data into numerical format

filtered_book_reviews <- filtered_book_reviews%>% 
  mutate(review_num = case_when(
                              review == "Poor" ~ 1,
                              review == "Fair" ~ 2,
                              review == "Good" ~ 3,
                              review == "Great" ~ 4,
                              review == "Excellent" ~ 5,
                              )) %>% 
  mutate(is_high_review = case_when(
    review_num >= 4 ~ "TRUE",
    TRUE ~ "FALSE"
  ))


# Analyzing the data to identify top-performing titles

# Most profitable book - price column with the number of books purchased

# Compute the total number that each book was purchase
book_count_sorted <- filtered_book_reviews %>% 
  count(book) %>% 
  arrange(-n)

print(book_count_sorted[1,])

top_books_count <- head(book_count_sorted,3)
print(top_books_count)

print("Fundamentals of R For Beginners is the most popular.")


# It would be best to focus efforts on marketing and producing more of
# "Fundamentals of R For Beginners' as this book was sold the most. 

# Extract the books in order total pricing the book generated (highest to lowest)
total_price_sorted <- filtered_book_reviews %>%
  group_by(book) %>%
  summarise(total_price = sum(price)) %>% 
  arrange(-total_price) %>% 
  select(book, total_price)

top_books_revenue <- head(total_price_sorted,3)
print(top_books_revenue)

print("Secrets of R For Advanced Students is the most profitable.")

# Additionally, focusing efforts on 'Secrets of R for Advanced Students'
# would also be ideal as this book has brought in the most profits.

# Reporting the Results




