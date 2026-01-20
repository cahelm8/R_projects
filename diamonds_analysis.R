
getwd()

# getwd() - get the working directory
# clear console - CTL L

rm(list = ls()) # clears the environmental variables

library(tidyverse)
library(readr) # fast and simple way to read table data
library(tibble) # enables data analysis to be done cleanlier
library(dplyr) # enables verbs for data manipulation
library(tidyr) # makes data easy to work with (inside the tidyverse)
library(ggplot2) # used for easy data visualization


########## Load in the data
# View the dataset in a spreadsheet-like format in RStudio
View(diamonds)

# Print the first few rows to the console
head(diamonds)

# Get a summary of the data structure and variables
str(diamonds)

# Access the documentation for details on the variables (e.g., price, carat, cut, color, clarity)
?diamonds

summary(diamonds)

diamonds %>% 
  mutate(price200 = price - 200)

dim(diamonds)

ggplot(data=diamonds)+ 
  geom_histogram(binwidth=100, aes(x=diamonds$price)) + 
  ggtitle("Diamond Price Distribution") + 
  xlab("Diamond Price USD") + 
  ylab("Frequency") + 
  theme_minimal() + 
  xlim(0,2500)


sum(diamonds$price < 500)
