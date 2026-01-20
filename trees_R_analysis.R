
setwd("~/Delaware/Professional Development/Technical Skills Courses/R")

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
# Load the built-in 'trees' dataset into the R session environment
data(trees)

# View the first few rows of the dataset
head(trees)

# Get a description and structure of the dataset
?trees
str(trees) # displays the internal structure of the dataframe

dim(trees) # retrieves the dimensions of the dataframe


# mutate is used for creating new columns or modifying existing ones
trees <- trees %>% 
  mutate(Volume = round(Volume))

print(trees)

trees <- trees %>% 
  mutate(
    Volume_group = cut(
      Volume,
      breaks = seq(min(Volume), max(Volume) + 10, by = 10), # adjust max as needed
      right = FALSE
    )
  )

############# Plot a histogram of the volume of trees

ggplot(data = trees, aes(x = Volume)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 5, fill = 'blue', alpha = 0.5) +
  # Add the normal density curve using stat_function
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(trees$Volume), sd = sd(trees$Volume)),
    color = "blue", 
    linewidth = 2.5) +
  xlab("Volume (in m^3)") + 
  ylab("Counts") +
  ggtitle("Tree Volume Distribution") +
  theme_minimal()


######### Summarize the stats for volumes for trees

summary_Volume <- trees %>%
  summarise(
    mean_Volume = mean(Volume, na.rm = TRUE),
    median_Volume = median(Volume, na.rm = TRUE),
    sd_Volume = sd(Volume, na.rm = TRUE),
    min_Volume = min(Volume, na.rm = TRUE),
    max_Volume = max(Volume, na.rm = TRUE)
  )

# View the result
print(summary_Volume)


########## plotting Height vs Volume linear regression model

ggplot(data = trees, aes(x = Height, y = Volume)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black", se = TRUE, lwd = 1.5) +
  xlab("Height (meters)") + 
  ylab("Volume (m^3)")


# Fit the linear model and save it to a variable called 'model'
model <- lm(Volume ~ Height, data = trees)

summary(model)

print(trees)


########## plotting Girth vs Height linear regression model
ggplot(data = trees, aes(x = Girth, y = Height)) +
  geom_point() +
  geom_smooth(method = "lm", col = "black", se = TRUE, lwd = 1.5) +
  xlab("Girth (meters)") +
  ylab("Height (meters)")

model <- lm(Height ~ Girth, data = trees)

summary(model)  
print(model)


##### Distribution of the height of trees plot
ggplot(data = trees, aes(x = Height)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, fill = 'green', alpha = 0.35) +
  # Add the normal density curve using stat_function
  stat_function(
    fun = dnorm, 
    args = list(mean = mean(trees$Height), sd = sd(trees$Height)),
    color = "green", 
    linewidth = 2.5) +
  xlab("Height (meters)") + 
  ylab("Counts") +
  ggtitle("Tree Height Distribution") +
  theme_minimal()


############ Summarize the tree heights
summary(trees$Height)

summary_Height <- trees %>% 
  summarise (
  mean_height = mean(Height),
  median_height = median(Height),
  sd_height = sd(Height),
  min_height = min(Height),
  max_height = max(Height)
  )

print(summary_Height)
  
  
  



