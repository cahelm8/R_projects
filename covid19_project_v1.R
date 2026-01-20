# getwd()

library(tidyverse)
library(readr)
library(tibble)
library(dplyr)

# load in the covid 19 dataframe
covid_df <- read.csv("covid19.csv")

# Understand the size of the dataset
dim(covid_df)

# define a variable as the column names from the covid-19 dataframe
vector_cols <- colnames(covid_df)

# View the first few parts of the dataset
head(covid_df)

# provide a concise view of the dataset
glimpse(covid_df)

# filter by "All States" in the Province_State Column and then remove the Province_State column
covid_df_all_states <- filter(covid_df, Province_State == "All States")
covid_df_all_states$Province_State <- NULL

# extract out only the relevant columns for additional analysis
covid_df_all_states_daily <- covid_df_all_states[,c("Date","Country_Region","active",
                                                 "hospitalizedCurr","daily_tested",
                                                 "daily_positive")]


# group the rows by country region and sum the daily values for each metric
covid_df_all_states_daily_sum <- covid_df_all_states_daily %>% 
  group_by(Country_Region) %>%
  summarise(tested = sum(daily_tested),
    positive = sum(daily_positive),
    active = sum(active),
    hospitalized = sum(hospitalizedCurr)
  )


# arrange the rows by descending order of covid-19 cases tested
covid_df_all_states_daily_sum <- covid_df_all_states_daily_sum %>% arrange(desc(tested))
view(covid_df_all_states_daily_sum)

# extract out the top 10 tested countries using head
covid_top_10 <- head(covid_df_all_states_daily_sum,10)
view(covid_top_10)

# extract out the relevant columns from the top 10 matrix and make into vectors
countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

# add the countries names to the vectors for each metric
names(tested_cases) <- c(countries)
names(positive_cases) <- c(countries)
names(active_cases) <- c(countries)
names(hospitalized_cases) <- c(countries)

# compute the ratio of positive cases to tested cases
ratio <- positive_cases / tested_cases

# extract the top three countries with the highest ratios
positive_tested_top_3 <- sort(ratio,decreasing = TRUE)
positive_tested_top_3 <- head(positive_tested_top_3,3)
view(positive_tested_top_3)

# Combined the positive to tested ratio with the other vectors and make a matrix
united_kingdom <- c(0.11,1473672, 166909, 0, 0)
united_states <- c(0.10,17282363, 1877179, 0, 0)
turkey <- c(0.08, 2031192, 153941, 2980960, 0)
covid_mat <- rbind(united_kingdom, united_states, turkey)

colnames(covid_mat) <- c("Ratio","tested","positive","active","hospitalized")
view(covid_mat)



question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)

# Create a list of the dataframes, matrices, and vectors
dataframes <- list(covid_df, covid_df_all_states, covid_df_all_states_daily, covid_top_10)
matrices <- list(covid_mat)
vectors <- list(vector_cols, countries)

# Create a data structure list with all the data
data_structure_list <- list(dataframes, matrices, vectors)
names(data_structure_list) <- c("Dataframes", "Matrices", "Vectors")

covid_analysis_list <- list(question, answer, data_structure_list)
print(covid_analysis_list[[2]])


