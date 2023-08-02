## Load necessary libraries
library(dplyr)
library(readr)
library(purrr)

## Define path to the directory
path_from_dir <- "../cleandata/data_new"
path_to_dir <- "../cleandata"


## Get list of all csv files in the directory, except 'data_new.csv'
all_files <- list.files(path = path_from_dir, pattern = "*.csv", full.names = TRUE)

## Read and combine all csv files, 
## and print the name of each file as it is processed
combined_df <- map_df(all_files, function(file) {
  print(paste("Processing file:", file))
  read_csv(file, show_col_types = T)
})

## Arrange by 'LastName', 'FirstName', 'Competition' and then 'Apparatus'
combined_df <- combined_df %>%
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  arrange(LastName, FirstName, Competition, Apparatus) %>%
  group_by(LastName) %>%
  fill(FirstName, .direction = "downup")

## Write to a new csv file
write_csv(combined_df, file.path(path_to_dir, "data_2022_2023.csv"), na = "")