# Load necessary libraries
library(dplyr)
library(readr)
library(purrr)

# Define path to the directory
path_to_dir <- "cleandata"

# Get list of all csv files in the directory, except 'data_new.csv'
all_files <- list.files(path = path_to_dir, pattern = "*.csv", full.names = TRUE)
all_files <- all_files[!grepl("data_new.csv", all_files)]

# Read and combine all csv files, and print the name of each file as it is processed
combined_df <- map_df(all_files, function(file) {
  print(paste("Processing file:", file))
  read_csv(file, show_col_types = T)
})

# Arrange by 'LastName' and then 'Apparatus'
combined_df <- combined_df %>% arrange(LastName, Apparatus)

# Write to a new csv file
write_csv(combined_df, file.path(path_to_dir, "data_new.csv"))