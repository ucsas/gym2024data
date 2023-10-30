## Load necessary libraries
library(tidyverse)

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
  fill(FirstName, .direction = "downup") %>% 
  mutate(Apparatus = ifelse(Apparatus == "RG", "SR", Apparatus))

## 把LastName全大写，有些来自web scraping的数据LastName没有大写
combined_df <- combined_df %>%
  mutate(LastName = str_to_upper(LastName)) %>% 
  arrange(LastName, FirstName, Competition, Apparatus)

## manually adjust for name differences
combined_df <- combined_df %>%
  mutate(FirstName = ifelse(LastName == "AAS", "Fredrik", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "ABIYURAFI", ".", FirstName)) %>%
  mutate(FirstName = if_else(LastName == "ANTILA" & FirstName == "Misella Alli Helena", "Misella", FirstName)) %>%
  mutate(LastName = if_else(LastName == "ACHONDO", "ACHONDO ANDINO", LastName)) %>%
  mutate(FirstName = if_else(LastName == "ACHONDO ANDINO", "Barbara", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "BACSKAY", "Csenge Maria", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "BESSIT", "Tamsyn", FirstName)) %>%
  mutate(LastName = ifelse(LastName == "BROSZIO VOITIER", "BROSZIO", LastName)) %>%
  mutate(FirstName = ifelse(LastName == "BROSZIO", "Pablo", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "CALVO MORENO JO", "Jossimar Orlando", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "CARDERO", "Sabrina", FirstName)) %>%
  mutate(LastName = ifelse(LastName == "CARRERES MACIA", "CARRERES", LastName)) %>%
  mutate(FirstName = ifelse(LastName == "CEMLYN JONES", "Joseph", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "BLIXT", "Landen", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "	CHRISTOPULOS", "Taylor", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "FATTA", "Addison", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "JOHNSON"& FirstName == "Madray Emmanuelle", "Madray", FirstName))
  

## Write to a new csv file
write_csv(combined_df, file.path(path_to_dir, "data_2022_2023.csv"), na = "")