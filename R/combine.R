## Load necessary libraries
library(tidyverse)
library(rlang)

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
  mutate(Apparatus = ifelse(Apparatus == "RG", "SR", Apparatus)) %>% 
  filter(!is.na(D_Score) & !is.na(E_Score) & !is.na(Score)) %>%  # 删除combined_df数据框中D_Score、E_Score和Score任一列为NA的行
  filter(!Score == 0) %>% 
  distinct() # 删除完全相同的行

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
  mutate(FirstName = ifelse(LastName == "JOHNSON"& FirstName == "Madray Emmanuelle", "Madray", FirstName)) %>% 
  mutate(FirstName = ifelse(LastName == "ABIYURAFI", "", FirstName)) %>% 
  mutate(
    FirstName = ifelse(LastName == "VANESSA", "Vanessa", FirstName),
    LastName = ifelse(LastName == "VANESSA", "WONG", LastName)
  ) %>% 
  mutate(
    FirstName = ifelse(LastName == "KAKERU", "Kakeru", FirstName),
    LastName = ifelse(LastName == "KAKERU", "TANIGAWA", LastName)
  ) %>% 
  mutate(
    FirstName = ifelse(LastName == "SHOHEI", "Shohei", FirstName),
    LastName = ifelse(LastName == "SHOHEI", "KAWAKAMI", LastName)
  ) %>% 
  mutate(
    FirstName = ifelse(LastName == "TAKERU", "Takeru", FirstName),
    LastName = ifelse(LastName == "TAKERU", "KITAZONO", LastName)
  ) %>% 
  mutate(
    FirstName = ifelse(LastName == "WATARU", "Wataru", FirstName),
    LastName = ifelse(LastName == "WATARU", "TANIGAWA", LastName)
  ) %>% 
  mutate(FirstName = ifelse(LastName == "YONEKURA", "Hidenobu", FirstName)) %>% 
  mutate(FirstName = ifelse(LastName == "JESUS SANT", "Melanie", FirstName)) %>% 
  mutate(LastName = ifelse(LastName == "JESUS SANT", "DE JESUS DOS SANTOS", LastName)) %>% 
  mutate(LastName = ifelse(LastName == "JESUS SANTOS", "DE JESUS DOS SANTOS", LastName)) %>% 
  mutate(FirstName = ifelse(LastName == "ELPITIYA BADALG D", "Milka Gehani", FirstName)) %>% 
  mutate(Gender = ifelse(LastName == "ELPITIYA BADALG D", "w", Gender)) %>% 
  mutate(LastName = ifelse(LastName == "ELPITIYA BADALG D", "ELPITIYA BADALGE DONA", LastName)) %>% 
  mutate(
    FirstName = ifelse(LastName == "SOUZA BITENCOU", "Lucas", FirstName),
    LastName = ifelse(LastName == "SOUZA BITENCOU", "DE SOUZA BITENCOURT", LastName)
  ) %>% 
  mutate(LastName = ifelse(LastName == "SOUZA BITENCOURT", "DE SOUZA BITENCOURT", LastName)) %>% 
  mutate(
    FirstName = ifelse(LastName == "KUMARASINGHEGE HG", "Hansa Gayashan", FirstName),
    LastName = ifelse(LastName == "KUMARASINGHEGE HG", "KUMARASINGHEGE", LastName)
  ) %>% 
  mutate(FirstName = ifelse(LastName == "NOURISHOURAKCHALI", "Hossein", FirstName)) %>% 
  mutate(
    FirstName = ifelse(LastName == "AQUINO III", "Miguel", FirstName),
    LastName = ifelse(LastName == "AQUINO III", "AQUINO", LastName),
    Country = ifelse(LastName == "AQUINO", "PUR", Country)
  ) %>% 
  mutate(
    Country = ifelse(LastName == "DARIES", "RSA", Country),
    Country = ifelse(LastName == "SIMONOV", "AZE", Country)
  ) %>% 
  mutate(FirstName = ifelse(LastName == "BERNARD", "Cameron-Lie", FirstName)) %>% 
  mutate(FirstName = sub(" Guest$", "", FirstName)) %>% 
  mutate(LastName = ifelse(FirstName == "Jorden", "OCONNELL-INNS", LastName)) %>% 
  mutate(LastName = ifelse(FirstName == "Derin", "TANRIYASUKUR", LastName)) %>% 
  mutate(LastName = ifelse(FirstName == "Jimi", "PAEIVAENEN", LastName)) %>% 
  mutate(LastName = ifelse(FirstName == "Akhmejanov ", "PAEIVAENEN", LastName)) %>% 
  mutate(LastName = ifelse(FirstName == "Jimi", "PAEIVAENEN", LastName)) %>% 
  mutate(FirstName = ifelse(LastName == "AKHMEJANOV", "Emil", FirstName))

# replace all non-English letters
replacement_map <- c("Ã" = "A", "Ü" = "UE", "Ä" = "AE", "Ó" = "O", 
                     "Í" = "I", "Ö" = "OE", "Ø" = "OE", "Ê" = "E", 
                     "É" = "E", "Ñ" = "N", "O’" = "O", "Á" = "A",
                     "ù" = "u", "é"="e", "ö"="o", "á"="a")
combined_df <- combined_df %>%
  mutate(LastName = Reduce(function(x, y) gsub(y, replacement_map[[y]], x), 
                           names(replacement_map), init = LastName)) %>% 
  mutate(FirstName = Reduce(function(x, y) gsub(y, replacement_map[[y]], x), 
                           names(replacement_map), init = FirstName))
# quick way to check
# non_english_rows_LastName <- combined_df[grepl("[^\x01-\x7F]", combined_df$LastName), ]
# non_eng_rows_FirstName <- combined_df[grepl("[^\x01-\x7F]", combined_df$FirstName), ]


## Write to a new csv file
write_csv(combined_df, file.path(path_to_dir, "data_2022_2023.csv"), na = "")