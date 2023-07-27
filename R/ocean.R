library(tesseract)
library(tidyverse)


# oceania Continental Championships 2023

# special about oceania:
# 1. image based, used tesseract::ocr to extract text form image
# 2. sometimes decimal point are missing, adjusted in get_scores() function
# 3. sometimes space between first and last name are missing, adjusted in get_name() function
# 4. certain events are missing for some players, adjusted in male_tb_incomplete() function

# further improvement direction:
# 1. incorporate gender and missing column as argument in the function
# 2. detect missing column automatically by the location between "T" and "PLANED"/"DNS".




# define functions

get_scores <- function(vector) {
  vector1 <- unlist(str_extract_all(vector, "\\d+\\.\\d{3}|\\d{4}"))
  modified_vector <- str_replace_all(vector1, "(\\d{1})(\\d{3})", "\\1.\\2")
  return(modified_vector)
}

get_names <- function(string) {
  pattern <- "[A-Z]+\\s[A-Z][a-z]+|[A-Z]+[a-z]+"
  matches <- str_extract_all(string, pattern)
  names <- unlist(matches)
  return(names)
}

remove_before_first_upper <- function(input_string) {
  pattern <- ".*?([A-Z])"
  replacement <- "\\1"
  modified_string <- sub(pattern, replacement, input_string, perl = TRUE)
  return(modified_string)
}

get_country <- function(string) {
  pattern <- "^(.*?)(?=/)"
  first_match <- str_extract(string, pattern)
  words <- unlist(strsplit(first_match, "\\s+"))
  phrase <- paste(words, collapse = " ")
  phrase <- remove_before_first_upper(phrase)
  return(phrase)
}

male_tb <- function(vec) {
  D <- get_scores(vec[1])
  E <- get_scores(vec[2])
  tot <- get_scores(vec[4])[1:length(D)]
  name <- rep(get_names(vec[1]), 6)
  Country <- rep(get_country(vec[2]), 6)
  Apparatus <- c("FX","PH","SR","VT","PB","HB")
  tb <- data.frame(name, Country, Apparatus, D, E, tot)
  return(tb)
}

male_tb_incomplete <- function(vec, miss_index_vec) { # miss_index_vec is the column number of the missing apparatus
  D <- get_scores(vec[1])
  E <- get_scores(vec[2])
  tot <- get_scores(vec[4])[1:length(D)]
  name <- rep(get_names(vec[1]), length(D))
  Country <- rep(get_country(vec[2]), length(D))
  Apparatus <- c("FX","PH","SR","VT","PB","HB")[-miss_index_vec]
  tb <- data.frame(name, Country, Apparatus, D, E, tot)
  return(tb)
}




# import text from image

ocean_text <- ocr("/Users/minzefang/gym2024data/pdfs_2023/oceania/m_AA.pdf")

# transform text into list

ocean_ls <- strsplit(ocean_text, split = "@") %>% 
  unlist() %>% 
  `[`(3:12) %>% 
  map(~ strsplit(., "\n")[[1]])



# generate table from list

ocean_m_tb <- NULL
for (i in 1:5) {
  result <- male_tb(ocean_ls[[i]])
  if (is.null(ocean_m_tb)) {
    ocean_m_tb <- result
  } else {
    ocean_m_tb <- rbind(ocean_m_tb, result)
  }
}

p6 <- male_tb_incomplete(ocean_ls[[6]], c(5))
p7 <- male_tb_incomplete(ocean_ls[[7]], c(3,6))
p8 <- male_tb_incomplete(ocean_ls[[8]], c(4,6))
p9 <- male_tb_incomplete(ocean_ls[[9]], 1:4)
p10 <- male_tb_incomplete(ocean_ls[[10]], c(1,2,4,6))


ocean_m_tb <- rbind(ocean_m_tb, p6, p7, p8, p9, p10)



ocean_m <- ocean_m_tb %>% 
  mutate(name = gsub("THORPEHeath", "THORPE Heath", name)) %>% 
  mutate(FirstName = map_chr(str_extract_all(name, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) %>% 
  mutate(LastName = map_chr(str_extract_all(name, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " "))) %>% 
  mutate(Gender = "m", Date = "05-06 May 2023", Competition = "Oceania Continental Championships 2023", 
         Round = "aa", Location = "Carrara, Australia", Rank = NA) %>%  # from this pdf, I cannot get Placement on specified Apparatus in a given round, so I set them NA
  mutate(D_Score = as.numeric(D), E_Score = as.numeric(E), Score = as.numeric(tot)) %>% 
  mutate(Score = ifelse(Score > 100, Score - 100, Score)) %>% 
  mutate(Score = replace(Score, 23, 12.866)) %>% # Dick Ethan PB total score, read T as 1 and omitted decimal points when reading from image
  mutate(Penalty = round(D_Score + E_Score - Score, 3), Score = round(Score, 3)) %>% 
  relocate(FirstName, LastName, Gender, Country, Date, Competition, Round, Location,
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!name:tot)

write_csv(ocean_m, "cleandata/oceania.csv")
  
  
  
