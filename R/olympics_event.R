library(pdftools)
library(tabulizer)
library(tidyverse)

# setwd("/Users/minzefang/gym2024data")

## Baku World Cup

# set areas of pdf for table extraction
qual_other_page_area <- list(c(225, 97, 774, 497)) 
qual_last_page_area <- list(c(226, 80, 718, 487))
final_area <- list(c(226, 49, 442.69, 460))


folder_path <- "/Users/minzefang/gym2024data/pdfs_2023/olympics_event"
all_paths <- list.files(folder_path, full.names = T)
final_paths <- list.files(folder_path, pattern = "final", full.names = TRUE) |> 
  set_names(basename)
qual_paths <- setdiff(all_paths, final_paths) |>
  set_names(basename)

col_names <- c("Rank", "bib", "name", "noc", "D_Score", "E_Score", "Penalty", "Score")
col_names_vt <- c("Rank", "bib", "name", "noc", "vault", "D_Score", "E_Score", "Penalty", "Score")



# 定义一个函数，根据名称条件将matrix转换为tibble
convert_matrix_to_tibble <- function(matrix_data) {
  if (length(matrix_data) > 100) {
    tibble_data <- as_tibble(matrix_data, .name_repair = "unique")
    tibble_data <- tibble_data[1:16, ]  # 设置16行
  } else {
    tibble_data <- as_tibble(matrix_data, .name_repair = "unique")
    tibble_data <- tibble_data[1:8, ]  # 设置8行
  }
  return(tibble_data)
}


## extract tables from final results
final_ls <- final_paths |>  
  set_names(basename) |> 
  map(\(path) extract_tables(path, guess = F, area = final_area, output = "matrix")) |> 
  unlist(recursive = F, use.names = TRUE) |> 
  map(convert_matrix_to_tibble) |> 
  map(~ {
    if (ncol(.x) == 7) {
      add_column(.x, Pen = "", .after = 6)         # generate column Penalty for tibbles which has not
    } else {                                       
      .x                       
    }
  }) |> 
  map(~ {
    if (ncol(.x) == 8) {
      add_column(.x, Vault = "", .after = 4)         # generate column Vault for non-vault tibbles to make two types of tibble compatible to combine
    } else {                                         # .after=4 as we want column Vault to be after column NOC.code
      .x                       
    }
  }) |> 
  map(function(tibble_data) {
    set_names(tibble_data, col_names_vt)
  })



# define a function for extract table 
extract_tables_with_area <- function(file_path, last_page_area, other_page_area) {
  num_pages <- pdf_info(file_path)$pages
  
  if (num_pages == 1) {
    pages <- 1
    area <- last_page_area
  } else {
    pages <- c(1:(num_pages-1), num_pages)
    area <- c(rep(other_page_area, num_pages-1), last_page_area)
  }
  
  extract_tables(file_path, pages = pages, area = area, guess = FALSE, output = "matrix")
}


## extract tables from qual results

qual_ls_raw <- map(qual_paths, extract_tables_with_area, qual_last_page_area, qual_other_page_area)
  
qual_ls <- unlist(qual_ls_raw, recursive = F, use.names = TRUE) |> 
  map(~ as_tibble(.x)) |> 
  map(~ {
    if (ncol(.x) == 7) {
      add_column(.x, Pen = "", .after = 6)         # generate column Vault for non-vault tibbles to make two types of tibble compatible to combine
    } else {                                         # .after=4 as we want column Vault to be after column NOC.code
      .x                       
    }
  }) |> 
  map(~ {
    if (ncol(.x) == 8) {
      add_column(.x, Vault = "", .after = 4)         # generate column Vault for non-vault tibbles to make two types of tibble compatible to combine
    } else {                                         # .after=4 as we want column Vault to be after column NOC.code
      .x                       
    }
  }) |> 
  map(function(tibble_data) {
    set_names(tibble_data, col_names_vt)
  })




## combine final and qual

olm_ls <- c(qual_ls, final_ls) |> 
  map(~ mutate_all(.x, ~ ifelse(. == "", lag(.), .))) |> 
  map(~ mutate_at(.x, vars(c(Rank,bib)), ~ ifelse(is.na(.), lag(.), .))) |> 
  map(~ {.                           # remove "! " D columns in some tibbles and change to numerics
    .x |> 
      mutate(D_Score = as.numeric(gsub("! ", "", D_Score, fixed = TRUE)))
  })


olm_tb <- list_rbind(olm_ls, names_to = "title") |> 
  separate_wider_delim(
    title,
    delim = "_",
    names = c("Gender", "Round", "Apparatus")
  ) |> 
  mutate(Penalty = as.numeric(Penalty) * -1) |> # delete the negative sign in Penalty column
  mutate(FirstName = map_chr(str_extract_all(name, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(LastName = map_chr(str_extract_all(name, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) |> 
  mutate(Competition = "Olympic Games", Location = "Tokyo, Japan", Country = noc) |> 
  mutate(Date = case_when(
    Round == "qual" ~ "25 July 2021",
    Round == "final" ~ "1 Aug 2021",
    TRUE ~ NA_character_
  )) |> 
  mutate(Apparatus = case_when(
    vault == "1" ~ "VT1",
    vault == "2" ~ "VT2",
    TRUE ~ Apparatus
  )) |> 
  relocate(FirstName, LastName, Gender, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) |> 
  select(!bib:vault) |> 
  mutate(Score = ifelse(Score == "D", "", Score),
         Score = as.numeric(Score)) |> 
  mutate(
    Rank = as.numeric(Rank),
    E_Score = as.numeric(E_Score)
  )

write_csv(olm_tb, "cleandata/olympics_event.csv")  








