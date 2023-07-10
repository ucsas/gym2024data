library(pdftools)
library(tabulizer)
library(tidyverse)

# setwd("/Users/minzefang/gym2024data")

## Doha World Cup

# set areas of pdf for table extraction
other_page_area <- list(c(166, 34.14, 767.77, 549))
last_page_area <- list(c(166, 23, 695, 549)) 


folder_path <- "/Users/minzefang/gym2024data/pdfs_2023/doha"
all_paths <- list.files(folder_path, full.names = T) |> 
  set_names(basename)

col_names <- c("Rank", "bib", "name", "noc", "D_Score", "E_Score", "Penalty", "Score")
col_names_vt <- c("Rank", "bib", "name", "noc", "vault", "D_Score", "E_Score", "Penalty", "Score", "Total")


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
  
  extract_tables(file_path, pages = pages, area = area, guess = FALSE, output = "data.frame")
}


# extract tables
doha_ls_raw <- map(all_paths, extract_tables_with_area, last_page_area, other_page_area)

doha_ls <- unlist(doha_ls_raw, recursive = F, use.names = TRUE) |> 
  map(as_tibble, .name_repair = "minimal") |> 
  map( ~ {
    if ("VaultScore" %in% colnames(.x)) { 
      .x <- .x %>% select(Rank:VaultScore) # for VT data, drop columns after VaultScore
    }
    .x
  }) |> 
  map( ~ {
    if ("X" %in% colnames(.x)) {
      .x <- .x %>% select(-Name) %>% rename(Name = X) # for m_qual_RG, drop column Name, rename column X as "Name"
    }
    .x
  }) |> 
  map(~ {
    if (ncol(.x) == 8) {
      add_column(.x, Vault = 0, .after = 4)         # generate column Vault for non-vault tibbles to make two types of tibble compatible to combine
    } else {                                         # .after=4 as we want column Vault to be after column NOC.code
      .x                       
    }
  }) |> 
  map(~ mutate_all(.x, ~ ifelse(. == "", lag(.), .))) |> 
  map(~ mutate_at(.x, vars(c(Rank,Bib)), ~ ifelse(is.na(.), lag(.), .))) |> 
  map(~ {.                           # remove "! " D columns in some tibbles and change to numerics
    .x %>%
      mutate(D = as.numeric(gsub("! ", "", D, fixed = TRUE)))
  }) |> 
  map(~ {
    if ("VaultScore" %in% colnames(.x) && any(.x$VaultScore == "DNS")) {
      .x$VaultScore <- ifelse(.x$VaultScore == "DNS", "", .x$VaultScore)
      .x$VaultScore <- as.numeric(.x$VaultScore)
    }
    .x
  }) |> 
  map(~ {
    colnames(.x)[ncol(.x)] <- "Score"
    .x
  }) |> 
  map(~ {
    if ("NOC.code" %in% colnames(.x)) {
      .x |> rename_with(~ if_else(.x == "NOC.code", "NOC", .x), contains("NOC.code"))
    } else {
      .x
    }
  })  # change column name "NOC.code" into "NOC" to allign with VT data
  


doha_tb <- list_rbind(doha_ls, names_to = "title") |>
  separate_wider_delim(
    title,
    delim = "_",
    names = c("City", "Gender", "Round", "Apparatus")
  ) |> 
  mutate(Penalty = Penalty * -1) |> 
  mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) |> # remove ".pdf" and anything after it
  mutate(Date = "1-4 Mar 2023", Competition = "World Cup", Location = "Doha, Qatar") |> 
  mutate(Country = NOC, D_Score = D, E_Score = E) |> 
  relocate(FirstName, LastName, Gender, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) |> 
  mutate(Apparatus = ifelse(Apparatus == "VT", paste0("VT", Vault), Apparatus)) |> 
  select(!City:D)

write_csv(doha_tb, "cleandata/doha.csv")


