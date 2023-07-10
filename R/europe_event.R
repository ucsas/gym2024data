library(pdftools)
library(tabulizer)
library(tidyverse)

# setwd("/Users/minzefang/gym2024data")

## Baku World Cup

# set areas of pdf for table extraction
other_page_area <- list(c(164.46, 34.58, 768, 551)) # longer area non-last page for pdfs more than single page, also short in right to avoid Q/P
last_page_area <- list(c(166.88, 36.06, 695, 550))  # shorter area for the last pages, 
# since they have note & legend at the bottom.

folder_path <- "/Users/minzefang/gym2024data/pdfs_2023/europe_event"
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

eu_ls_raw <- map(all_paths, extract_tables_with_area, last_page_area, other_page_area)

eu_ls <- unlist(eu_ls_raw, recursive = F, use.names = TRUE) |> 
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

  map(~ mutate_all(.x, ~ ifelse(. == "", lag(.), .))) |> 
  map(~ {
    if ("Vault" %in% colnames(.x)) {
      .x %>%
        mutate_at(vars(Rank, Bib), ~ ifelse(is.na(.), lag(.), .))
    } else {
      .x
    }
  }) |> 
  
  map(~ {
    if (ncol(.x) == 8) {
      add_column(.x, Vault = 0, .after = 4)         # generate column Vault for non-vault tibbles to make two types of tibble compatible to combine
    } else {                                         # .after=4 as we want column Vault to be after column NOC.code
      .x                       
    }
  }) |> 
  
  map(~ {.                           # remove "! " D columns in some tibbles and change to numerics
    .x |> 
      mutate(D = as.numeric(gsub("! ", "", D, fixed = TRUE)))
  }) |> 
  map( ~ {
    colnames(.x)[ncol(.x)] <- "Score" # Rename column VaultScore as "Score" for VT tibbles
    .x
  }) |> 
  map(~ {
    if (any(.x$Score == "DNS")) {
      .x$Score <- ifelse(.x$Score == "DNS", "", .x$Score)
      .x$Score <- as.numeric(.x$Score)
    }
    .x
  }) |> 
  map(~ {
    if ("NOC.code" %in% colnames(.x)) {
      .x |> rename_with(~ if_else(.x == "NOC.code", "NOC", .x), contains("NOC.code"))
    } else {
      .x
    }
  }) |> 
  discard( ~ nrow(.x) == 0) # delete tibbles with 0 rows


eu_tb <- list_rbind(eu_ls, names_to = "title") |>
  separate_wider_delim(
    title,
    delim = "_",
    names = c("Gender", "Round", "Apparatus")
  ) |> 
  mutate(Penalty = Penalty * -1) |> # delete the negative sign in Penalty column
  mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) |> 
  mutate(Date = "11-16 Apr 2023", Competition = "10th EUROPEAN ARTISTIC GYMNASTICS CHAMPIONSHIPS", Location = "Antalya, Turkey") |> 
  mutate(Country = NOC, D_Score = D, E_Score = E) |> 
  relocate(FirstName, LastName, Gender, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) |> 
  mutate(Apparatus = ifelse(Apparatus == "VT", paste0("VT", Vault), Apparatus)) |> 
  select(!Bib:D)

write_csv(eu_tb, "cleandata/europe_event.csv")






