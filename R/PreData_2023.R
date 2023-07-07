library(pdftools)
library(tabulizer)
library(tidyverse)

setwd("/Users/minzefang/gym2024data/pdfs_2023")

## Cairo Non-Vault


# set areas of pdf for table extraction

area_1 <- list(c(124, 8.9, 810, 585))   # Page 1: Let top=124 to include the column name
area_2 <- list(c(73, 9, 810, 585)) # Page 2



# get file paths for each PDF
paths <- list.files("/Users/minzefang/Downloads/gym2024data/pdfs_2023/cairo", full.names = T)

col_names <- c("Rank", "bib", "name", "noc", "D_Score", "E_Score", "Penalty", "Score")


# extract tables from the 1st page of each PDF
p1 <- paths |> 
  set_names(basename) |>
  map(\(path) extract_tables(path, pages = 1, guess = F, area = area_1, output = "data.frame")) |> 
  
  # unlist the outer layer of list
  map(~ unlist(.x, recursive = F)) |>  
  
  # transform as tibbles
  map(as_tibble, .name_repair = "minimal") |> 
  
  # remove redundant column
  map(~ select(.x, -c("Rank", "BIB", "D.Score", "E.Score", "PEN", "Total"))) |> 
  
  # remove the additional column of Q/P result for qualification round
  map(~ {                                    
    if ("X.6" %in% colnames(.x)) {                 
      select(.x, -X.6)
    } else {
      .x
    }
  }) 

p1 <- map(p1, ~ if ("X.5" %in% colnames(.x) && "Q" %in% .x$X.5) {
    .x %>% select(-X.5)
  } else {
    .x
  }) |> 
  
  # add Penalty column for tables which does not contain
  map(~ {
    if (ncol(.x) == 7) {
      mutate(.x, PEN = NA) |> 
        relocate(PEN, .after = "X.3")              
    } else {
      .x
    }
  })


# create a logical value vector indicating which PDF contains 2 pages
one_pages <- map_lgl(paths, ~ pdf_info(.x)$pages == 1)


# extract tables from the 2nd page of each PDF
p2 <- paths |> 
  set_names(basename) |> 
  map_if(!one_pages, \(path) extract_tables(path, pages = 2, guess = F, area = area_2)) 
p2 <- discard(unlist(p2, recursive = F), one_pages)
p2 <- map(p2, as_tibble)
p2 <- map(p2, ~ mutate_at(.x, vars(-V3, -V4), as.numeric)) |> 
  map(~ {                                    
    if ("V8" %in% colnames(.x)) {                 
      select(.x, -V8)
    } else {
      .x
    }
  }) |> 
  map(~ {
    if (ncol(.x) == 7) {
      mutate(.x, PEN = NA) |> 
        relocate(PEN, .after = "V6")              
    } else {
      .x
    }
  })



# merge two lists p1 and p2

all <- c(p1, p2) |> 
  map(~ rename_all(.x, ~ col_names)) |>   # rename columns as what they are
  list_rbind(names_to = "title") |> 
  separate_wider_delim(
    title,
    delim = "_",
    names = c("City", "Gender", "Round", "Apparatus")
  ) |> 
  mutate(FirstName = map_chr(str_extract_all(name, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(LastName = map_chr(str_extract_all(name, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(Apparatus = str_remove(Apparatus, "\\.pdf$")) |> 
  mutate(Date = "27–30 Apr 2023", Competition = "World Cup", Location = "Cairo, Egypt") |> 
  mutate(Country = noc) |> 
  relocate(FirstName, LastName, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score )|> 
  select(!bib:noc)

write_csv(all, "/Users/minzefang/gym2024data/cleandata/cairo_data.csv")

























