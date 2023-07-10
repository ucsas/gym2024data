# Cottbus World Cup

area <- list(c(147, 53, 739, 568))

folder_path <- "pdfs_2023/cottbus"
all_paths <- list.files(folder_path, full.names = T)
vt_paths <- list.files(folder_path, pattern = "VT", full.names = TRUE) |> 
  set_names(basename)
non_vt_paths <-setdiff(all_paths, vt_paths) |>  # paths for all non-VT files
  set_names(basename)

col_names <- c("Rank", "bib", "Name", "Country", "D_Score", "E_Score", "Penalty", "Score")


# First, for Vault data
vt_ls <- map(vt_paths, ~ extract_tables(file = .x, guess = F, area = area, output = "data.frame")) 
vt_tb <- lapply(vt_ls, function(sublist) {
  bind_rows(sublist)
}) |> 
  list_rbind(names_to = "title")

vt_1 <- vt_tb |> 
  filter(row_number() %% 3 == 1) |> 
  select(1:5) |> # select from column title to NAT
  mutate(id = row_number(), .before = 1)

vt_2 <- vt_tb |> 
  filter(row_number() %% 3 == 2) |> 
  select(6:9) |>  # select from column D to Total
  rename(D_VT1 = D, E_VT1 = E, Pen_VT1 = Pen, Total_VT1 = Total) |> 
  mutate(id = row_number(), .before = 1) # using row numbers as numeric surrogate key

vt_3 <- vt_tb |> 
  filter(row_number() %% 3 == 0) %>%
  select(6:9) |> 
  rename(D_VT2 = D, E_VT2 = E, Pen_VT2 = Pen, Total_VT2 = Total) |> 
  mutate(id = row_number(), .before = 1)

vt_all <- vt_1 |> 
  left_join(vt_2, join_by(id)) |> 
  left_join(vt_3, join_by(id)) |> 
  pivot_longer(
    cols = D_VT1:Total_VT2,
    names_to = c(".value", "Apparatus"), 
    names_sep = "_"
  ) |> 
  separate_wider_delim(
    title,
    delim = "_",
    names = c("City", "Gender", "Round", "Redundant")
  ) |> 
  select(!c(id, Redundant))


# Second, for non-Vault data

non_vt_ls <- map(non_vt_paths, ~ extract_tables(file = .x, guess = F, area = area, output = "data.frame")) 

non_vt_tb <- lapply(non_vt_ls, function(sublist) {
  bind_rows(sublist)
}) |> 
  list_rbind(names_to = "title") |> 
  separate_wider_delim(
    title,
    delim = "_",
    names = c("City", "Gender", "Round", "Apparatus")
  ) |> 
  mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) |> # remove ".pdf" and anything after it
  relocate(City, Gender, Round, Rank, BIB, NAME, NAT, 
           Apparatus, D, E, Pen, Total )


# Combine VT and non-VT tibbles

cottbus_all <- bind_rows(non_vt_tb, vt_all) |> 
  mutate(D = as.numeric(str_replace_all(D, ",", ".")),
         E = as.numeric(str_replace_all(E, ",", ".")),
         Pen = as.numeric(str_replace_all(Pen, ",", ".")),
         Total = as.numeric(str_replace_all(Total, ",", "."))) |> 
  mutate(FirstName = map_chr(str_extract_all(NAME, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(LastName = map_chr(str_extract_all(NAME, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(Date = "23-26 Feb 2023", Competition = "World Cup", Location = "Cottbus, Germany") |> 
  mutate(Country = NAT, D_Score = D, E_Score = E, Penalty = Pen, Score = Total) |> 
  relocate(FirstName, LastName, Gender, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) |> 
  select(!City:Total)

write_csv(cottbus_all, "cleandata/cottbus.csv")  




