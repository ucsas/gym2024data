# Before continue next step: please run all codes in "R/function.R",
# and make sure you've successfully loaded functions: get_gym_tables(), align_tables(), and transform_table()
# the prerequisite functions for defining above three are: get_bottom(), gym_table(), and remove_column_if_q()


## Central American and Caribbean Games San Salvador 2023
ca_path <- "/Users/minzefang/gym2024data/pdfs_2023/central_am"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "E_Score","D_Score", "Penalty", "Score")

ca_ls_raw <- get_gym_tables(folder_path = ca_path)
ca_ls <- align_tables(raw_table_list = ca_ls_raw, col_names = col_names_vt)
ca_tb <- transform_table(table_list = ca_ls, 
                         Date = "24-28 Jun 2023", 
                         Competition = "Central American and Caribbean Games", 
                         Location = "San Salvador, El Salvador")
write_csv(ca_tb, "cleandata/central_america.csv")


## 2022 Senior European Championships MUNICH (GER)
eu22_path <- "/Users/minzefang/gym2024data/pdfs_2023/europe_22"
eu_ls_raw <- get_gym_tables(eu22_path)
# map(eu_ls_raw, ~ map(., ncol))
eu_ls <- align_tables(eu_ls_raw, col_names_vt)
# map(eu_ls, ncol)
eu_tb <- transform_table(table_list = eu_ls, 
                         Date = "Aug 2022", 
                         Competition = "2022 Senior European Championships ", 
                         Location = "Munich, Germany") %>% 
  mutate(Date = case_when(
    Gender == "m" ~ "18-21 Aug 2022",
    Gender == "w" ~ "11-14 Aug 2022",
    TRUE ~ Date
  ))
write_csv(eu_tb, "cleandata/european_2022.csv")
