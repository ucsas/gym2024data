# Before continue next step: please run all codes in "R/function.R",
# and make sure you've successfully loaded functions: get_gym_tables(), align_tables(), and transform_table()
# the prerequisite functions for defining above three are: get_bottom(), gym_table(), and remove_column_if_q()


## Central American and Caribbean Games San Salvador 2023


folder_path <- "/Users/minzefang/gym2024data/pdfs_2023/central_am"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "D_Score", "E_Score", "Penalty", "Score")

ca_ls_raw <- get_gym_tables(folder_path)
# ca_ls_raw
ca_ls <- align_tables(raw_table_list = ca_ls_raw, col_names = col_names_vt)
# ca_ls
ca_tb <- transform_table(table_list = ca_ls)
# ca_tb
write_csv(ca_tb, "cleandata/central_america.csv")  