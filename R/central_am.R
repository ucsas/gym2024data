## define functions

# remove_column_if_q() takes data.frame as inputs
# What it does: if "Q" is in the last column of a data.frame, delete the last column.
remove_column_if_q <- function(df) {
  if (any(grepl("Q", df[[ncol(df)]]))) {
    df <- df[, -ncol(df)]
  }
  return(df)
}


## Central American and Caribbean Games San Salvador 2023


folder_path <- "/Users/minzefang/gym2024data/pdfs_2023/central_am"
all_paths <- list.files(folder_path, full.names = T) %>% 
  set_names(basename)

col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "D_Score", "E_Score", "Penalty", "Score")


ca_ls_raw <- map(all_paths, gym_table)

ca_ls <- unlist(ca_ls_raw, recursive = F, use.names = TRUE) %>% 
  map(remove_column_if_q) %>%   # if "Q" is in the last column of a data.frame, delete the last column.
  map(function(df) {
    if (ncol(df) > 8) { # For VT (9 or 10 columns, delete last column: Total)
      df <- df[, -ncol(df)]
    } else { # For non-VT (7 or 8 columns) generate column Vault for convenience of combining tables
      df <- add_column(df, Vault = "", .after = 4)
    }
    return(df)
  }) %>% 
  map(function(df) {
    if (ncol(df) == 8) {
      df <- add_column(df, Penalty = "", .after = 7)
    }
    return(df)
  }) %>% 
  map(function(tibble_data) {
    set_names(tibble_data, col_names_vt)
  })

# 用来看ca_ls中数据框的列数
# lapply(ca_ls, function(df) ncol(df)) 


ca_ls <- map(ca_ls, function(df) {
  # 将前四列中的空字符串替换为 NA
  df[,1:4] <- map(df[,1:4], na_if, "")
  
  # 检查是否有任何行的前四列都是 NA
  if(any(rowSums(is.na(df[,1:4])) == 4)) {
    # 如果有，使用 fill 函数来填充缺失值
    df <- fill(df, 1:4, .direction = "downup")
  }
  return(df)
})

# 如果某一行在vault列的值为“1”，则删掉这行
ca_ls <- map(ca_ls, function(df) {
  if ("vault" %in% names(df)) {
    df <- df %>% filter(vault != "1")
  }
  return(df)
})

ca_tb <- list_rbind(ca_ls, names_to = "title") %>% 
  separate_wider_delim(
    title,
    delim = "_",
    names = c("Gender", "Round", "Apparatus")
  ) %>% 
  mutate(Penalty = as.numeric(Penalty) * -1) |> # delete the negative sign in Penalty column
  mutate(FirstName = map_chr(str_extract_all(name, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(LastName = map_chr(str_extract_all(name, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " "))) |> 
  mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) %>% 
  mutate(Date = "24-28 Jun 2023", Competition = "Central American and Caribbean Games", 
         Location = "San Salvador, El Salvador", Country = noc) %>% 
  mutate(Apparatus = ifelse(vault == "2", "VT2", Apparatus)) %>% 
  mutate(Apparatus = ifelse(Apparatus == "VT", "VT1", Apparatus)) %>% 
  relocate(FirstName, LastName, Gender, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!bib:vault) %>% 
  mutate(
    Rank = as.numeric(Rank),
    E_Score = as.numeric(E_Score)
  ) %>% 
  mutate(Score = ifelse(Score == "DNS", "", Score),
         Score = as.numeric(Score))

write_csv(ca_tb, "cleandata/central_america.csv")  

  
  




