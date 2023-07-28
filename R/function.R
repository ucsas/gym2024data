library(pdftools)
library(tabulizer)
library(tidyverse)

## define functions

# extract country abbreviation
noc <- readLines("R/noc_key.txt") %>% 
  regmatches(. , gregexpr("\\b[A-Z]+\\b", .)) %>% 
  unlist()

# Function 1: get_bottom() function
# what it does: serves as a building block of later gym_table() function
# remainder means how many unique y coordinates there is between last Noc line and last line of table
get_bottom <- function(page, y_vals, y_noc) {
  unit_chunk <- page %>% # every text between first noc and second noc
    filter(y >= y_noc[2] & y <= y_noc[3]) # used 2nd & 3rd row is for varna data 1st row has abnormal font size
  y_diff <- diff(unique(unit_chunk$y))
  
  remainder <- which.max(y_diff) - 1 # get the position that has the largest y difference than its prior raw, which is the first raw of a unit chunk
  # since we want the last row of a unit chunk, we minus 1
  bottom <- page %>% 
    filter(y == y_vals[match(y_noc[length(y_noc)], y_vals) + remainder]) %>% # the last noc row + remaining row numbers
    slice(1) %>% 
    select(y_end) %>% 
    pull()+1
  return(bottom)
}



# Function 2: gym_table() function

gym_table <- function(file_path, output = "data.frame"){
  page_data <- pdf_data(file_path)
  pdf_tables <- list()
  
  for (i in 1:length(page_data)) {
    page <- page_data[[i]] %>% 
      arrange(y,x) %>% 
      mutate(x_right = x + width, y_end = y + height)
    y_vals <- sort(unique(page$y))
    y_noc <- page %>%  
      filter(text %in% noc) %>%
      filter(abs(x - median(x)) <= 5) %>%  # in case other text coincide with NOC abbr.
      pull(y)
    if (length(y_noc) == 0) {
      warning("No valid input in this page")
      break
    }
    chunk <- page %>% # every text between first noc and last noc
      filter(y >= y_noc[1] & y<= y_noc[length(y_noc)])
    
    top <- y_noc[1] - 2 # -2 is to make it compatible for aa results
    left <- chunk %>% 
      arrange(x) %>% 
      pull(x) %>% 
      first()
    right <- chunk %>%  
      arrange(x_right) %>% 
      pull(x_right) %>% 
      last() + 1
    
    bottom <- get_bottom(page = page, y_vals = y_vals, y_noc = y_noc)
    
    page_area <- list(c(top, left, bottom, right))
    page_table <- extract_tables(file_path, pages = i, guess = F, area = page_area)
    pdf_tables <- c(pdf_tables, page_table)
  }
  
  if (output == "matrix") {
    return(pdf_tables)
  } else if (output == "data.frame") {
    pdf_tables <- lapply(pdf_tables, as.data.frame)
    return(pdf_tables)
  } else {
    warning("Invalid value for 'output' argument. Using default: 'matrix'")
    return(mdf_tables)
  }
}



# Function 3: remove_column_if_q()
# What it does: if "Q" is in the last column of a data.frame, delete the last column.
remove_column_if_q <- function(df) {
  if (any(grepl("Q|R", df[[ncol(df)]]))) {
    df <- df[, -ncol(df)]
  }
  return(df)
}



# Function 4: get_gym_tables()
# what it does: taking the path of a folder containing the required PDF files as input,
# and batch using the gym_table() function on all PDF files in the folder.

get_gym_tables <- function(folder_path) {
  all_paths <- list.files(folder_path, full.names = T) %>% 
    set_names(basename)
  raw_table_list <- map(all_paths, gym_table)
  return(raw_table_list)
}
  
  

# Function 5: align_tables()
# what it does: taking the output of function get_gym_tables() and the column names 
# of the original table as input, it removes redundant columns, 
# adds missing columns, and ensures that all data frames in the list are ready for row_binding.
align_tables <- function(raw_table_list, col_names) {  
  ca_ls <- unlist(raw_table_list, recursive = F, use.names = TRUE) %>% 
    map(remove_column_if_q) %>%
    map(function(df) {
      if (ncol(df) > 8) { 
        df <- df[, -ncol(df)]
      } else { 
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
      set_names(tibble_data, col_names)
    })
  
  ca_ls <- map(ca_ls, function(df) {
    df[,1:4] <- map(df[,1:4], na_if, "")
    if(any(rowSums(is.na(df[,1:4])) == 4)) {
      df <- fill(df, 1:4, .direction = "downup")
    }
    return(df)
  })
  
  ca_ls <- map(ca_ls, function(df) {
    if ("vault" %in% names(df)) {
      df <- df %>% filter(vault != "1")
    }
    return(df)
  })
  
  return(ca_ls)
}


# Function 6: transform_table()
# what it does: take the output of the function align_tables() as input,
# row_bind all tables in the list, add new columns, remove redundant columns, generate the final table outputs as the format we need
transform_table <- function(table_list, Date, Competition, Location) {
  competition_tb <- list_rbind(table_list, names_to = "title") %>% 
    separate_wider_delim(
      title,
      delim = "_",
      names = c("Gender", "Round", "Apparatus")
    ) %>% 
    mutate(Penalty = as.numeric(Penalty),
           Penalty = ifelse(Penalty < 0, -Penalty, Penalty)) %>% 
    mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) |> 
    mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " "))) |> 
    mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) %>% 
    mutate(Date = Date, Competition = Competition, 
           Location = Location, Country = NOC) %>% 
    mutate(Apparatus = ifelse(vault == "2", "VT2", Apparatus)) %>% 
    mutate(Apparatus = ifelse(Apparatus == "VT", "VT1", Apparatus)) %>% 
    relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, Location, 
             Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
    select(!Bib:vault) %>% 
    mutate(D_Score = ifelse(str_detect(D_Score, "! "), gsub("! ", "", D_Score, fixed = TRUE), D_Score)) %>%  # remove "! " D columns in some tibbles and change to numerics
    mutate(
      Rank = as.numeric(Rank),
      E_Score = as.numeric(E_Score),
      D_Score = as.numeric(D_Score)
    ) %>% 
    mutate(Score = ifelse(Score %in% c("DNS", "DN"), "", Score),
           Score = as.numeric(Score))
  
  return(competition_tb)
}


### Functions for Varna
process_df <- function(df) {
  df <- df[, -c((ncol(df)-1), ncol(df))] # 删除最后两列
  df <- df %>% mutate(Vault = ifelse(row_number() %% 2 == 0, "2", "1")) 
  df <- df %>% select(1:4, Vault, everything()) # 将 Vault 列移动到第四列后面
  return(df)
}

remove_empty_rows <- function(df) {
  df <- df %>% filter(D_Score != "")
  return(df)
}

align_tables_vn <- function(raw_table_list, col_names) {  
  ca_ls <- raw_table_list %>% 
    map(remove_column_if_q) %>%
    map(function(df) {
      if (ncol(df) > 8) { 
        df <- df
      } else { 
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
      set_names(tibble_data, col_names)
    })
  
  ca_ls <- map(ca_ls, function(df) {
    df[,1:4] <- map(df[,1:4], na_if, "")
    if(any(rowSums(is.na(df[,1:4])) == 4)) {
      df <- fill(df, 1:4, .direction = "downup")
    }
    return(df)
  }) %>% 
    map(remove_empty_rows)
  return(ca_ls)
}

split_column_vn <- function(df) {
  if(ncol(df) == 7) {
    df <- df %>% 
      separate(col = 2, into = c("first_part", "rest_parts"), sep = " ", extra = "merge") %>%
      select(1, first_part, rest_parts, everything())
  }
  return(df)
}





