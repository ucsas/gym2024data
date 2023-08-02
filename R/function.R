library(pdftools)
library(tabulizer)
library(tidyverse)
library(rvest)

## define functions

# extract country abbreviation
noc <- readLines("noc_key.txt") %>% 
  regmatches(. , gregexpr("\\b[A-Z]+\\b", .)) %>% 
  unlist()

# Function 1: get_bottom() function
# what it does: serves as a building block of later gym_table() function
# remainder means how many unique y coordinates there is between last Noc line 
# and last line of table
get_bottom <- function(page, y_vals, y_noc) {
  last_unit_chunk <- page %>%  # every text after the last noc line
    filter(y >= y_noc[length(y_noc)]) 
  y_diff <- diff(unique(last_unit_chunk$y)) 
  # get the position that has the largest y difference than its prior raw, 
  # which is the first raw after the last row of a table
  remainder <- which.max(y_diff) - 1 
  # since we want the last row of a unit chunk, we minus 1
  bottom <- page %>% 
    # the last noc row + remaining row numbers
    filter(y == y_vals[match(y_noc[length(y_noc)], y_vals) + remainder]) %>% 
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
      # in case other text coincide with NOC abbr.
      filter(abs(x - median(x)) <= 5) %>%  
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
    page_table <- extract_tables(file_path, pages = i, guess = F, 
                                 area = page_area)
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
# What it does: if "Q" is in the last column of a data.frame, 
# delete the last column.
remove_column_if_q <- function(df) {
  if (any(grepl("Q|R", df[[ncol(df)]]))) {
    df <- df[, -ncol(df)]
  }
  return(df)
}



# Function 4: get_gym_tables()
# what it does: taking the path of a folder containing the required PDF files as
# input, and batch using the gym_table() function on all PDF files in the folder.

get_gym_tables <- function(folder_path) {
  all_paths <- list.files(folder_path, full.names = T) %>% 
    set_names(basename)
  raw_table_list <- map(all_paths, gym_table)
  return(raw_table_list)
}
  
  

# Function 5: align_tables()
# what it does: taking the output of function get_gym_tables() and the column 
# names of the original table as input, it removes redundant columns, 
# adds missing columns, and ensures that all data frames in the list are ready 
# for row_binding.
align_tables <- function(raw_table_list, col_names) {  
  ca_ls <- raw_table_list %>% 
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
# row_bind all tables in the list, add new columns, remove redundant columns, 
# generate the final table outputs as the format we need
transform_table <- function(table_list, Date, Competition, Location) {
  competition_tb <- list_rbind(table_list, names_to = "title") %>% 
    separate_wider_delim(
      title,
      delim = "_",
      names = c("Gender", "Round", "Apparatus")
    ) %>% 
    mutate(Penalty = as.numeric(Penalty),
           Penalty = ifelse(Penalty < 0, -Penalty, Penalty)) %>% 
    mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), 
                               ~ paste(.x, collapse = " "))) %>% 
    mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), 
                              ~ paste(.x, collapse = " "))) %>% 
    mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) %>% 
    mutate(Date = Date, Competition = Competition, 
           Location = Location, Country = NOC) %>% 
    mutate(Apparatus = ifelse(vault == "2", "VT2", Apparatus)) %>% 
    mutate(Apparatus = ifelse(Apparatus == "VT", "VT1", Apparatus)) %>% 
    relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
             Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
    select(!Bib:vault) %>% 
    # remove "! " D columns in some tibbles and change to numerics
    mutate(D_Score = ifelse(str_detect(D_Score, "! "), 
                            gsub("! ", "", D_Score, fixed = TRUE), D_Score)) %>%  
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
  df <- df[, -c((ncol(df)-1), ncol(df))] # delete last two column
  df <- df %>% mutate(Vault = ifelse(row_number() %% 2 == 0, "2", "1")) 
  df <- df %>% select(1:4, Vault, everything()) # move Vault after column 4
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
      separate(col = 2, into = c("first_part", "rest_parts"), sep = " ", 
               extra = "merge") %>%
      select(1, first_part, rest_parts, everything())
  }
  return(df)
}


### Functions for Web Scraping: Tel Aviv & Osijek World Challenge Cup

get_web_tb <- function(url, gender) {
  if (gender == "m") {
    round <- c(rep("final",6), rep("qual",6))
    appar <- rep(c("FX", "PH", "SR", "VT", "PB", "HB"),2)
  } else if (gender == "w") {
    round <- c(rep("final",4), rep("qual",4))
    appar <- rep(c("VT", "UB", "BB", "FX"),2)
  } else {
    stop("Invalid gender. Please use 'm' or 'w'.")
  }
  
  webpage <- read_html(url)
  
  tables_list <- html_table(webpage, fill = TRUE, header = T)
  
  # 对每个数据框添加新列Round
  tables_list <- lapply(1:length(tables_list), function(i) {
    tables_list[[i]]$Round <- round[i]
    return(tables_list[[i]])
  })
  
  # 对每个数据框添加新列Apparatus
  tables <- lapply(1:length(tables_list), function(i) {
    tables_list[[i]]$Apparatus <- appar[i]
    return(tables_list[[i]])
  }) %>% 
    lapply(function(df) {
      df$Gender <- gender
      return(df)
    })
  
  return(tables)
}


update_vt <- function(table_list) {
  table_list <- lapply(table_list, function(df) {
    if ("VT" %in% df$Apparatus) {
      vt_rows <- which(df$Apparatus == "VT")
      df$Apparatus[vt_rows] <- ifelse(vt_rows %% 2 == 1, "VT1", "VT2")
    }
    return(df)
  })
  return(table_list)
}

transform_web_tb <- function(table_list, Date, Competition, Location) {
  tel_tb <- list_rbind(table_list)
  merged_tb <- merge(tel_tb, result_df, by.x = "Nation", by.y = "Full_Name", 
                     all.x = TRUE)
  
  tel_tb <- merged_tb %>% 
    mutate(Penalty = ND, D_Score = D, E_Score = E, Country = Country_Abbr, 
           Score = Total,
           Date = Date, Competition = Competition, Location = Location,
           Penalty = ifelse(Penalty < 0, -Penalty, Penalty)) %>% 
    mutate(LastName = ifelse(Nation == "Hong Kong", 
                             str_split_fixed(Athlete, " ", 2)[,1],
                             word(Athlete, -1)),
           FirstName = ifelse(Nation == "Hong Kong",
                              str_split_fixed(Athlete, " ", 2)[,2],
                              str_replace(Athlete, LastName, ""))) %>% 
    relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
             Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
    select(!Athlete:Country_Abbr)
  
  return(tel_tb)
}

convert_dns_or_dash_to_empty_string <- function(df) {
  df <- df %>% mutate(
    Rank = ifelse(Rank %in% c("DNS", "–"), "", Rank),
    D = ifelse(D %in% c("DNS", "–"), "", D),
    E = ifelse(E %in% c("DNS", "–"), "", E),
    Total = ifelse(Total %in% c("DNS", "–"), "", Total)
  )
  return(df)
}


### Functions for Cottbus and EnBW DTB Pokal
### characteristics: For VT, each player has 3 line; info for each gymnast can
### be divided into two pages.

extract_data_cot <- function(folder_path, area) {
  all_paths <- list.files(folder_path, full.names = T)
  vt_paths <- list.files(folder_path, pattern = "VT", full.names = TRUE) |> 
    set_names(basename)
  non_vt_paths <-setdiff(all_paths, vt_paths) |>  # paths for all non-VT files
    set_names(basename)
  
  # First, for Vault data
  vt_ls <- map(vt_paths, ~ extract_tables(file = .x, guess = F, area = area, 
                                          output = "data.frame")) 
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
      names = c("Gender", "Round", "appar")
    ) |>
    select(!c(id, appar))

  # Second, for non-Vault data
  non_vt_ls <- map(non_vt_paths, ~ extract_tables(file = .x, guess = F, 
                                                  area = area, 
                                                  output = "data.frame")) 
  non_vt_tb <- lapply(non_vt_ls, function(sublist) {
    bind_rows(sublist)
  }) |> 
    list_rbind(names_to = "title") |> 
    separate_wider_delim(
      title,
      delim = "_",
      names = c("Gender", "Round", "Apparatus")
    ) |>
    # remove ".pdf" and anything after it
    mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) |> 
    relocate(Gender, Round, Rank, BIB, NAME, NAT,
             Apparatus, D, E, Pen, Total )
  
  cottbus_all <- bind_rows(non_vt_tb, vt_all)
  return(cottbus_all)
}


process_data_cot <- function(data_frame, type, Date, Competition, Location) {
  processed_data <- data_frame |> 
    mutate(D = as.numeric(str_replace_all(D, ",", ".")),
           E = as.numeric(str_replace_all(E, ",", ".")),
           Pen = as.numeric(str_replace_all(Pen, ",", ".")),
           Total = as.numeric(str_replace_all(Total, ",", ".")))
  
  if (type == "Cottbus") {
    processed_data <- processed_data |> 
      mutate(FirstName = map_chr(str_extract_all(NAME, "\\b[A-Z][a-z]+\\b"), ~ paste(.x, collapse = " "))) |> 
      mutate(LastName = map_chr(str_extract_all(NAME, "\\b[A-Z]+\\b"), ~ paste(.x, collapse = " ")))
  } else if (type == "dtb") {
    processed_data <- processed_data |> 
      mutate(
        split_name = str_split_fixed(NAME, ",", n = 2),
        LastName = split_name[, 1],
        FirstName = split_name[, 2]
      ) %>% 
      select(-split_name)
  }
  
  processed_data <- processed_data |> 
    mutate(Date = Date, Competition = Competition, Location = Location) |> 
    mutate(Country = NAT, D_Score = D, E_Score = E, Penalty = Pen, Score = Total) |> 
    relocate(FirstName, LastName, Gender, Country, Date, Competition, Round, Location, 
             Apparatus, Rank, D_Score, E_Score, Penalty, Score ) |> 
    select(!BIB:Total)
  
  return(processed_data)
}

