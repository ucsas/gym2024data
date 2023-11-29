library(pdftools)
library(tabulizer)
library(tidyverse)
library(rvest)
library(rlang)

### Functions for general type pdfs ############################################

## extract country abbreviation
noc <- readLines("noc_key.txt") %>% 
  regmatches(. , gregexpr("\\b[A-Z]+\\b", .)) %>% 
  unlist()

##' Function 1: get_bottom() function
##' What it does: Serves as a building block of the later gym_table() function.
##' @param page: Data frame containing extracted text from a PDF page.
##' @param y_vals: Numeric vector representing y-coordinates of text elements.
##' @param y_noc: Numeric vector representing y-coordinates of NOC lines.
##' @return: An integer representing the bottom index of a unit chunk.
get_bottom <- function(page, y_vals, y_noc) {
  last_unit_chunk <- page %>%  # every text after the last noc line
    filter(y >= y_noc[length(y_noc)]) 
  y_diff <- diff(unique(last_unit_chunk$y)) 
  ## get the position that has the largest y difference than its prior raw, 
  ## which is the first raw after the last row of a table
  remainder <- which.max(y_diff) - 1 
  ## since we want the last row of a unit chunk, we minus 1
  bottom <- page %>% 
    ## the last noc row + remaining row numbers
    filter(y == y_vals[match(y_noc[length(y_noc)], y_vals) + remainder]) %>% 
    slice(1) %>% 
    select(y_end) %>% 
    pull()+1
  return(bottom)
}



## Function 2: gym_table() function

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
      ## in case other text coincide with NOC abbr.
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
    return(pdf_tables)
  }
}



## Function 3: remove_column_if_q(), remove_qr_from_total()

## if "Q" is in the last column of a data.frame, delete the last column.
remove_column_if_q <- function(df) {
  if (any(grepl("Q|R", df[[ncol(df)]]))) {
    df <- df[, -ncol(df)]
  }
  return(df)
}

## if "Q" is in the last column of a data.frame, 删掉最后一列空格后面的部分
remove_qr_from_total <- function(df) {
  # 判断最后一列中是否有值包含 "Q" 或 "R"
  if(any(grepl("Q|R", df[[ncol(df)]]))) {
    df[[ncol(df)]] <- sub("\\s.*", "", df[[ncol(df)]]) 
  }
  return(df)
}


## Function 4: get_gym_tables()
## what it does: taking the path of a folder containing the required PDF files as
## input, and batch using the gym_table() function on all PDF files in the folder.

get_gym_tables <- function(folder_path) {
  all_paths <- list.files(folder_path, full.names = T) %>% 
    set_names(basename)
  raw_table_list <- map(all_paths, gym_table)
  return(raw_table_list)
}
  
  

## Function 5: align_tables()
## what it does: taking the output of function get_gym_tables() and the column 
## names of the original table as input, it removes redundant columns, 
## adds missing columns, and ensures that all data frames in the list are ready 
## for row_binding.
align_tables <- function(raw_table_list, col_names) {  
  ca_ls <- raw_table_list %>% 
    map(remove_column_if_q) %>% # 删除qual表格中最后一列为Q/R的列
    map(function(df) { #如果此时大于8列，说明是VT，则删掉最后一列
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
  
  # ca_ls <- map(ca_ls, function(df) {
  #   if ("vault" %in% names(df)) {
  #     df <- df %>% filter(vault != "1")
  #   }
  #   return(df)
  # })
  
  return(ca_ls)
}


## Function 6: transform_table()
## what it does: take the output of the function align_tables() as input,
## row_bind all tables in the list, add new columns, remove redundant columns, 
## generate the final table outputs as the format we need
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
    ## remove "! " D columns in some tibbles and change to numerics
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


### Functions for Varna type pdfs ##############################################

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


### Functions for Web Scraping: Tel Aviv & Osijek World Challenge Cup ##########

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
  
  ## add new column Round for each df
  tables_list <- lapply(1:length(tables_list), function(i) {
    tables_list[[i]]$Round <- round[i]
    return(tables_list[[i]])
  })
  
  ## add new column Apparatus for each df
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

# 构造process_web_tb_ls函数，输入数据框list（和性别参数），返回增加Round和Gender的数据框list
# 是get_web_tb函数的简化修改版，去掉了直接从网页链接获取数据框列表的步骤
# 应用于该网页上包含不止所需要的数据框的情况，需预先把那些表格从列表中删掉，
# 参考2022 Asian championship的处理
process_web_tb_ls <- function(tables_list, gender) {
  if (gender == "m") {
    round <- c(rep("final",6), rep("qual",6))
    appar <- rep(c("FX", "PH", "SR", "VT", "PB", "HB"),2)
  } else if (gender == "w") {
    round <- c(rep("final",4), rep("qual",4))
    appar <- rep(c("VT", "UB", "BB", "FX"),2)
  } else {
    stop("Invalid gender. Please use 'm' or 'w'.")
  }
  
  ## add new column Round for each df
  tables_list <- lapply(1:length(tables_list), function(i) {
    tables_list[[i]]$Round <- round[i]
    return(tables_list[[i]])
  })
  
  ## add new column Apparatus for each df
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

# this function is used for change "VT" into "VT1" and "VT2" by line num
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

### 修改transform_web_tb函数的姓名提取算法
# 中日韩越南香港台湾是姓在前，其他皆为姓在后（包括新加坡蒙古匈牙利），
# 香港有些情况会英文名在前（当香港且姓名为四个单词，如A B C D，姓为 B，名为 C D A）
# 例如Frankie Lee Man Hin, 应转写为 LastName: Lee, FirstName: Man Hin Frankie

last_name_first_noc <- c("CHN", "JPN", "KOR", "PRK", "VIE", "HKG", "TPE")

transform_web_tb <- function(table_list, Date, Competition, Location, NOCkey) {
  tel_tb <- list_rbind(table_list)
  merged_tb <- merge(tel_tb, NOCkey, by.x = "Nation", by.y = "Full_Name", 
                     all.x = TRUE)
  
  tel_tb <- merged_tb %>% 
    mutate(Penalty = ND, D_Score = D, E_Score = E, Country = Country_Abbr, 
           Score = Total,
           Date = Date, Competition = Competition, Location = Location,
           Penalty = ifelse(Penalty < 0, -Penalty, Penalty)) %>% 
    mutate(LastName = if_else(Country %in% last_name_first_noc,
                              str_split_fixed(Athlete, " ", 2)[,1],
                              word(Athlete, -1)),
           FirstName = if_else(Country %in% last_name_first_noc,
                               str_split_fixed(Athlete, " ", 2)[,2],
                               str_replace(Athlete, LastName, ""))) %>% 
    mutate(
      # 处理HKD的特殊情况
      LastName = case_when(
        Country == "HKG" & str_count(Athlete, boundary("word")) == 4 ~ 
          word(Athlete, 2),
        TRUE ~ LastName),
      FirstName = case_when(
        Country == "HKG" & str_count(Athlete, boundary("word")) == 4 ~ 
          str_c(word(Athlete, 3), word(Athlete, 4), word(Athlete, 1), sep = " "),
        TRUE ~ FirstName
      )) %>% 
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



### Functions for Cottbus and EnBW DTB Pokal ###################################

## characteristics: For VT, each player has 3 line; info for each gymnast can
## be divided into two pages.

extract_data_cot <- function(folder_path, area) {
  all_paths <- list.files(folder_path, full.names = T)
  vt_paths <- list.files(folder_path, pattern = "VT", full.names = TRUE) |> 
    set_names(basename)
  non_vt_paths <-setdiff(all_paths, vt_paths) |>  # paths for all non-VT files
    set_names(basename)
  
  ## First, for Vault data
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

  ## Second, for non-Vault data
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
    ## remove ".pdf" and anything after it
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
      mutate(FirstName = map_chr(str_extract_all(NAME, "\\b[A-Z][a-z]+\\b"), 
                                 ~ paste(.x, collapse = " "))) |> 
      mutate(LastName = map_chr(str_extract_all(NAME, "\\b[A-Z]+\\b"), 
                                ~ paste(.x, collapse = " ")))
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

### Functions for British Championship type pdfs ###############################

extract_tables_first_diff <- function(file_path, first_page_area, other_page_area) {
  num_pages <- pdf_info(file_path)$pages
  pages <- 1:num_pages
  area <- rep(other_page_area, num_pages)
  area[1] <- first_page_area
  pdf_tables <- extract_tables(file_path, pages = pages, area = area, guess = FALSE, output = "matrix")
  pdf_tables <- lapply(pdf_tables, as.data.frame)
  return(pdf_tables)
}

get_gym_tables_first_diff <- function(folder_path, first_page_area, other_page_area) {
  all_paths <- list.files(folder_path, full.names = T) %>% 
    set_names(basename)
  raw_table_list <- map(all_paths, extract_tables_first_diff, first_page_area, other_page_area)
  return(raw_table_list)
}

process_data_frames_br <- function(br_ls_raw, col_names_vt_p1_br, col_names_vt_p2_br, col_names_nonvt_br) {
  br_ls <- br_ls_raw %>% 
    unlist(recursive = F, use.names = TRUE) %>% 
    map( function(df) { # 这人名字太长显示成两行了
      df %>%
        mutate(V3 = ifelse(V3 == "PENGKEREGO", "PENGKEREGO RAVENSCROFT Leasha", V3))
    }) %>% 
    map( function(df) {
      df %>%
        mutate(V4 = ifelse(V4 == "Loughborough Students Gymnastics", "Loughborough Students Gymnastics Club", V4)) %>% 
        mutate(V4 = ifelse(V4 == "City of Manchester Institute of", "Gymnastics", V4))
    }) %>%
    map(~ .x %>%
          filter(V3 != "RAVENSCROFT Leasha")) %>% 
    imap(function(df, name) { # For VT, delete all even rows
      if (grepl("VT", name)) {
        df <- df[-seq(2, nrow(df), by = 2), ]
      }
      return(df)
    }) %>% 
    map(function(df) { # For VT,删掉总罚分和两次总分
      cols_to_remove <- intersect(c("V12", "V13"), names(df))
      df %>%
        select(-all_of(cols_to_remove))
    }) %>% 
    imap( function(df, name) { # For VT第一页，改名再把两次的宽表格改成长表格
      if (grepl("VT", name) & !grepl("2", name)) {
        names(df) <- col_names_vt_p1_br
        df <- df %>% 
          pivot_longer(
            cols = !1:3, 
            names_to = c(".value", "vt_round"), 
            names_sep = "_", 
            values_drop_na = TRUE
          )
      }
      return(df)
    }) %>% 
    imap(function(df, name){ # For VT第二页，加一个VT1
      if (grepl("VT", name) & grepl("2", name)){
        names(df) <- col_names_vt_p2_br
        df <- df %>% mutate(vt_round = "VT1", .after = 3)
      }
      return(df)
    }) %>% 
    imap(function(df, name){
      if (!grepl("VT", name)){ # For non-VT，删掉club
        df <- df %>% select(!V4)
        names(df) <- col_names_nonvt_br
      }
      return(df)
    })
  return(br_ls)
}


### Functions for US Championship type pdfs ####################################

extract_uschampionship_data <- function(m_path, w_path, Date, Competition, Location) {
  library(tabulizer)
  library(tidyverse)
  
  us_area <- list(c(106.851163, 9.628874, 763.505728, 613.347834))
  
  m_champ23_ls <- extract_tables(m_path, guess = F, area = us_area)
  w_champ23_ls <- extract_tables(w_path, guess = F, area = us_area)
  
  w_qual_ls <- list()
  w_final_ls <- list()
  m_qual_ls <- list()
  m_final_ls <- list()
  
  ### for women ########################
  for (j in 1:length(w_champ23_ls)) {
    page <- w_champ23_ls[[j]]
    for (k in seq(2, nrow(page), 7)) {
      name <- page[k,3]
      Round <- page[k,4]
      score_VT <- page[k,6]
      score_UB <- page[k,7]
      score_BB <- page[k,8]
      score_FX <- page[k,9]
      DE_VT <- page[k+1,6]
      DE_UB <- page[k+1,7]
      DE_BB <- page[k+1,8]
      DE_FX <- page[k+1,9]
      person_qual_df <- data.frame(name, Round, score_VT, score_UB, score_BB, score_FX,
                                   DE_VT, DE_UB, DE_BB, DE_FX)
      w_qual_ls[[length(w_qual_ls)+1]] <- person_qual_df
      
      name <- page[k,3]
      Round <- page[k+3,4]
      score_VT <- page[k+3,6]
      score_UB <- page[k+3,7]
      score_BB <- page[k+3,8]
      score_FX <- page[k+3,9]
      DE_VT <- page[k+4,6]
      DE_UB <- page[k+4,7]
      DE_BB <- page[k+4,8]
      DE_FX <- page[k+4,9]
      person_final_df <- data.frame(name, Round, score_VT, score_UB, score_BB, score_FX,
                                    DE_VT, DE_UB, DE_BB, DE_FX)
      w_final_ls[[length(w_final_ls)+1]] <- person_final_df
    }
  }
  
  w_tb <- c(w_qual_ls, w_final_ls) %>% 
    bind_rows() %>% 
    separate(DE_VT, into = c("D_VT", "E_VT"), sep = " / ") %>% 
    separate(DE_UB, into = c("D_UB", "E_UB"), sep = " / ") %>% 
    separate(DE_BB, into = c("D_BB", "E_BB"), sep = " / ") %>%
    separate(DE_FX, into = c("D_FX", "E_FX"), sep = " / ") %>% 
    pivot_longer(
      cols = !1:2, 
      names_to = c(".value", "Apparatus"), 
      names_sep = "_", 
      values_drop_na = TRUE
    ) %>% 
    mutate(
      E_Score = as.numeric(E),
      D_Score = as.numeric(D),
      Score = as.numeric(score),
      Gender = "w"
    ) %>% 
    relocate(name, Gender,Round, Apparatus, D_Score, E_Score, Score) %>% 
    select(!score:E)
  
  ## for men ########################
  for (j in 1:length(m_champ23_ls)) {
    page <- m_champ23_ls[[j]]
    for (k in seq(2, nrow(page), 7)) {
      name <- page[k,3]
      Round <- page[k,4]
      score_FX <- page[k,6]
      score_PH <- page[k,7]
      score_SR <- page[k,8]
      score_VT <- page[k,9]
      score_PB_HB <- page[k,10]
      DE_FX <- page[k+1,6]
      DE_PH <- page[k+1,7]
      DE_SR <- page[k+1,8]
      DE_VT <- page[k+1,9]
      DE_PB_HB <- page[k+1,10]
      B.ND_FX <- page[k+2,6]
      B.ND_PH <- page[k+2,7]
      B.ND_SR <- page[k+2,8]
      B.ND_VT <- page[k+2,9]
      B.ND_PB_HB <- page[k+2,10]
      person_qual_df <- data.frame(name, Round, score_FX, score_PH, score_SR, 
                                   score_VT, score_PB_HB,
                                   DE_FX, DE_PH, DE_SR, DE_VT, DE_PB_HB,
                                   B.ND_FX,B.ND_PH,B.ND_SR,B.ND_VT,B.ND_PB_HB)
      m_qual_ls[[length(m_qual_ls)+1]] <- person_qual_df
      
      name <- page[k,3]
      Round <- page[k+3,4]
      score_FX <- page[k+3,6]
      score_PH <- page[k+3,7]
      score_SR <- page[k+3,8]
      score_VT <- page[k+3,9]
      score_PB_HB <- page[k+3,10]
      DE_FX <- page[k+4,6]
      DE_PH <- page[k+4,7]
      DE_SR <- page[k+4,8]
      DE_VT <- page[k+4,9]
      DE_PB_HB <- page[k+4,10]
      B.ND_FX <- page[k+5,6]
      B.ND_PH <- page[k+5,7]
      B.ND_SR <- page[k+5,8]
      B.ND_VT <- page[k+5,9]
      B.ND_PB_HB <- page[k+5,10]
      person_final_df <- data.frame(name, Round, score_FX, score_PH, score_SR, 
                                    score_VT, score_PB_HB,
                                    DE_FX, DE_PH, DE_SR, DE_VT, DE_PB_HB,
                                    B.ND_FX,B.ND_PH,B.ND_SR,B.ND_VT,B.ND_PB_HB)
      m_final_ls[[length(m_final_ls)+1]] <- person_final_df
    }
  }
  
  m_tb <- c(m_qual_ls, m_final_ls) %>% 
    bind_rows() %>% 
    separate(DE_FX, into = c("D_FX", "E_FX"), sep = " / ") %>% 
    separate(DE_PH, into = c("D_PH", "E_PH"), sep = " / ") %>% 
    separate(DE_SR, into = c("D_SR", "E_SR"), sep = " / ") %>%
    separate(DE_VT, into = c("D_VT", "E_VT"), sep = " / ") %>% 
    separate(score_PB_HB, into = c("score_PB", "score_HB"), sep = " ") 
  # 根据 DE_PB_HB 列中 '/' 的个数来划分数据框
  m_tb_2 <- m_tb %>% 
    filter(sapply(strsplit(as.character(DE_PB_HB), ""), function(x) sum(x == "/")) == 2) %>% 
    separate(DE_PB_HB, into = c("D_PB", "middle", "E_HB"), sep = " / ") %>% 
    separate(middle, into = c("E_PB", "D_HB"), sep = " ")
  m_tb_1 <- m_tb %>% 
    filter(sapply(strsplit(as.character(DE_PB_HB), ""), function(x) sum(x == "/")) != 2) %>% 
    mutate(DE_PB = ifelse(score_PB == "__.___", NA, DE_PB_HB)) %>% 
    mutate(DE_HB = ifelse(score_HB == "__.___", NA, DE_PB_HB)) %>% 
    separate(DE_PB, into = c("D_PB", "E_PB"), sep = " / ") %>%
    separate(DE_HB, into = c("D_HB", "E_HB"), sep = " / ")
  m_tb <- bind_rows(m_tb_2, m_tb_1) %>% 
    separate(B.ND_PB_HB, into = c("B.ND_PB", "B.ND_HB"), sep = " ") %>% 
    separate(B.ND_FX, into = c("bonus_FX", "pen_FX"), sep = "/") %>% 
    separate(B.ND_PH, into = c("bonus_PH", "pen_PH"), sep = "/") %>% 
    separate(B.ND_SR, into = c("bonus_SR", "pen_SR"), sep = "/") %>%
    separate(B.ND_VT, into = c("bonus_VT", "pen_VT"), sep = "/") %>%
    separate(B.ND_PB, into = c("bonus_PB", "pen_PB"), sep = "/") %>%
    separate(B.ND_HB, into = c("bonus_HB", "pen_HB"), sep = "/") %>% 
    pivot_longer(
      cols = !1:2, 
      names_to = c(".value", "Apparatus"), 
      names_sep = "_", 
      values_drop_na = TRUE
    ) %>% 
    mutate(
      E_Score = as.numeric(E),
      D_Score = as.numeric(D),
      bonus = as.numeric(bonus),
      Score = as.numeric(score)-bonus,
      Gender = "m"
    ) %>% 
    relocate(name, Gender, Round, Apparatus, D_Score, E_Score, Score) %>% 
    select(!score:DE)
  
  # combine women and men ###################
  champ23_tb <- bind_rows(m_tb, w_tb) %>% 
    separate(name, into = c("FirstName", "LastName"), sep = " ") %>% 
    mutate(FirstName = sub("^\\*", "", FirstName)) %>% 
    mutate(Date = Date, 
           Competition = Competition, 
           Location = Location, 
           Country = "USA", 
           Rank = NA,
           Penalty = round(D_Score + E_Score - Score, 3)) %>% 
    mutate(Round = ifelse(Round == "Prelims", "qual", "final")) %>% 
    mutate(Penalty = ifelse(Penalty == 0, NA, Penalty)) %>% 
    relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
             Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
    filter(!is.na(D_Score))
  
  return(champ23_tb)
}


### Functions for US Winter Cup type pdfs ####################################

extract_wintercup_data <- function(wc23_m_path, wc23_w_path, Date, Competition, Location) {
  
  us_area=list(c(106.851163, 9.628874, 763.505728, 613.347834))
  
  m_wc23_ls <- extract_tables(wc23_m_path, guess = F, area = us_area)
  w_wc23_ls <- extract_tables(wc23_w_path, guess = F, area = us_area)
  
  w_qual_ls <- list()
  m_qual_ls <- list()
  
  ### for women ###################
  for (j in 1:length(w_wc23_ls)) {
    page <- w_wc23_ls[[j]]
    for (k in seq(3, nrow(page), 5)) {
      name <- page[k,2]
      D_VT <- page[k,5]
      D_UB <- page[k,6]
      D_BB <- page[k,7]
      D_FX <- page[k,8]
      
      E_VT <- page[k+1,5]
      E_UB <- page[k+1,6]
      E_BB <- page[k+1,7]
      E_FX <- page[k+1,8]
      
      Penalty_VT <- page[k+2,5]
      Penalty_UB <- page[k+2,6]
      Penalty_BB <- page[k+2,7]
      Penalty_FX <- page[k+2,8]
      
      Score_VT <- page[k+3,5]
      Score_UB <- page[k+3,6]
      Score_BB <- page[k+3,7]
      Score_FX <- page[k+3,8]
      
      Rank_VT <- page[k+4,5]
      Rank_UB <- page[k+4,6]
      Rank_BB <- page[k+4,7]
      Rank_FX <- page[k+4,8]
      
      person_qual_df <- data.frame(name, 
                                   D_VT, D_UB, D_BB, D_FX,
                                   E_VT, E_UB, E_BB, E_FX,
                                   Penalty_VT, Penalty_UB, Penalty_BB, Penalty_FX,
                                   Score_VT, Score_UB, Score_BB, Score_FX,
                                   Rank_VT, Rank_UB, Rank_BB, Rank_FX)
      w_qual_ls[[length(w_qual_ls)+1]] <- person_qual_df
    }
  }
  
  w_tb <- w_qual_ls %>% 
    bind_rows() %>% 
    pivot_longer(
      cols = !1, 
      names_to = c(".value", "Apparatus"), 
      names_sep = "_", 
      values_drop_na = TRUE
    ) %>% 
    filter(D != "__.___") %>% 
    mutate(
      E_Score = as.numeric(E),
      D_Score = as.numeric(D),
      Score = as.numeric(Score),
      Gender = "w"
    ) %>% 
    relocate(name, Gender, Apparatus, Rank, D_Score, E_Score, Score) %>% 
    select(!D:Penalty)
  
  ### for men ####################
  for (j in 1:length(m_wc23_ls)) {
    page <- m_wc23_ls[[j]]
    for (k in seq(3, nrow(page), 6)) {
      name <- page[k,2]
      
      D_FX <- page[k,5]
      D_PH <- page[k,6]
      D_SR <- page[k,7]
      D_VT <- page[k,8]
      D_PB <- page[k,9]
      D_HB <- page[k,10]
      
      E_FX <- page[k+1,5]
      E_PH <- page[k+1,6]
      E_SR <- page[k+1,7]
      E_VT <- page[k+1,8]
      E_PB <- page[k+1,9]
      E_HB <- page[k+1,10]
      
      bonus_FX <- page[k+2,5]
      bonus_PH <- page[k+2,6]
      bonus_SR <- page[k+2,7]
      bonus_VT <- page[k+2,8]
      bonus_PB <- page[k+2,9]
      bonus_HB <- page[k+2,10]
      
      Penalty_FX <- page[k+3,5]
      Penalty_PH <- page[k+3,6]
      Penalty_SR <- page[k+3,7]
      Penalty_VT <- page[k+3,8]
      Penalty_PB <- page[k+3,9]
      Penalty_HB <- page[k+3,10]
      
      Score_FX <- page[k+4,5]
      Score_PH <- page[k+4,6]
      Score_SR <- page[k+4,7]
      Score_VT <- page[k+4,8]
      Score_PB <- page[k+4,9]
      Score_HB <- page[k+4,10]
      
      Rank_FX <- page[k+5,5]
      Rank_PH <- page[k+5,6]
      Rank_SR <- page[k+5,7]
      Rank_VT <- page[k+5,8]
      Rank_PB <- page[k+5,9]
      Rank_HB <- page[k+5,10]
      
      person_qual_df <- data.frame(name, 
                                   D_FX, D_PH, D_SR, D_VT, D_PB, D_HB,
                                   E_FX, E_PH, E_SR, E_VT, E_PB, E_HB,
                                   bonus_FX, bonus_PH, bonus_SR, bonus_VT, bonus_PB, bonus_HB,
                                   Penalty_FX, Penalty_PH, Penalty_SR, Penalty_VT, Penalty_PB, Penalty_HB,
                                   Score_FX, Score_PH, Score_SR, Score_VT, Score_PB, Score_HB,
                                   Rank_FX, Rank_PH, Rank_SR, Rank_VT, Rank_PB, Rank_HB)
      m_qual_ls[[length(m_qual_ls)+1]] <- person_qual_df
    }
  }
  
  m_tb <- m_qual_ls %>% 
    bind_rows() %>% 
    pivot_longer(
      cols = !1, 
      names_to = c(".value", "Apparatus"), 
      names_sep = "_", 
      values_drop_na = TRUE
    ) %>% 
    filter(D != "__.___") %>% 
    mutate(
      E_Score = as.numeric(E),
      D_Score = as.numeric(D),
      bonus = as.numeric(bonus),
      Score = as.numeric(Score)-bonus,
      Gender = "m"
    ) %>% 
    relocate(name, Gender, Apparatus, Rank, D_Score, E_Score, Score) %>% 
    select(!D:Penalty)
  
  ## combine women and men #############
  
  wc23_tb <- bind_rows(m_tb, w_tb) %>% 
    mutate(Rank = as.numeric(gsub("[^0-9]", "", Rank))) %>% 
    mutate(FirstName = sub("^(\\S+)\\s.*$", "\\1", name),
           LastName = sub("^\\S+\\s(.*)$", "\\1", name)) %>% 
    mutate(Round = "AAfinal") %>% 
    mutate(Date = Date,
           Competition = Competition,
           Location = Location,
           Country = "USA",
           Penalty = round(D_Score + E_Score - Score, 3)) %>% 
    mutate(Penalty = ifelse(Penalty == 0, NA, Penalty)) %>% 
    relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
             Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
    select(!name)
  
  return(wc23_tb)
}



### Functions for name adjustment in the final dataset: combined_df ############
flip_first_last <- function(data, condition) {
  # 使用mutate和ifelse来根据条件交换列的值
  data <- data %>%
    mutate(
      LastName = ifelse(!!condition, FirstName, LastName),
      FirstName = ifelse(!!condition, LastName, FirstName)
    )
  return(data)
}
