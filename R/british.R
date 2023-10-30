source("function.R")

### 2023 British Gymnastics Championships ######################################

# locate_areas("../pdfs_2023/british_23/m_qual_VT.pdf", pages = 2)

folder_path <- "../pdf/british_23"
first_page_area <- list(c(176.27017,  30.66901, 796.18054 ,565.40909 )) 
other_page_area <- list(c(85.70935,  32.82522, 749.82202, 567.56530 ))

col_names_vt_p1_br <- c("Rank", "Bib", "Name","D_VT1","Score_VT1","Pen_VT1",
                        "Bonus_VT1","D_VT2","Score_VT2","Pen_VT2","Bonus_VT2")
col_names_vt_p2_br <- c("Rank", "Bib", "Name","D","Score","Pen","Bonus")
col_names_nonvt_br <- c("Rank", "Bib", "Name","D","E","Bonus","Pen", "Score")

Date = "23-26 Mar 2023"
Competition = "2023 British Gymnastics Championships"
Location = "Liverpool, England"

br_ls_raw <- get_gym_tables_first_diff(folder_path, first_page_area, other_page_area)
br_ls <- process_data_frames_br(br_ls_raw, col_names_vt_p1_br, col_names_vt_p2_br, col_names_nonvt_br)
br23_tb <- list_rbind(br_ls, names_to = "title") %>% 
  separate_wider_delim(
    title,
    delim = "_",
    names = c("Gender", "Round", "Apparatus")
  ) %>% 
  mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) %>% 
  mutate(Apparatus = ifelse(!is.na(vt_round), vt_round, Apparatus)) %>% 
  mutate(
    Rank = as.numeric(Rank),
    E_Score = as.numeric(E),
    D_Score = as.numeric(D),
    Penalty = as.numeric(Pen),
    Score = as.numeric(Score)
  ) %>% 
  mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), 
                             ~ paste(.x, collapse = " "))) %>% 
  mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), 
                            ~ paste(.x, collapse = " "))) %>% 
  mutate(Date = Date, Competition = Competition, 
         Location = Location, Country = "GBR") %>% 
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!Bib:vt_round)

## 修改数据的问题
br23_tb <- br23_tb %>% 
  filter(!D_Score==0) %>%  # 删掉D=0的行，基本上都是VT2没参加的情况
  mutate(E_Score = if_else(is.na(E_Score), 
                           Score - D_Score + if_else(is.na(Penalty), 0, Penalty), 
                           E_Score))

write_csv(br23_tb, "../cleandata/data_new/british_23.csv")





### 2022 British Gymnastics Championships ######################################

folder_path <- "../pdf/british_22"
col_names_nonvt_br <- c("Rank", "Bib", "Name","D","E","Pen", "Score")
col_names_vt_qual_p1 <- c("Rank", "Bib", "Name", "Club","D_VT1","Score_VT1","Pen_VT1",
                          "D_VT2","Score_VT2","Pen_VT2")
col_names_vt_qual_p2 <- c("Rank", "Bib", "Name", "Club","D","Score","Pen")
col_names_vt_final <- c("Rank", "Bib", "Name","D_VT1","Score_VT1","Pen_VT1","Bonus_VT1",
                        "D_VT2","Score_VT2","Pen_VT2", "Bonus_VT2")

br_ls_raw_22 <- get_gym_tables_first_diff(folder_path, first_page_area, other_page_area)
# br_ls_22 <- process_data_frames_br(br_ls_raw_22, col_names_vt_p1_br, col_names_vt_p2_br, col_names_nonvt_br)

br_ls_22 <- br_ls_raw_22 %>% 
  unlist(recursive = F, use.names = TRUE) %>% 
  map( function(df) {
    df %>%
      mutate(V3 = ifelse(V3 == "PENGKEREGO", "PENGKEREGO RAVENSCROFT Leasha", V3))
  }) %>% 
  map( function(df) {
    df %>%
      mutate(V4 = ifelse(V4 == "Loughborough Students Gymnastics", "Loughborough Students Gymnastics Club", V4)) %>% 
      mutate(V4 = ifelse(V4 == "City of Manchester Institute of", "City of Manchester Institute of Gymnastics", V4))
  }) %>%
  map(~ .x %>%
        filter(V3 != "RAVENSCROFT Leasha") %>% 
        filter(V4 != "Gymnastics") %>% 
        filter(V4 != "Club")) %>% 
  imap(function(df, name) { # For VT final, delete all even rows
    if (grepl("VT", name) & grepl("final", name)) {
      df <- df[-seq(2, nrow(df), by = 2), ]
    }
    return(df)
  }) %>% 
  map(function(df) { # For VT final, delete last two cols: ND, Total
    cols_to_remove <- intersect(c("V12", "V13"), names(df))
    df %>%
      select(-all_of(cols_to_remove))
  }) %>% 
  ## VT共三种类型：final，qual第一页，qual第二页。以下三步，将所有VT格式统一。
  imap( function(df, name) {
    if (grepl("VT", name) & grepl("qual", name) & !grepl("2", name)) { # VT, qual, 不是第二页
      df <- df[,1:10]
      names(df) <- col_names_vt_qual_p1
      df <- df %>%
        pivot_longer(
          cols = !1:4,
          names_to = c(".value", "vt_round"),
          names_sep = "_",
          values_drop_na = TRUE
        ) %>% 
        select(!Club)
    }
    return(df)
  }) %>% 
  imap( function(df, name) {
    if (grepl("VT", name) & grepl("qual", name) & grepl("2", name)) { # VT, qual, 是第二页
      df <- df[,1:7]
      names(df) <- col_names_vt_qual_p2
      df <- df %>% 
        mutate(vt_round = "VT1", .after = 3) %>% 
        select(!Club)
    }
    return(df)
  }) %>% 
  imap(function(df, name){
    if (grepl("VT", name) & grepl("final", name)){ # VT, final
      names(df) <- col_names_vt_final
      df <- df %>%
        pivot_longer(
          cols = !1:3,
          names_to = c(".value", "vt_round"),
          names_sep = "_",
          values_drop_na = TRUE
        ) %>% 
        select(!Bonus)
    }
    return(df)
  }) %>% 
  imap(function(df, name){
    if (!grepl("VT", name)){ # all non-VT tables
      df <- df %>% select(!c(V4,V7))
      names(df) <- col_names_nonvt_br
    }
    return(df)
  })

# br_ls_22

split_lists <- br_ls_22 %>% 
  split(grepl("VT", names(br_ls_22)))
vt_list <- split_lists$`TRUE`
non_vt_list <- split_lists$`FALSE`



br_tb_22_vt <- list_rbind(vt_list, names_to = "title") %>% 
  separate_wider_delim(
    title,
    delim = "_",
    names = c("Gender", "Round"),
    too_many = "drop"
  ) %>% 
  mutate(Apparatus = vt_round) %>% 
  mutate(
    D_Score = as.numeric(D),
    Penalty = as.numeric(Pen),
    Score = as.numeric(Score),
    E_Score = round(Score - D_Score - Penalty, 2)
  ) %>% 
  filter(!is.na(Score))

br_tb_22_nonvt <- list_rbind(non_vt_list, names_to = "title") %>% 
  separate_wider_delim(
    title,
    delim = "_",
    names = c("Gender", "Round", "Apparatus")
  ) %>% 
  mutate(Apparatus = str_replace(Apparatus, "\\.pdf.*$", "")) %>% 
  mutate(
    D_Score = as.numeric(D),
    Penalty = as.numeric(Pen),
    Score = as.numeric(Score),
    E_Score = as.numeric(E)
  )


Date = "24-27 Mar 2022"
Competition = "2022 British Gymnastics Championships"
Location = "Liverpool, England"

br_tb_22 <- bind_rows(br_tb_22_nonvt, br_tb_22_vt) %>% 
  mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), 
                             ~ paste(.x, collapse = " "))) %>% 
  mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), 
                            ~ paste(.x, collapse = " "))) %>% 
  mutate(Date = Date, Competition = Competition, 
         Location = Location, Country = "GBR") %>% 
  mutate(Rank = as.numeric(Rank)) %>% 
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!Bib:vt_round)

write_csv(br_tb_22, "../cleandata/data_new/british_22.csv")
