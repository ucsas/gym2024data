# locate_areas("../pdfs_2023/british_23/m_qual_VT.pdf", pages = 2)

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



folder_path <- "../pdfs_2023/british_23"
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
  
br_ls <- br_ls_raw %>% 
  unlist(recursive = F, use.names = TRUE) %>% 
  map( function(df) {
    df %>%
      mutate(V3 = ifelse(V3 == "PENGKEREGO", "PENGKEREGO RAVENSCROFT Leasha", V3))
  }) %>% 
  map(~ .x %>%
        filter(V3 != "RAVENSCROFT Leasha")) %>% 
  imap(function(df, name) { # For VT, delete all even rows
    if (grepl("VT", name)) {
      df <- df[-seq(2, nrow(df), by = 2), ]
    }
    return(df)
  }) %>% 
  map(function(df) {
    cols_to_remove <- intersect(c("V12", "V13"), names(df))
    df %>%
      select(-all_of(cols_to_remove))
  }) %>% 
  imap( function(df, name) {
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
  imap(function(df, name){
    if (grepl("VT", name) & grepl("2", name)){
      names(df) <- col_names_vt_p2_br
      df <- df %>% mutate(vt_round = "VT1", .after = 3)
    }
    return(df)
  }) %>% 
  imap(function(df, name){
    if (!grepl("VT", name)){
      df <- df %>% select(!V4)
      names(df) <- col_names_nonvt_br
    }
    return(df)
  })

# br_ls

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
         Location = Location, Country = "BRN") %>% 
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!Bib:vt_round)
  

write_csv(br23_tb, "../cleandata/data_new/british_23.csv")


