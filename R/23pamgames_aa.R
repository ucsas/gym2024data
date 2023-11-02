### SANTIAGO 2023 XIX Pan American Games #######################################


### SANTIAGO 2023 XIX Pan American Games: Women AA Final #######################

pam_w_aa_final_page1 <- pdf_data("../pdf/23pan_am_aa/w_aa_final.pdf")[[1]] %>%
  arrange(y,x) %>% 
  filter(!text == "!") %>% 
  filter(y > 194 & y < 526) %>% 
  mutate(y_rank = dense_rank(y)) %>% 
  mutate(group = ceiling(y_rank / 5))

pam_w_aa_final_list1 <- split(pam_w_aa_final_page1, pam_w_aa_final_page1$group) %>% 
  lapply( function(df) {
    df %>% mutate(y_rank = dense_rank(y))
  }) %>% 
  lapply( function(df) {
    df %>%
      group_by(y_rank) %>%
      mutate(x_rank = dense_rank(x)) %>%
      ungroup()
  })

pam_w_aa_final_page2 <- pdf_data("../pdf/23pan_am_aa/w_aa_final.pdf")[[2]] %>% 
  arrange(y,x) %>% 
  filter(!text == "!") %>% 
  filter(y > 194 & y < 526) %>% 
  mutate(y_rank = dense_rank(y)) %>% 
  mutate(group = ceiling(y_rank / 5))

pam_w_aa_final_list2 <- split(pam_w_aa_final_page2, pam_w_aa_final_page2$group) %>% 
  lapply( function(df) {
    df %>% mutate(y_rank = dense_rank(y))
  }) %>% 
  lapply( function(df) {
    df %>%
      group_by(y_rank) %>%
      mutate(x_rank = dense_rank(x)) %>%
      ungroup()
  })

pam_w_aa_final_page3 <- pdf_data("../pdf/23pan_am_aa/w_aa_final.pdf")[[3]] %>% 
  arrange(y,x) %>% 
  filter(!text == "!") %>% 
  filter(y > 194 & y < 480) %>% 
  mutate(y_rank = dense_rank(y)) %>% 
  mutate(group = ceiling(y_rank / 5))

pam_w_aa_final_list3 <- split(pam_w_aa_final_page3, pam_w_aa_final_page3$group) %>% 
  lapply( function(df) {
    df %>% mutate(y_rank = dense_rank(y))
  }) %>% 
  lapply( function(df) {
    df %>%
      group_by(y_rank) %>%
      mutate(x_rank = dense_rank(x)) %>%
      ungroup()
  })

pam_w_aa_final_list_combined <- c(pam_w_aa_final_list1, pam_w_aa_final_list2, pam_w_aa_final_list3)

#View(pam_w_aa_final_list_combined[[24]])

### 开始循环
w_final_ls <- list()

for (i in 1:length(pam_w_aa_final_list_combined)) {
  gymnast <- pam_w_aa_final_list_combined[[i]]
  
  Name <- gymnast %>%
    filter(y_rank == 3) %>%
    slice(-n()) %>%
    pull(text) %>%
    paste(collapse = " ")
  
  Country <- gymnast %>%
    filter(y_rank == 3) %>%
    slice(n()) %>%
    pull(text)
  
  VT_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 1) %>%
    pull(text)
  VT_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 2) %>%
    pull(text)
  VT_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 3) %>%
    pull(text)
  VT_E <- gymnast %>%
    filter(y_rank == 5 & x_rank == 1) %>%
    pull(text)
  VT_Penalty <- gymnast %>%
    filter(y_rank == 5 & x_rank == 2) %>%
    pull(text)
  
  UB_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 4) %>%
    pull(text)
  UB_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 5) %>%
    pull(text)
  UB_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 6) %>%
    pull(text)
  UB_E <- gymnast %>%
    filter(y_rank == 5 & x_rank == 3) %>%
    pull(text)
  UB_Penalty <- gymnast %>%
    filter(y_rank == 5 & x_rank == 4) %>%
    pull(text)
  
  BB_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 7) %>%
    pull(text)
  BB_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 8) %>%
    pull(text)
  BB_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 9) %>%
    pull(text)
  BB_E <- gymnast %>%
    filter(y_rank == 5 & x_rank == 5) %>%
    pull(text)
  BB_Penalty <- gymnast %>%
    filter(y_rank == 5 & x_rank == 6) %>%
    pull(text)
  
  FX_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 10) %>%
    pull(text)
  FX_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 11) %>%
    pull(text)
  FX_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 12) %>%
    pull(text)
  FX_E <- gymnast %>%
    filter(y_rank == 4 & x_rank == 1) %>%
    pull(text)
  FX_Penalty <- gymnast %>%
    filter(y_rank == 4 & x_rank == 2) %>%
    pull(text)
  
  person_final_df <- data.frame(Name, Country, 
                                VT_Rank,UB_Rank,BB_Rank,FX_Rank,
                                VT_D,UB_D,BB_D,FX_D,
                                VT_E,UB_E,BB_E,FX_E,
                                VT_Penalty,UB_Penalty,BB_Penalty,FX_Penalty,
                                VT_Score,UB_Score,BB_Score,FX_Score)
  w_final_ls[[length(w_final_ls)+1]] <- person_final_df
}

# w_final_ls

pam_w_aa_final_tb_wide <- list_rbind(w_final_ls) 

pam_w_aa_final_tb <- pam_w_aa_final_tb_wide %>%  
  pivot_longer(
    cols = !1:2, 
    names_to = c("Apparatus",".value"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w", Round = "AAfinal")









### SANTIAGO 2023 XIX Pan American Games: Men AA Final #######################

pam_m_aa_final_page1 <- pdf_data("../pdf/23pan_am_aa/m_aa_final.pdf")[[1]] %>%
  arrange(y,x) %>% 
  filter(!text == "!") %>% 
  filter(y > 194 & y < 526) %>% 
  mutate(y_rank = dense_rank(y)) %>% 
  mutate(group = ceiling(y_rank / 5))

pam_m_aa_final_list1 <- split(pam_m_aa_final_page1, pam_m_aa_final_page1$group) %>% 
  lapply( function(df) {
    df %>% mutate(y_rank = dense_rank(y))
  }) %>% 
  lapply( function(df) {
    df %>%
      group_by(y_rank) %>%
      mutate(x_rank = dense_rank(x)) %>%
      ungroup()
  })

pam_m_aa_final_page2 <- pdf_data("../pdf/23pan_am_aa/m_aa_final.pdf")[[2]] %>% 
  arrange(y,x) %>% 
  filter(!text == "!") %>% 
  filter(y > 194 & y < 526) %>% 
  mutate(y_rank = dense_rank(y)) %>% 
  mutate(group = ceiling(y_rank / 5))

pam_m_aa_final_list2 <- split(pam_m_aa_final_page2, pam_m_aa_final_page2$group) %>% 
  lapply( function(df) {
    df %>% mutate(y_rank = dense_rank(y))
  }) %>% 
  lapply( function(df) {
    df %>%
      group_by(y_rank) %>%
      mutate(x_rank = dense_rank(x)) %>%
      ungroup()
  })

pam_m_aa_final_page3 <- pdf_data("../pdf/23pan_am_aa/m_aa_final.pdf")[[3]] %>% 
  arrange(y,x) %>% 
  filter(!text == "!") %>% 
  filter(y > 194 & y < 480) %>% 
  mutate(y_rank = dense_rank(y)) %>% 
  mutate(group = ceiling(y_rank / 5))

pam_m_aa_final_list3 <- split(pam_m_aa_final_page3, pam_m_aa_final_page3$group) %>% 
  lapply( function(df) {
    df %>% mutate(y_rank = dense_rank(y))
  }) %>% 
  lapply( function(df) {
    df %>%
      group_by(y_rank) %>%
      mutate(x_rank = dense_rank(x)) %>%
      ungroup()
  })

pam_m_aa_final_list_combined <- c(pam_m_aa_final_list1, pam_m_aa_final_list2, pam_m_aa_final_list3)


### 开始循环
m_final_ls <- list()

for (i in 1:length(pam_m_aa_final_list_combined)) {
  gymnast <- pam_m_aa_final_list_combined[[i]]
  
  Name <- gymnast %>%
    filter(y_rank == 3) %>%
    slice(-n()) %>%
    pull(text) %>%
    paste(collapse = " ")
  
  Country <- gymnast %>%
    filter(y_rank == 3) %>%
    slice(n()) %>%
    pull(text)
  
  FX_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 1) %>%
    pull(text)
  FX_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 2) %>%
    pull(text)
  FX_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 3) %>%
    pull(text)
  FX_E <- gymnast %>%
    filter(y_rank == 5 & x_rank == 1) %>%
    pull(text)
  FX_Penalty <- gymnast %>%
    filter(y_rank == 5 & x_rank == 2) %>%
    pull(text)
  
  PH_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 4) %>%
    pull(text)
  PH_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 5) %>%
    pull(text)
  PH_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 6) %>%
    pull(text)
  PH_E <- gymnast %>%
    filter(y_rank == 5 & x_rank == 3) %>%
    pull(text)
  PH_Penalty <- gymnast %>%
    filter(y_rank == 5 & x_rank == 4) %>%
    pull(text)
  
  SR_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 7) %>%
    pull(text)
  SR_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 8) %>%
    pull(text)
  SR_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 9) %>%
    pull(text)
  SR_E <- gymnast %>%
    filter(y_rank == 5 & x_rank == 5) %>%
    pull(text)
  SR_Penalty <- gymnast %>%
    filter(y_rank == 5 & x_rank == 6) %>%
    pull(text)
  
  VT_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 10) %>%
    pull(text)
  VT_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 11) %>%
    pull(text)
  VT_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 12) %>%
    pull(text)
  VT_E <- gymnast %>%
    filter(y_rank == 5 & x_rank == 7) %>%
    pull(text)
  VT_Penalty <- gymnast %>%
    filter(y_rank == 5 & x_rank == 8) %>%
    pull(text)
  
  PB_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 13) %>%
    pull(text)
  PB_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 14) %>%
    pull(text)
  PB_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 15) %>%
    pull(text)
  PB_E <- gymnast %>%
    filter(y_rank == 5 & x_rank == 9) %>%
    pull(text)
  PB_Penalty <- gymnast %>%
    filter(y_rank == 5 & x_rank == 10) %>%
    pull(text)
  
  HB_D <- gymnast %>%
    filter(y_rank == 1 & x_rank == 16) %>%
    pull(text)
  HB_Score <- gymnast %>%
    filter(y_rank == 1 & x_rank == 17) %>%
    pull(text)
  HB_Rank <- gymnast %>%
    filter(y_rank == 1 & x_rank == 18) %>%
    pull(text)
  HB_E <- gymnast %>%
    filter(y_rank == 4 & x_rank == 1) %>%
    pull(text)
  HB_Penalty <- gymnast %>%
    filter(y_rank == 4 & x_rank == 2) %>%
    pull(text)
  
  
  
  person_final_df <- data.frame(Name, Country, 
                                FX_Rank,PH_Rank,SR_Rank,VT_Rank,PB_Rank,HB_Rank,
                                FX_D,PH_D,SR_D,VT_D,PB_D,HB_D,
                                FX_E,PH_E,SR_E,VT_E,PB_E,HB_E,
                                FX_Penalty,PH_Penalty,SR_Penalty,VT_Penalty,PB_Penalty,HB_Penalty,
                                FX_Score,PH_Score,SR_Score,VT_Score,PB_Score,HB_Score)
  m_final_ls[[length(m_final_ls)+1]] <- person_final_df
}

m_final_ls

pam_m_aa_final_tb_wide <- list_rbind(m_final_ls) 

pam_m_aa_final_tb <- pam_m_aa_final_tb_wide %>%  
  pivot_longer(
    cols = !1:2, 
    names_to = c("Apparatus",".value"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAfinal")

### Combine 2 tables ###########################################################
Date = "21-25 Oct 2023"
Competition = "SANTIAGO 2023 XIX Pan American Games"
Location = "Santiago, Chile"

pam_aa_tb <- bind_rows(pam_m_aa_final_tb, pam_w_aa_final_tb) %>% 
  filter(D != "DNS") %>% # 删掉D==DNS的列
  mutate(Penalty = as.numeric(Penalty), # 把D E Score Rank转成数值
         Rank = as.numeric(Rank), # D E P列重命名
         E_Score = as.numeric(E),
         D_Score = as.numeric(D),
         Score = as.numeric(Score),
         Penalty = ifelse(Penalty < 0, -Penalty, Penalty)) %>% 
  mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), # 拆分姓名
                             ~ paste(.x, collapse = " "))) %>% 
  mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), 
                            ~ paste(.x, collapse = " "))) %>% 
  mutate(Date = Date, Competition = Competition, Location = Location) %>% # 加上赛事信息
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!Name:E)

write_csv(pam_aa_tb, "../cleandata/data_new/pan_am_games_23_aa.csv")