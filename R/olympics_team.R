### 2022 Tokyo Olympics Team Women #############################################

ol_w_ls_raw <- get_gym_tables(folder_path = "../pdf/olympics_team")
ol_w_qual_tb_raw <- ol_w_ls_raw[[2]] %>% 
  list_rbind() %>% 
  select(!c(1,8,9))
ol_w_final_tb_raw <- ol_w_ls_raw[[1]] %>% 
  list_rbind() %>% 
  select(!c(1,8))


# 创建一个序列，标记每9行为一个组
group <- as.factor((seq_len(nrow(ol_w_qual_tb_raw)) - 1) %/% 9)
# 使用split函数根据组拆分数据框
ol_w_qual_ls_country <- split(ol_w_qual_tb_raw, group) %>% 
  map(  ~{
    # 获取第一行第一列的值
    value <- .x[1, 1]
    # 以“ - ”为分隔符分割字符串，并保留第一部分
    first_part <- strsplit(value, split = " - ")[[1]][1]
    # 将修改后的值赋回数据框的第一行第一列
    .x[1, 1] <- first_part
    # 返回修改后的数据框
    .x
  })

names(ol_w_qual_ls_country) <- sapply(ol_w_qual_ls_country, function(df) {
  return(df[1, 1]) # 直接返回第二列第一行的值
})

ol_w_qual_ls_country <- map(ol_w_qual_ls_country, ~ .x[-1, ])


w_qual_ls <- list()

for (j in 1:length(ol_w_qual_ls_country)) {
  page <- ol_w_qual_ls_country[[j]]
  noc <- names(ol_w_qual_ls_country)[j]
  for (k in seq(1, nrow(page), 2)) {
    Name <- page[k,1]
    VT_d_score <- page[k,3]
    UB_d_score <- page[k,4]
    BB_d_score <- page[k,5]
    FX_d_score <- page[k,6]
    
    VT_e_pen <- page[k+1,3]
    UB_e_pen <- page[k+1,4]
    BB_e_pen <- page[k+1,5]
    FX_e_pen <- page[k+1,6]
    
    person_qual_df <- data.frame(Name, Country = noc, VT_d_score,UB_d_score,BB_d_score,FX_d_score,
                                 VT_e_pen,UB_e_pen,BB_e_pen,FX_e_pen)
    w_qual_ls[[length(w_qual_ls)+1]] <- person_qual_df
  }
}

w_qual_ls

ol_w_qual_tb <- w_qual_ls %>% 
  list_rbind() %>% 
  mutate(Round = "TeamQual", .after = 2)

ol_w_qual_tb


### final ############################################################

ol_w_final_tb_raw
# 创建一个序列，标记每9行为一个组
group <- as.factor((seq_len(nrow(ol_w_final_tb_raw)) - 1) %/% 9)

# 使用split函数根据组拆分数据框
ol_w_final_ls_country <- split(ol_w_final_tb_raw, group) %>% 
  map(  ~{
    # 获取第一行第一列的值
    value <- .x[1, 1]
    # 以“ - ”为分隔符分割字符串，并保留第一部分
    first_part <- strsplit(value, split = " - ")[[1]][1]
    # 将修改后的值赋回数据框的第一行第一列
    .x[1, 1] <- first_part
    # 返回修改后的数据框
    .x
  })

names(ol_w_final_ls_country) <- sapply(ol_w_final_ls_country, function(df) {
  return(df[1, 1]) # 直接返回第二列第一行的值
})

ol_w_final_ls_country <- map(ol_w_final_ls_country, ~ .x[-1, ])


w_final_ls <- list()

for (j in 1:length(ol_w_final_ls_country)) {
  page <- ol_w_final_ls_country[[j]]
  noc <- names(ol_w_final_ls_country)[j]
  for (k in seq(1, nrow(page), 2)) {
    Name <- page[k,1]
    VT_d_score <- page[k,3]
    UB_d_score <- page[k,4]
    BB_d_score <- page[k,5]
    FX_d_score <- page[k,6]
    
    VT_e_pen <- page[k+1,3]
    UB_e_pen <- page[k+1,4]
    BB_e_pen <- page[k+1,5]
    FX_e_pen <- page[k+1,6]
    
    person_final_df <- data.frame(Name, Country = noc, VT_d_score,UB_d_score,BB_d_score,FX_d_score,
                                 VT_e_pen,UB_e_pen,BB_e_pen,FX_e_pen)
    w_final_ls[[length(w_final_ls)+1]] <- person_final_df
  }
}


ol_w_final_tb <- w_final_ls %>% 
  list_rbind() %>% 
  mutate(Round = "TeamFinal", .after = 2)



### combine qual and final ######################
ol_w_tb_wide <- bind_rows(ol_w_qual_tb, ol_w_final_tb) %>% 
  separate(VT_d_score, into = c("D_VT", "Score_VT"), sep = " ") %>% 
  separate(UB_d_score, into = c("D_UB", "Score_UB"), sep = " ") %>% 
  separate(BB_d_score, into = c("D_BB", "Score_BB"), sep = " ") %>%
  separate(FX_d_score, into = c("D_FX", "Score_FX"), sep = " ") %>% 
  separate(VT_e_pen, into = c("E_VT", "Pen_VT"), sep = " ") %>%
  separate(UB_e_pen, into = c("E_UB", "Pen_UB"), sep = " ") %>%
  separate(BB_e_pen, into = c("E_BB", "Pen_BB"), sep = " ") %>%
  separate(FX_e_pen, into = c("E_FX", "Pen_FX"), sep = " ")

ol_w_tb_long <- ol_w_tb_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w")

ol_w_tb <- ol_w_tb_long %>% 
  mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), 
                             ~ paste(.x, collapse = " "))) %>% 
  mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), 
                            ~ paste(.x, collapse = " "))) %>% 
  mutate(Penalty = as.numeric(Pen), # 把D E Score Rank转成数值
         E_Score = as.numeric(E),
         D_Score = as.numeric(D),
         Score = as.numeric(Score),
         Penalty = ifelse(Penalty < 0, -Penalty, Penalty),
         Rank = NA) %>% 
  filter(!is.na(D_Score)) %>% 
  mutate(Competition = "Olympic Games", Location = "Tokyo, Japan") %>% # 加上赛事信息
  mutate(Date = case_when(
    Round == "TeamQual" ~ "25 July 2021",
    Round == "TeamFinal" ~ "1 Aug 2021",
    TRUE ~ NA_character_
  )) %>% 
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!Name:Pen)

write_csv(ol_w_tb, "../cleandata/data_old/olympics_team.csv")
