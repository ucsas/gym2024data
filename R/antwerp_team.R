### 2023 ANTWERP 52nd FIG Artistic Gymnastics World Championships ##############

atwp_w_ls_raw <- get_gym_tables(folder_path = "../pdf/antwerp_team")

atwp_m_final_tb_raw <- atwp_w_ls_raw[[1]] %>% 
  list_rbind() %>% 
  select(!c(1,10)) # 删掉第一列团体排名，最后一列团体总分

atwp_w_final_tb_raw <- atwp_w_ls_raw[[2]] %>% 
  list_rbind() %>% 
  select(!c(1,8))

### women final ################################################################

# 创建一个序列，标记每11行为一个组
group <- as.factor((seq_len(nrow(atwp_w_final_tb_raw)) - 1) %/% 11)

# 使用split函数根据组拆分数据框
atwp_w_final_ls_country <- split(atwp_w_final_tb_raw, group) %>% 
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

# 把列表里每个数据框命名为国家名
names(atwp_w_final_ls_country) <- sapply(atwp_w_final_ls_country, function(df) {
  return(df[1, 1]) # 直接返回第二列第一行的值
})

# 删掉每个数据框第一行：团体分数行
atwp_w_final_ls_country <- map(atwp_w_final_ls_country, ~ .x[-1, ])


w_final_ls <- list()

for (j in 1:length(atwp_w_final_ls_country)) {
  page <- atwp_w_final_ls_country[[j]]
  noc <- names(atwp_w_final_ls_country)[j]
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


atwp_w_final_tb <- w_final_ls %>% 
  list_rbind() %>% 
  mutate(Round = "TeamFinal", .after = 2) %>% 
  separate(VT_d_score, into = c("D_VT", "Score_VT"), sep = " ") %>% 
  separate(UB_d_score, into = c("D_UB", "Score_UB"), sep = " ") %>% 
  separate(BB_d_score, into = c("D_BB", "Score_BB"), sep = " ") %>%
  separate(FX_d_score, into = c("D_FX", "Score_FX"), sep = " ") %>% 
  separate(VT_e_pen, into = c("E_VT", "Pen_VT"), sep = " ") %>%
  separate(UB_e_pen, into = c("E_UB", "Pen_UB"), sep = " ") %>%
  separate(BB_e_pen, into = c("E_BB", "Pen_BB"), sep = " ") %>%
  separate(FX_e_pen, into = c("E_FX", "Pen_FX"), sep = " ") %>% 
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w") %>% 
  filter(!D == "")



### men final ##################################################################

# 创建一个序列，标记每11行为一个组
group <- as.factor((seq_len(nrow(atwp_m_final_tb_raw)) - 1) %/% 11)

# 使用split函数根据组拆分数据框
atwp_m_final_ls_country <- split(atwp_m_final_tb_raw, group) %>% 
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

# 把列表里每个数据框命名为国家名
names(atwp_m_final_ls_country) <- sapply(atwp_m_final_ls_country, function(df) {
  return(df[1, 1]) # 直接返回第二列第一行的值
})

# 删掉每个数据框第一行：团体分数行
atwp_m_final_ls_country <- map(atwp_m_final_ls_country, ~ .x[-1, ])


m_final_ls <- list()

for (j in 1:length(atwp_m_final_ls_country)) {
  page <- atwp_m_final_ls_country[[j]]
  noc <- names(atwp_m_final_ls_country)[j]
  for (k in seq(1, nrow(page), 2)) {
    Name <- page[k,1]
    FX_d_score <- page[k,3]
    PH_d_score <- page[k,4]
    SR_d_score <- page[k,5]
    VT_d_score <- page[k,6]
    PB_d_score <- page[k,7]
    HB_d_score <- page[k,8]
    
    FX_e_pen <- page[k+1,3]
    PH_e_pen <- page[k+1,4]
    SR_e_pen <- page[k+1,5]
    VT_e_pen <- page[k+1,6]
    PB_e_pen <- page[k+1,7]
    HB_e_pen <- page[k+1,8]
    
    person_final_df <- data.frame(Name, Country = noc, FX_d_score,PH_d_score,SR_d_score,VT_d_score,PB_d_score,HB_d_score,
                                  FX_e_pen,PH_e_pen,SR_e_pen,VT_e_pen, PB_e_pen,HB_e_pen)
    m_final_ls[[length(m_final_ls)+1]] <- person_final_df
  }
}


atwp_m_final_tb <- m_final_ls %>% 
  list_rbind() %>% 
  mutate(Round = "TeamFinal", .after = 2) %>% 
  separate(FX_d_score, into = c("D_FX", "Score_FX"), sep = " ") %>% 
  separate(PH_d_score, into = c("D_PH", "Score_PH"), sep = " ") %>% 
  separate(SR_d_score, into = c("D_SR", "Score_SR"), sep = " ") %>%
  separate(VT_d_score, into = c("D_VT", "Score_VT"), sep = " ") %>% 
  separate(PB_d_score, into = c("D_PB", "Score_PB"), sep = " ") %>%
  separate(HB_d_score, into = c("D_HB", "Score_HB"), sep = " ") %>% 
  separate(FX_e_pen, into = c("E_FX", "Pen_FX"), sep = " ") %>%
  separate(PH_e_pen, into = c("E_PH", "Pen_PH"), sep = " ") %>%
  separate(SR_e_pen, into = c("E_SR", "Pen_SR"), sep = " ") %>%
  separate(VT_e_pen, into = c("E_VT", "Pen_VT"), sep = " ") %>% 
  separate(PB_e_pen, into = c("E_PB", "Pen_PB"), sep = " ") %>%
  separate(HB_e_pen, into = c("E_HB", "Pen_HB"), sep = " ") %>% 
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m") %>% 
  filter(!D == "")



### combine m_team_final and w_team_final ######################

atwp_team_final_tb <- bind_rows(atwp_m_final_tb, atwp_w_final_tb) %>% 
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
  mutate(Competition = "2023 52nd FIG Artistic Gymnastics World Championships", 
         Location = "Antwerp, Belgium",
         Date = "30 Sep 2023 - 8 Oct 2023") %>% # 加上赛事信息
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!Name:Pen)

write_csv(atwp_team_final_tb, "../cleandata/data_new/antwerp_team.csv")
