# 2022 51st FIG Artistic Gymnastics World Championships antwerp (GBR) All-Around ##

path1 <- "../pdf/antwerp_aa"
atwp_aa_ls_raw <- get_gym_tables(path1)

###m_aa_final###################################################################
atwpaa1 <- atwp_aa_ls_raw[[1]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2))

atwpaa1_odd <- atwpaa1[seq(1, nrow(atwpaa1), 2), ]
atwpaa1_even <- atwpaa1[seq(2, nrow(atwpaa1), 2), ] %>% 
  select(c(2,seq(6,16,2)))

rank_cols <- c("V7","V9","V11","V13","V15","V17")
rank_cols_new <- c("Rank_FX","Rank_PH","Rank_SR","Rank_VT","Rank_PB","Rank_HB")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

atwpaa1_wide <- left_join(atwpaa1_odd, atwpaa1_even, join_by(V2)) %>% 
  mutate(V4 = str_split(V4, " ", simplify = TRUE)[, 1]) %>% 
  separate(V6.x, into = c("D_FX", "Score_FX"), sep = " ") %>% 
  separate(V8.x, into = c("D_PH", "Score_PH"), sep = " ") %>% 
  separate(V10.x, into = c("D_SR", "Score_SR"), sep = " ") %>%
  separate(V12.x, into = c("D_VT", "Score_VT"), sep = " ") %>%
  separate(V14.x, into = c("D_PB", "Score_PB"), sep = " ") %>%
  separate(V16.x, into = c("D_HB", "Score_HB"), sep = " ") %>%
  separate(V6.y, into = c("E_FX", "Pen_FX"), sep = " ") %>%
  separate(V8.y, into = c("E_PH", "Pen_PH"), sep = " ") %>%
  separate(V10.y, into = c("E_SR", "Pen_SR"), sep = " ") %>%
  separate(V12.y, into = c("E_VT", "Pen_VT"), sep = " ") %>%
  separate(V14.y, into = c("E_PB", "Pen_PB"), sep = " ") %>%
  separate(V16.y, into = c("E_HB", "Pen_HB"), sep = " ") %>%
  mutate(across(all_of(rank_cols), ~str_replace_all(., "\\(|\\)", ""))) %>% 
  rename(!!!name_changes1) %>% 
  rename(!!!name_changes2) %>% 
  select(!c(V1,V5,V18))

atwpaa1_long <- atwpaa1_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAfinal")


###w_aa_final###################################################################

atwpaa3 <- atwp_aa_ls_raw[[2]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2))
atwpaa3_odd <- atwpaa3[seq(1, nrow(atwpaa1), 2), ]
atwpaa3_even <- atwpaa3[seq(2, nrow(atwpaa1), 2), ] %>% 
  select(c(2,seq(6,12,2)))

rank_cols <- c("V7","V9","V11","V13")
rank_cols_new <- c("Rank_VT","Rank_UB","Rank_BB","Rank_FX")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

atwpaa3_wide <- left_join(atwpaa3_odd, atwpaa3_even, join_by(V2)) %>% 
  mutate(V4 = str_split(V4, " ", simplify = TRUE)[, 1]) %>% 
  separate(V6.x, into = c("D_VT", "Score_VT"), sep = " ") %>% 
  separate(V8.x, into = c("D_UB", "Score_UB"), sep = " ") %>% 
  separate(V10.x, into = c("D_BB", "Score_BB"), sep = " ") %>%
  separate(V12.x, into = c("D_FX", "Score_FX"), sep = " ") %>% 
  separate(V6.y, into = c("E_VT", "Pen_VT"), sep = " ") %>%
  separate(V8.y, into = c("E_UB", "Pen_UB"), sep = " ") %>%
  separate(V10.y, into = c("E_BB", "Pen_BB"), sep = " ") %>%
  separate(V12.y, into = c("E_FX", "Pen_FX"), sep = " ") %>% 
  mutate(across(all_of(rank_cols), ~str_replace_all(., "\\(|\\)", ""))) %>% 
  rename(!!!name_changes1) %>% 
  rename(!!!name_changes2) %>% 
  select(!c(V1,V5,V14))

atwpaa3_long <- atwpaa3_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w", Round = "AAfinal")


### Combine 4 tables ############################################################
Date = "30 Sep 2023 - 8 Oct 2023"
Competition = "2023 52nd FIG Artistic Gymnastics World Championships"
Location = "Antwerp, Belgium"

atwp_aa_tb <- bind_rows(atwpaa1_long, atwpaa3_long) %>% 
  filter(D != "DNS") %>% # 删掉D==DNS的列
  mutate(Penalty = as.numeric(Pen), # 把D E Score Rank转成数值
         Rank = as.numeric(Rank), # D E P列重命名
         E_Score = as.numeric(E),
         D_Score = as.numeric(D),
         Score = as.numeric(Score),
         Penalty = ifelse(Penalty < 0, -Penalty, Penalty)) %>% # 把负号的penalty变正
  mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), # 拆分姓名
                             ~ paste(.x, collapse = " "))) %>% 
  mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), 
                            ~ paste(.x, collapse = " "))) %>% 
  mutate(Date = Date, Competition = Competition, Location = Location) %>% # 加上赛事信息
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% # 列顺序relocate
  select(!Bib:Pen)

# 处理Melanie De Jesus Dos Santos的姓名异常
atwp_aa_tb <- atwp_aa_tb %>%
  mutate(
    FirstName = if_else(LastName == "JESUS SANT", "Melanie", FirstName),
    LastName = if_else(LastName == "JESUS SANT", "DE JESUS DOS SANTOS", LastName)
  )

write_csv(atwp_aa_tb, "../cleandata/data_new/antwerp_aa.csv")
