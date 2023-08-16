### CHENGDU 2023 FISH World University Games: All-Around Finals ################

path_uniaa <- "../pdfs_2023/23univgames_aa"
univ_aa_ls_raw <- get_gym_tables(path_uniaa)

###m_aa_final###################################################################
univaa1 <- univ_aa_ls_raw[[1]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2)) %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2)) %>% 
  mutate(V3 = ifelse(lead(V5) == "" & V5 != "", paste(V3, lead(V3)), V3)) %>% 
  filter(V5 != "")


univaa1_odd <- univaa1[seq(1, nrow(univaa1), 2), ]
univaa1_even <- univaa1[seq(2, nrow(univaa1), 2), ] %>% 
  select(c(2,seq(6,16,2)))

rank_cols <- c("V7","V9","V11","V13","V15","V17")
rank_cols_new <- c("Rank_FX","Rank_PH","Rank_SR","Rank_VT","Rank_PB","Rank_HB")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

univaa1_wide <- left_join(univaa1_odd, univaa1_even, join_by(V2)) %>% 
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

univaa1_long <- univaa1_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAfinal")



###w_aa_final###################################################################

univaa3 <- univ_aa_ls_raw[[2]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2)) %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2)) %>% 
  mutate(V3 = ifelse(lead(V5) == "" & V5 != "", paste(V3, lead(V3)), V3)) %>% 
  filter(V5 != "")

univaa3_odd <- univaa3[seq(1, nrow(univaa1), 2), ]
univaa3_even <- univaa3[seq(2, nrow(univaa1), 2), ] %>% 
  select(c(2,seq(6,12,2)))

rank_cols <- c("V7","V9","V11","V13")
rank_cols_new <- c("Rank_VT","Rank_UB","Rank_BB","Rank_FX")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

univaa3_wide <- left_join(univaa3_odd, univaa3_even, join_by(V2)) %>% 
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

univaa3_long <- univaa3_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w", Round = "AAfinal")


### Combine 2 tables ###########################################################
Date = "1-5 Aug 2023"
Competition = "2023 FISH World University Games"
Location = "Chengdu, China"

univ_aa_tb <- bind_rows(univaa1_long, univaa3_long) %>% 
  filter(D != "DNS") %>% # 删掉D==DNS的列
  mutate(Penalty = as.numeric(Pen), # 把D E Score Rank转成数值
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
  select(!Bib:Pen)

write_csv(univ_aa_tb, "../cleandata/data_new/univgames_aa.csv")

