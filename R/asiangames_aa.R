### HANGZHOU 2022 19th Asian Games: All-Around Finals ################

path_asianaa <- "../pdf/23asiangames_aa"
asian_aa_ls_raw <- get_gym_tables(path_asianaa)

###m_aa_final###################################################################
asianaa1 <- asian_aa_ls_raw[[1]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2)) %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2)) %>% 
  mutate(V3 = ifelse(lead(V5) == "" & V5 != "", paste(V3, lead(V3)), V3)) %>% 
  filter(V5 != "")


asianaa1_odd <- asianaa1[seq(1, nrow(asianaa1), 2), ]
asianaa1_even <- asianaa1[seq(2, nrow(asianaa1), 2), ] %>% 
  select(c(2,seq(6,16,2)))

rank_cols <- c("V7","V9","V11","V13","V15","V17")
rank_cols_new <- c("Rank_FX","Rank_PH","Rank_SR","Rank_VT","Rank_PB","Rank_HB")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

asianaa1_wide <- left_join(asianaa1_odd, asianaa1_even, join_by(V2)) %>% 
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

asianaa1_long <- asianaa1_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAfinal")



###w_aa_final###################################################################

## 这块是杭州亚运会和成都大运会的不同之处，对杭州表格，extract_table()会把三列识别成一列，
## 排名和后一列的分数被识别到了同一列，故而总列书更少，而对成都仅仅两列识别成一列

asianaa3 <- asian_aa_ls_raw[[2]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2)) %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2)) %>% 
  mutate(V3 = ifelse(lead(V5) == "" & V5 != "", paste(V3, lead(V3)), V3)) %>% 
  filter(V5 != "")

asianaa3_odd <- asianaa3[seq(1, nrow(asianaa3), 2), ]
asianaa3_even <- asianaa3[seq(2, nrow(asianaa3), 2), ] %>% 
  select(c(2,6,7,8,9))


name_changes <- setNames(c("V2","V3","V4","V10"), c("Bib","Name","Country","Rank_FX"))

asianaa3_wide <- left_join(asianaa3_odd, asianaa3_even, join_by(V2)) %>% 
  separate(V6.x, into = c("D_VT", "Score_VT"), sep = " ") %>% 
  separate(V7.x, into = c("Rank_VT","D_UB", "Score_UB"), sep = " ") %>% 
  separate(V8.x, into = c("Rank_UB","D_BB", "Score_BB"), sep = " ") %>%
  separate(V9.x, into = c("Rank_BB","D_FX", "Score_FX"), sep = " ") %>% 
  separate(V6.y, into = c("E_VT", "Pen_VT"), sep = " ") %>%
  separate(V7.y, into = c("E_UB", "Pen_UB"), sep = " ") %>%
  separate(V8.y, into = c("E_BB", "Pen_BB"), sep = " ") %>%
  separate(V9.y, into = c("E_FX", "Pen_FX"), sep = " ") %>% 
  rename(!!!name_changes) %>% 
  select(!c(V1,V5,V11))

asianaa3_long <- asianaa3_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w", Round = "AAfinal") %>% 
  mutate(Rank = str_replace_all(Rank, "\\(|\\)", "")) #删掉Rank列数字外的圆括号


### Combine 2 tables ###########################################################
Date = "24-29 Sep 2023"
Competition = "HANGZHOU 2022 19th Asian Games"
Location = "Hangzhou, China"

asian_aa_tb <- bind_rows(asianaa1_long, asianaa3_long) %>% 
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

asian_aa_tb <- asian_aa_tb %>% 
  mutate(FirstName = ifelse(LastName == "BAUYRZHANOVA", "Aida", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "MUEANGPHUAN", "Sasiwimon", FirstName)) %>%
  mutate(FirstName = ifelse(LastName == "KHALIMARDEN", "Amina", FirstName))

write_csv(asian_aa_tb, "../cleandata/data_new/asiangames_aa.csv")

