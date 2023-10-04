### BIRMINGHAM 2022 Commonwealth Games: All-Around #############################
source("function.R")
path_commaa <- "../pdf/22comm_aa"
comm_aa_ls_raw <- get_gym_tables(path_commaa)

###m_aa_final###################################################################
commaa1 <- comm_aa_ls_raw[[1]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2))


commaa1_odd <- commaa1[seq(1, nrow(commaa1), 2), ]
commaa1_even <- commaa1[seq(2, nrow(commaa1), 2), ] %>% 
  select(c(2,seq(6,16,2)))

rank_cols <- c("V7","V9","V11","V13","V15","V17")
rank_cols_new <- c("Rank_FX","Rank_PH","Rank_SR","Rank_VT","Rank_PB","Rank_HB")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

commaa1_wide <- left_join(commaa1_odd, commaa1_even, join_by(V2)) %>% 
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
commaa1_long <- commaa1_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAfinal")


###w_aa_final###################################################################

commaa3 <- comm_aa_ls_raw[[3]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2))

commaa3_odd <- commaa3[seq(1, nrow(commaa3), 2), ]
commaa3_even <- commaa3[seq(2, nrow(commaa3), 2), ] %>% 
  select(c(2,seq(6,12,2)))

rank_cols <- c("V7","V9","V11","V13")
rank_cols_new <- c("Rank_VT","Rank_UB","Rank_BB","Rank_FX")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

commaa3_wide <- left_join(commaa3_odd, commaa3_even, join_by(V2)) %>% 
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

commaa3_long <- commaa3_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w", Round = "AAfinal")



### m_aa_qual###################################################################
wide_cols <- c("Bib","Name","Country","VT","Rank_VT1")
rank_cols <- c("Rank_FX","Rank_PH","Rank_SR","Rank_VT","Rank_PB","Rank_HB")

commaa2_ls <- comm_aa_ls_raw[[2]]

commaa2_ls[[1]] <- commaa2_ls[[1]] %>% 
  select(c(2,3,4,12,13))
commaa2_ls[[2]] <- commaa2_ls[[2]] %>% 
  select(c(1,2,3,12,13))

colnames(commaa2_ls[[1]]) <- colnames(commaa2_ls[[2]]) <- wide_cols

commaa2 <- bind_rows(commaa2_ls[[1]], commaa2_ls[[2]]) %>% 
  mutate(Bib = ifelse(Bib == "", lag(Bib), Bib))
commaa2_odd <- commaa2[seq(1, nrow(commaa2), 2), ] %>% 
  separate(VT, into = c("D_VT1", "Score_VT1"), sep = " ")
commaa2_even <- commaa2[seq(2, nrow(commaa2), 2), ] %>% 
  select(Bib, VT) %>% 
  separate(VT, into = c("E_VT1", "Pen_VT1"), sep = " ")
commaa2_wide <- left_join(commaa2_odd, commaa2_even, join_by(Bib))

commaa2_long <- commaa2_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAqual")

### w_aa_qual###################################################################
wide_cols <- c("Bib","Name","Country","VT","Rank_VT1")
rank_cols <- c("Rank_FX","Rank_PH","Rank_SR","Rank_VT","Rank_PB","Rank_HB")

commaa4_ls <- comm_aa_ls_raw[[4]]

commaa4_ls[[1]] <- commaa4_ls[[1]] %>% 
  select(c(2,3,4,6,7))
commaa4_ls[[2]] <- commaa4_ls[[2]] %>% 
  select(c(2,3,4,6,7))
colnames(commaa4_ls[[1]]) <- colnames(commaa4_ls[[2]]) <- wide_cols

commaa4 <- bind_rows(commaa4_ls[[1]], commaa4_ls[[2]]) %>% 
  mutate(Bib = ifelse(Bib == "", lag(Bib), Bib))
commaa4_odd <- commaa4[seq(1, nrow(commaa2), 2), ] %>% 
  separate(VT, into = c("D_VT1", "Score_VT1"), sep = " ")
commaa4_even <- commaa4[seq(2, nrow(commaa2), 2), ] %>% 
  select(Bib, VT) %>% 
  separate(VT, into = c("E_VT1", "Pen_VT1"), sep = " ")
commaa4_wide <- left_join(commaa4_odd, commaa4_even, join_by(Bib))
commaa4_long <- commaa4_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAqual")



### Combine 4 tables ###########################################################
Date = "29 Jul-2 Aug 2022"
Competition = "BIRMINGHAM 2022 Commonwealth Games"
Location = "Birmingham, England"

comm_aa_tb <- bind_rows(commaa1_long, commaa2_long, commaa3_long, commaa4_long) %>% 
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

write_csv(comm_aa_tb, "../cleandata/data_new/commgames_22_aa.csv")
