# 51st FIG Artistic Gymnastics World Championships LIVERPOOL (GBR) All-Around

path1 <- "../pdfs_2023/liverpool_aa"
lv_aa_ls_raw <- get_gym_tables(path1)

###m_aa_final###################################################################
lvaa1 <- lv_aa_ls_raw[[1]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2))

lvaa1_odd <- lvaa1[seq(1, nrow(lvaa1), 2), ]
lvaa1_even <- lvaa1[seq(2, nrow(lvaa1), 2), ] %>% 
  select(c(2,seq(6,16,2)))

rank_cols <- c("V7","V9","V11","V13","V15","V17")
rank_cols_new <- c("Rank_FX","Rank_PH","Rank_SR","Rank_VT","Rank_PB","Rank_HB")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

lvaa1_wide <- left_join(lvaa1_odd, lvaa1_even, join_by(V2)) %>% 
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

lvaa1_long <- lvaa1_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAfinal")


###w_aa_final###################################################################

lvaa3 <- lv_aa_ls_raw[[3]][[1]] %>% 
  mutate(V2 = ifelse(V2 == "", lag(V2), V2))
lvaa3_odd <- lvaa3[seq(1, nrow(lvaa1), 2), ]
lvaa3_even <- lvaa3[seq(2, nrow(lvaa1), 2), ] %>% 
  select(c(2,seq(6,12,2)))

rank_cols <- c("V7","V9","V11","V13")
rank_cols_new <- c("Rank_VT","Rank_UB","Rank_BB","Rank_FX")
name_changes1 <- setNames(rank_cols, rank_cols_new)
name_changes2 <- setNames(c("V2","V3","V4"), c("Bib","Name","Country"))

lvaa3_wide <- left_join(lvaa3_odd, lvaa3_even, join_by(V2)) %>% 
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

lvaa3_long <- lvaa3_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w", Round = "AAfinal")

### m_aa_qual###################################################################
wide_cols <- c("rank","Bib","Name","Country","FX","PH","SR","VT","PB","HB","total")
rank_cols <- c("Rank_FX","Rank_PH","Rank_SR","Rank_VT","Rank_PB","Rank_HB")

lvaa2_ls <- lv_aa_ls_raw[[2]]

lvaa2_ls[[1]] <- lvaa2_ls[[1]] %>% 
  mutate(V6 = paste(V6, V7, sep = " ")) %>% 
  select(!seq(5,17,2))
lvaa2_ls[[2]] <- lvaa2_ls[[2]] %>% 
  select(!seq(5,17,2))
lvaa2_ls[[3]] <- lvaa2_ls[[3]] %>% 
  select(!c("V5","V7","V9","V12","V14"))
lvaa2_ls[[4]] <- lvaa2_ls[[4]] %>% 
  select(!c("V5","V8","V10","V14"))
colnames(lvaa2_ls[[1]]) <- colnames(lvaa2_ls[[2]]) <- 
  colnames(lvaa2_ls[[3]]) <- colnames(lvaa2_ls[[4]]) <- wide_cols

lvaa2 <- bind_rows(lvaa2_ls[[1]], lvaa2_ls[[2]], lvaa2_ls[[3]], lvaa2_ls[[4]]) %>% 
  mutate(Bib = ifelse(Bib == "", lag(Bib), Bib))
lvaa2_odd <- lvaa2[seq(1, nrow(lvaa2), 2), ]
lvaa2_even <- lvaa2[seq(2, nrow(lvaa2), 2), ] %>% 
  select(c(2,5:10))



lvaa2_wide <- left_join(lvaa2_odd, lvaa2_even, join_by(Bib)) %>% 
  mutate(Country = str_split(Country, " ", simplify = TRUE)[, 1]) %>% 
  separate(FX.x, into = c("D_FX", "Score_FX", "Rank_FX"), sep = " ") %>% 
  separate(PH.x, into = c("D_PH", "Score_PH", "Rank_PH"), sep = " ") %>% 
  separate(SR.x, into = c("D_SR", "Score_SR", "Rank_SR"), sep = " ") %>%
  separate(VT.x, into = c("D_VT", "Score_VT", "Rank_VT"), sep = " ") %>%
  separate(PB.x, into = c("D_PB", "Score_PB", "Rank_PB"), sep = " ") %>%
  separate(HB.x, into = c("D_HB", "Score_HB", "Rank_HB"), sep = " ") %>%
  separate(FX.y, into = c("E_FX", "Pen_FX"), sep = " ") %>%
  separate(PH.y, into = c("E_PH", "Pen_PH"), sep = " ") %>%
  separate(SR.y, into = c("E_SR", "Pen_SR"), sep = " ") %>%
  separate(VT.y, into = c("E_VT", "Pen_VT"), sep = " ") %>%
  separate(PB.y, into = c("E_PB", "Pen_PB"), sep = " ") %>%
  separate(HB.y, into = c("E_HB", "Pen_HB"), sep = " ") %>% 
  mutate(across(all_of(rank_cols), ~str_replace_all(., "\\(|\\)", ""))) %>% 
  select(!c("rank","total"))

lvaa2_long <- lvaa2_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m", Round = "AAqual")

### w_aa_qual###################################################################
wide_cols <- c("rank","Bib","Name","Country","VT","UB","BB","FX","total")
rank_cols <- c("Rank_VT","Rank_UB","Rank_BB","Rank_FX")

lvaa4_p1 <- lv_aa_ls_raw[[4]][[1]] %>% 
  mutate(V6 = paste(V6, V7, sep = " ")) %>% 
  mutate(V8 = paste(V8, V9, sep = " ")) %>%
  mutate(V10 = paste(V10, V11, sep = " ")) %>%
  mutate(V12 = paste(V12, V13, sep = " ")) %>% 
  select(!seq(5,13,2))

lvaa4_p2 <- lv_aa_ls_raw[[4]][[2]] %>% 
  mutate(V6 = paste(V6, V7, sep = " ")) %>% 
  select(!seq(5,13,2))
lvaa4_p3 <- lv_aa_ls_raw[[4]][[3]] %>% 
  select(!c(6,8,11))
lvaa4_p4 <- lvaa4_ls[[4]] %>% 
  select(!seq(5,11,2))
lvaa4_p5 <- lvaa4_ls[[5]] %>% 
  select(!c(5,9,11))
colnames(lvaa4_p1) <- colnames(lvaa4_p2) <- colnames(lvaa4_p3) <- 
  colnames(lvaa4_p4) <- colnames(lvaa4_p5) <-  wide_cols
lvaa4 <- bind_rows(lvaa4_p1,lvaa4_p2,lvaa4_p3,lvaa4_p4,lvaa4_p5) %>% 
  mutate(Bib = ifelse(Bib == "", lag(Bib), Bib))
lvaa4_odd <- lvaa4[seq(1, nrow(lvaa4), 2), ]
lvaa4_even <- lvaa4[seq(2, nrow(lvaa4), 2), ] %>% 
  select(c(2,5:8))

lvaa4_wide <- left_join(lvaa4_odd, lvaa4_even, join_by(Bib)) %>% 
  mutate(Country = str_split(Country, " ", simplify = TRUE)[, 1]) %>% 
  separate(VT.x, into = c("D_VT", "Score_VT", "Rank_VT"), sep = " ") %>%
  separate(UB.x, into = c("D_UB", "Score_UB", "Rank_UB"), sep = " ") %>%
  separate(BB.x, into = c("D_BB", "Score_BB", "Rank_BB"), sep = " ") %>%
  separate(FX.x, into = c("D_FX", "Score_FX", "Rank_FX"), sep = " ") %>% 
  separate(VT.y, into = c("E_VT", "Pen_VT"), sep = " ") %>%
  separate(UB.y, into = c("E_UB", "Pen_UB"), sep = " ") %>%
  separate(BB.y, into = c("E_BB", "Pen_BB"), sep = " ") %>% 
  separate(FX.y, into = c("E_FX", "Pen_FX"), sep = " ") %>% 
  mutate(across(all_of(rank_cols), ~str_replace_all(., "\\(|\\)", ""))) %>% 
  select(!c("rank","total"))

lvaa4_long <- lvaa4_wide %>%  
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w", Round = "AAqual")
  

### Combin 4 tables ############################################################
Date = "29 Oct 2022 - 6 Nov 2022"
Competition = "51st FIG Artistic Gymnastics World Championships"
Location = "Liverpool, England"

lv_aa_tb <- bind_rows(lvaa1_long, lvaa2_long, lvaa3_long, lvaa4_long) %>% 
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

write_csv(lv_aa_tb, "../cleandata/data_new/liverpool_aa.csv")

