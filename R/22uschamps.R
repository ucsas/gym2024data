library(pdftools)
library(tabulizer)
library(tidyverse)

setwd("/Users/minzefang/gym2024data/R")
# source("function.R")

m_path <- "../pdf/22uschamps/m_22champs_sraa.pdf"
w_path <- "../pdf/22uschamps/w_22champs_sraa.pdf"

us_area=list(c(106.851163, 9.628874, 763.505728, 613.347834))

m_champ22_ls <- extract_tables(m_path, guess = F, area = us_area)
w_champ22_ls <- extract_tables(w_path, guess = F, area = us_area)

w_qual_ls <- list()
w_final_ls <- list()
m_qual_ls <- list()
m_final_ls <- list()

### for women ##################################################################
for (j in 1:length(w_champ22_ls)) {
  page <- w_champ22_ls[[j]]
  for (k in seq(2, nrow(page), 7)) {
    name <- page[k,3]
    Round <- page[k,4]
    score_VT <- page[k,6]
    score_UB <- page[k,7]
    score_BB <- page[k,8]
    score_FX <- page[k,9]
    DE_VT <- page[k+1,6]
    DE_UB <- page[k+1,7]
    DE_BB <- page[k+1,8]
    DE_FX <- page[k+1,9]
    person_qual_df <- data.frame(name, Round, score_VT, score_UB, score_BB, score_FX,
                                 DE_VT, DE_UB, DE_BB, DE_FX)
    w_qual_ls[[length(w_qual_ls)+1]] <- person_qual_df
    
    name <- page[k,3]
    Round <- page[k+3,4]
    score_VT <- page[k+3,6]
    score_UB <- page[k+3,7]
    score_BB <- page[k+3,8]
    score_FX <- page[k+3,9]
    DE_VT <- page[k+4,6]
    DE_UB <- page[k+4,7]
    DE_BB <- page[k+4,8]
    DE_FX <- page[k+4,9]
    person_final_df <- data.frame(name, Round, score_VT, score_UB, score_BB, score_FX,
                                  DE_VT, DE_UB, DE_BB, DE_FX)
    w_final_ls[[length(w_final_ls)+1]] <- person_final_df
  }
}

w_tb <- c(w_qual_ls, w_final_ls) %>% 
  bind_rows() %>% 
  separate(DE_VT, into = c("D_VT", "E_VT"), sep = " / ") %>% 
  separate(DE_UB, into = c("D_UB", "E_UB"), sep = " / ") %>% 
  separate(DE_BB, into = c("D_BB", "E_BB"), sep = " / ") %>%
  separate(DE_FX, into = c("D_FX", "E_FX"), sep = " / ") %>% 
  pivot_longer(
    cols = !1:2, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    E_Score = as.numeric(E),
    D_Score = as.numeric(D),
    Score = as.numeric(score),
    Gender = "w"
  ) %>% 
  relocate(name, Gender,Round, Apparatus, D_Score, E_Score, Score) %>% 
  select(!score:E)

## for men #####################################################################
for (j in 1:length(m_champ22_ls)) {
  page <- m_champ22_ls[[j]]
  for (k in seq(2, nrow(page), 7)) {
    name <- page[k,3]
    Round <- page[k,4]
    score_FX <- page[k,6]
    score_PH <- page[k,7]
    score_SR <- page[k,8]
    score_VT <- page[k,9]
    score_PB_HB <- page[k,10]
    DE_FX <- page[k+1,6]
    DE_PH <- page[k+1,7]
    DE_SR <- page[k+1,8]
    DE_VT <- page[k+1,9]
    DE_PB_HB <- page[k+1,10]
    B.ND_FX <- page[k+2,6]
    B.ND_PH <- page[k+2,7]
    B.ND_SR <- page[k+2,8]
    B.ND_VT <- page[k+2,9]
    B.ND_PB_HB <- page[k+2,10]
    person_qual_df <- data.frame(name, Round, score_FX, score_PH, score_SR, 
                                 score_VT, score_PB_HB,
                                 DE_FX, DE_PH, DE_SR, DE_VT, DE_PB_HB,
                                 B.ND_FX,B.ND_PH,B.ND_SR,B.ND_VT,B.ND_PB_HB)
    m_qual_ls[[length(m_qual_ls)+1]] <- person_qual_df
    
    name <- page[k,3]
    Round <- page[k+3,4]
    score_FX <- page[k+3,6]
    score_PH <- page[k+3,7]
    score_SR <- page[k+3,8]
    score_VT <- page[k+3,9]
    score_PB_HB <- page[k+3,10]
    DE_FX <- page[k+4,6]
    DE_PH <- page[k+4,7]
    DE_SR <- page[k+4,8]
    DE_VT <- page[k+4,9]
    DE_PB_HB <- page[k+4,10]
    B.ND_FX <- page[k+5,6]
    B.ND_PH <- page[k+5,7]
    B.ND_SR <- page[k+5,8]
    B.ND_VT <- page[k+5,9]
    B.ND_PB_HB <- page[k+5,10]
    person_final_df <- data.frame(name, Round, score_FX, score_PH, score_SR, 
                                  score_VT, score_PB_HB,
                                  DE_FX, DE_PH, DE_SR, DE_VT, DE_PB_HB,
                                  B.ND_FX,B.ND_PH,B.ND_SR,B.ND_VT,B.ND_PB_HB)
    m_final_ls[[length(m_final_ls)+1]] <- person_final_df
  }
}

m_tb <- c(m_qual_ls, m_final_ls) %>% 
  bind_rows() %>% 
  separate(DE_FX, into = c("D_FX", "E_FX"), sep = " / ") %>% 
  separate(DE_PH, into = c("D_PH", "E_PH"), sep = " / ") %>% 
  separate(DE_SR, into = c("D_SR", "E_SR"), sep = " / ") %>%
  separate(DE_VT, into = c("D_VT", "E_VT"), sep = " / ") %>% 
  separate(score_PB_HB, into = c("score_PB", "score_HB"), sep = " ") %>% 
  separate(DE_PB_HB, into = c("D_PB", "middle", "E_HB"), sep = " / ") %>% 
  separate(middle, into = c("E_PB", "D_HB"), sep = " ") %>% 
  separate(B.ND_PB_HB, into = c("B.ND_PB", "B.ND_HB"), sep = " ") %>% 
  separate(B.ND_FX, into = c("bonus_FX", "pen_FX"), sep = "/") %>% 
  separate(B.ND_PH, into = c("bonus_PH", "pen_PH"), sep = "/") %>% 
  separate(B.ND_SR, into = c("bonus_SR", "pen_SR"), sep = "/") %>%
  separate(B.ND_VT, into = c("bonus_VT", "pen_VT"), sep = "/") %>%
  separate(B.ND_PB, into = c("bonus_PB", "pen_PB"), sep = "/") %>%
  separate(B.ND_HB, into = c("bonus_HB", "pen_HB"), sep = "/") %>% 
  pivot_longer(
    cols = !1:2, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(
    E_Score = as.numeric(E),
    D_Score = as.numeric(D),
    bonus = as.numeric(bonus),
    Score = as.numeric(score)-bonus,
    Gender = "m"
  ) %>% 
  relocate(name, Gender, Round, Apparatus, D_Score, E_Score, Score) %>% 
  select(!score:pen)

## combine women and men #######################################################

rm(name)
champ22_tb <- bind_rows(m_tb, w_tb) %>% 
  separate(name, into = c("FirstName", "LastName"), sep = " ") %>% 
  mutate(FirstName = sub("^\\*", "", FirstName)) %>% 
  mutate(Date = "19-20 Aug 2022", Competition = "2022 U.S. Championships", 
         Location = "Tampa, FL", Country = "USA", Rank = NA,
         Penalty = round(D_Score + E_Score - Score, 3)) %>% 
  mutate(Round = ifelse(Round == "Prelims", "qual", "final")) %>% 
  mutate(Penalty = ifelse(Penalty == 0, NA, Penalty)) %>% 
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  filter(!is.na(D_Score))

write_csv(champ22_tb, "../cleandata/data_new/uschamps_22.csv")
