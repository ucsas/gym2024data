### SANTIAGO 2023 XIX Pan American Games: Teams Final Men and Women ############

library(readxl)

santiago_team_tb <- read_excel("~/Downloads/santiago_team.xlsx") %>% 
  select(-11) %>% 
  rename(Penalty = Penalty...9) %>% 
  mutate(Date = "21-25 Oct 2023",
         Competition = "SANTIAGO 2023 XIX Pan American Games",
         Round = "qual",
         Location = "Santiago, Chile",
         .after = 4) %>%
  mutate(Penalty = na_if(Penalty, "NA")) %>%
  mutate(Penalty = as.numeric(Penalty)) %>% 
  filter(Apparatus == "VT") %>% 
  mutate(Apparatus = "VT1")

write_csv(santiago_team_tb, "../cleandata/data_new/pan_am_games_23_team.csv")

