ol_aa <- read_csv("../cleandata/data_old/olympics_aa.csv", show_col_types = T)
ol_aa$Rank <- as.numeric(gsub("[()]", "", ol_aa$Rank)) 
ol_aa$Penalty <- ol_aa$Penalty*(-1)
ol_aa$Round[ol_aa$Round == "qual"] <- "AAqual"
ol_aa$Round[ol_aa$Round == "final"] <- "AAfinal"

ol_event <- read_csv("../cleandata/data_old/olympics_event.csv", show_col_types = T)
names(ol_aa)[names(ol_aa) == "D-Score"] <- "D_Score"
names(ol_aa)[names(ol_aa) == "E-Score"] <- "E_Score"
ol_aa$Gender <- ifelse(ol_aa$Gender == "Women", "w", ifelse(ol_aa$Gender == "Men", "m", ol_aa$Gender))

ol_team <- read_csv("../cleandata/data_old/olympics_team.csv", show_col_types = T)
  
ol_all <- bind_rows(ol_aa, ol_event, ol_team)

combined_df_old <- ol_all %>%
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  arrange(LastName, FirstName, Competition, Apparatus) %>%
  group_by(LastName) %>%
  fill(FirstName, .direction = "downup")

write_csv(combined_df_old, file.path(path_to_dir, "data_2017_2021.csv"), na = "")
