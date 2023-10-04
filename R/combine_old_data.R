ol_aa <- read_csv("../cleandata/data_old/olympics_aa.csv", show_col_types = T)
ol_aa$Rank <- as.numeric(gsub("[()]", "", ol_aa$Rank)) 
ol_aa$Penalty <- ol_aa$Penalty*(-1)

ol_event <- read_csv("../cleandata/data_old/olympics_event.csv", show_col_types = T)
names(ol_aa)[names(ol_aa) == "D-Score"] <- "D_Score"
names(ol_aa)[names(ol_aa) == "E-Score"] <- "E_Score"
ol_aa$Gender <- ifelse(ol_aa$Gender == "Women", "w", ifelse(ol_aa$Gender == "Men", "m", ol_aa$Gender))
  
ol_all <- bind_rows(ol_aa, ol_event)

combined_df <- ol_all %>%
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, Location, 
           Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  arrange(LastName, FirstName, Competition, Apparatus) %>%
  group_by(LastName) %>%
  fill(FirstName, .direction = "downup")

write_csv(combined_df, file.path(path_to_dir, "data_2017_2021.csv"), na = "")
