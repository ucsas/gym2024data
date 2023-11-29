source("function.R")


### 2023 Central American and Caribbean Games San Salvador #####################

ca_path <- "../pdf/central_am"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "E_Score","D_Score", 
                  "Penalty", "Score")

ca_ls_raw <- get_gym_tables(folder_path = ca_path) %>% 
  unlist(recursive = F, use.names = TRUE)
ca_ls <- align_tables(raw_table_list = ca_ls_raw, col_names = col_names_vt)
ca_tb <- transform_table(table_list = ca_ls, 
                         Date = "24-28 Jun 2023", 
                         Competition = "2023 Central American and Caribbean Games", 
                         Location = "San Salvador, El Salvador")
write_csv(ca_tb, "../cleandata/data_new/central_america.csv")


### SANTIAGO 2023 XIX Pan American Games #######################################

pam_path <- "../pdf/23pan_am_event"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "E_Score","D_Score", 
                  "Penalty", "Score")

pam_ls_raw <- get_gym_tables(folder_path = pam_path) %>% 
  unlist(recursive = F, use.names = TRUE)
pam_ls <- align_tables(raw_table_list = pam_ls_raw, col_names = col_names_vt)
pam_tb <- transform_table(table_list = pam_ls, 
                          Date = "21-25 Oct 2023", 
                          Competition = "SANTIAGO 2023 XIX Pan American Games", 
                          Location = "Santiago, Chile") %>% 
  filter(!is.na(Score))

pam_tb <- pam_tb %>% 
  mutate(FirstName = ifelse(LastName == "VILLAVERDE", "Yohendry", FirstName)) %>% 
  mutate(FirstName = ifelse(LastName == "VELASQUEZ CANDRAY", "Pablo Natanael", FirstName)) %>% 
  mutate(FirstName = ifelse(LastName == "RODRIGUEZ JOHANNING", "Anelena", FirstName)) %>% 
  mutate(FirstName = ifelse(LastName == "PINTO ADASME", "Makarena", FirstName)) %>% 
  mutate(FirstName = ifelse(LastName == "DE LA CRUZ", "	Alejandro", FirstName))

write_csv(pam_tb, "../cleandata/data_new/pan_am_games_23_event.csv")


### 2022 Senior European Championships MUNICH (GER) ############################

eu22_path <- "../pdf/europe_22"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "E_Score","D_Score", 
                  "Penalty", "Score")
eu_ls_raw <- get_gym_tables(eu22_path) %>% 
  unlist(recursive = F, use.names = TRUE)
## map(eu_ls_raw, ~ map(., ncol))
eu_ls <- align_tables(eu_ls_raw, col_names_vt)
## map(eu_ls, ncol)
eu_tb <- transform_table(table_list = eu_ls, 
                         Date = "Aug 2022", 
                         Competition = "2022 Senior European Championships ", 
                         Location = "Munich, Germany") %>% 
  mutate(Date = case_when(
    Gender == "m" ~ "18-21 Aug 2022",
    Gender == "w" ~ "11-14 Aug 2022",
    TRUE ~ Date
  ))
write_csv(eu_tb, "../cleandata/data_new/european_2022.csv")



### 2023 Senior European Championships ANTALYA (TUR) ###########################

eu23_path <- "../pdf/europe_23"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "E_Score","D_Score", 
                  "Penalty", "Score")
eu_ls_raw <- get_gym_tables(eu23_path) %>% 
  unlist(recursive = F, use.names = TRUE)
## map(eu_ls_raw, ~ map(., ncol))
eu_ls <- align_tables(eu_ls_raw, col_names_vt)
## map(eu_ls, ncol)
eu_tb <- transform_table(table_list = eu_ls, 
                         Date = "11-16 Apr 2023", 
                         Competition = "2023 Senior European Championships", 
                         Location = "Antalya, Turkey")
write_csv(eu_tb, "../cleandata/data_new/european_2023.csv")



### 2023 Varna World Challenge Cup #############################################
## Note: the get_gym_tables() function does not apply anymore after some 
## changes of function code, any changes are made directly on clean data
## the following paragraph of codes may not run.

vn_path <- "../pdf/varna"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "D_Score","E_Score", 
                  "Penalty", "Score")
vn_ls_raw <- get_gym_tables(vn_path)
vn_ls <- unlist(vn_ls_raw, recursive = F, use.names = TRUE)
vn_ls[["m_qual_SR.pdf"]] <- vn_ls[["m_qual_SR.pdf"]] %>% 
  separate(col = V8, into = c("V8", "V9"), sep = " ", fill = "right")
vn_ls <- map(vn_ls, remove_column_if_q)
vn_ls[["w_qual_VT.pdf"]] <- vn_ls[["w_qual_VT.pdf"]][, -5]
vn_ls[["m_final_VT.pdf"]] <- 
  vn_ls[["m_final_VT.pdf"]][, -ncol(vn_ls[["m_final_VT.pdf"]])]
vn_ls[["m_final_FX.pdf"]] <- vn_ls[["m_final_FX.pdf"]] %>% 
  mutate_all(~str_replace_all(., ",", "."))

vt_names <- grepl("VT", names(vn_ls))
vn_ls[!vt_names] <- map(vn_ls[!vt_names], split_column_vn)
vn_ls[vt_names] <- map(vn_ls[vt_names], process_df)

vn_ls_n <- align_tables_vn(raw_table_list = vn_ls, col_names = col_names_vt)
vn_tb <- transform_table(table_list = vn_ls_n, 
                         Date = "25-28 May 2023", 
                         Competition = "2023 Varna World Challenge Cup Results", 
                         Location = "Varna, Bulgaria")
write_csv(vn_tb, "../cleandata/data_new/varna.csv")


### 2023 Varna Challenge Cup Modification 1: Switch D_Score and E_Score####
vn_tb <- read_csv("../cleandata/data_new/varna.csv")
vn_tb <- vn_tb %>% 
  mutate(D = E_Score, E_Score = D_Score, D_Score = D) %>% 
  select(!D)
write_csv(vn_tb, "../cleandata/data_new/varna.csv")

### 2023 Varna Challenge Cup Modification 2: remove "Results" from competition name 
vn_tb <- read_csv("../cleandata/data_new/varna.csv") %>% 
  mutate(Competition = "2023 Varna World Challenge Cup")
write_csv(vn_tb, "../cleandata/data_new/varna.csv")

### 2023 Varna Challenge Cup Modification 3: cancel the unintended rounding up to 2 digits for Score
vn_tb <- read_csv("../cleandata/data_new/varna.csv") %>% 
  mutate(Score = D_Score + E_Score - if_else(is.na(Penalty), 0, Penalty))
write_csv(vn_tb, "../cleandata/data_new/varna.csv")

vn_tb <- read_csv("../cleandata/data_new/varna.csv") %>% 
  mutate(LastName = if_else(FirstName == "Daria Angelina", "HARTMANN", LastName))
write_csv(vn_tb, "../cleandata/data_new/varna.csv")

### 2023 Tel Aviv Challenge Cup ################################################

url1 <- "https://thegymter.net/2023/06/05/2023-tel-aviv-challenge-cup-mens-results/"
url2 <- "https://thegymter.net/2023/06/05/2023-tel-aviv-challenge-cup-results/"
## to replace full names in source data with abbreviations
country_abbr = c("AUS", "AUT", "AZE", "BEL", "CRO", "ESP", "FIN", "FRA", "GBR", 
                 "GER", "HKG", "HUN", "ISR", "RSA", "SLO", "SVK", "TUR")
full_name <- c("Australia", "Austria", "Azerbaijan", "Belgium", "Croatia", 
               "Spain", "Finland", "France", "Great Britain", "Germany",
               "Hong Kong", "Hungary", "Israel", "South Africa", 
               "Slovenia", "Slovakia", "Türkiye")
## result_df is used as an input value of get_web_tb() function
result_df <- data.frame(Country_Abbr = country_abbr, Full_Name = full_name)

tel_m <- get_web_tb(url1, gender = "m")
tel_w <- get_web_tb(url2, gender = "w")
tel_tb_ls <- c(tel_m, tel_w) %>% 
  update_vt()
tel_tb <- transform_web_tb(table_list = tel_tb_ls, 
                           Date = "1-4 Jun 2023",
                           Competition = "2023 Tel Aviv World Challenge Cup",
                           Location = "Tel Aviv, Israel",
                           NOCkey = result_df) %>% 
  select(-Nation)
write_csv(tel_tb, "../cleandata/data_new/telaviv.csv")


### 2023 Osijek Challenge Cup ##################################################

url_m <- "https://thegymter.net/2023/06/13/2023-osijek-challenge-cup-mens-results/"
url_w <- "https://thegymter.net/2023/06/12/2023-osijek-challenge-cup-results/"

## to replace full names in source data with abbreviations
noc_key <- read.csv("noc_key.csv")
new_rows <- data.frame(
  Full_Name = c("Belarus*", "Norway", "Albania", "Armenia", "Czech Republic", "Great Britain", "Hong Kong", "Turkey", "South Korea", "Russia*"),
  Country_Abbr = c("BLR", "NOR", "ALB", "ARM", "CZE", "GBR", "HKG", "TUR", "KOR", "RUS")
)
noc_data <- rbind(noc_key, new_rows)

osi_m <- get_web_tb(url_m, gender = "m")
osi_w <- get_web_tb(url_w, gender = "w")
osi_tb_ls <- c(osi_m, osi_w) %>% 
  update_vt() %>% 
  map(convert_dns_or_dash_to_empty_string) %>% 
  map( ~mutate_at(.x, vars(Rank, D, E, Total), as.numeric)) %>% 
  map( ~if ("Average" %in% colnames(.x)) {
    select(.x, -Average)
  } else {
    .x
  })

osi_tb <- transform_web_tb(table_list = osi_tb_ls, 
                           Date = "8-11 Jun 2023",
                           Competition = "2023 Osijek World Challenge Cup",
                           Location = "Osijek, Croatia",
                           NOCkey = noc_data) %>% 
  select(-Nation)
write_csv(osi_tb, "../cleandata/data_new/osijek.csv")


### 2023 Cottbus Apparatus World Cup ###########################################

area <- list(c(147, 53, 739, 568))
folder_path <- "../pdf/cottbus"
cottbus_tb_raw <- extract_data_cot(folder_path, area)
cottbus_tb <- process_data_cot(cottbus_tb_raw, "Cottbus",
                                   Date = "23-26 Feb 2023", 
                                   Competition = "2023 Cottbus World Cup", 
                                   Location = "Cottbus, Germany")
write_csv(cottbus_tb, "../cleandata/data_new/cottbus.csv")  

## EnBW DTB Pokal Team Challenge 2023
folder_path <- "../pdf/dtb_pokal"
dtb_tb_raw <- extract_data_cot(folder_path, area)
## used a different split name algorithm since names here contains German letters
dtb_raw_name <- dtb_tb_raw %>% 
  mutate(
    split_name = str_split_fixed(NAME, ", ", n = 2),
    LastName = split_name[, 1],
    FirstName = split_name[, 2]
  ) %>% 
  select(-split_name)
dtb_tb <- process_data_cot(dtb_raw_name, type = "dtb",
                                   Date = "17-19 Mar 2023",
                                   Competition = "EnBW DTB Pokal Team Challenge 2023", 
                                   Location = "Stuttgart, Germany")
write_csv(dtb_tb, "../cleandata/data_new/dtb_pokal.csv")


### 2022 LIVERPOOL 51st FIG Artistic Gymnastics World Championships ############
lvp_path <- "../pdf/liverpool" 
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "D_Score","E_Score", 
                  "Penalty", "Score")
lvp_ls_raw <- get_gym_tables(folder_path = lvp_path)
lvp_ls_qr <- unlist(lvp_ls_raw, recursive = F, use.names = TRUE) %>% 
  map(remove_qr_from_total) %>% 
  imap(function(df, name) {
    if (grepl("final", name)) {
      df <- df[,-(ncol(df)-1:0)]
    }
    return(df)
  })
lvp_ls <- align_tables(raw_table_list = lvp_ls_qr, col_names = col_names_vt)
lvp_tb <- transform_table(table_list = lvp_ls, 
                         Date = "29 Oct 2022 - 6 Nov 2022", 
                         Competition = "2022 51st FIG Artistic Gymnastics World Championships", 
                         Location = "Liverpool, England") %>% 
  arrange(LastName, FirstName, Competition, Apparatus)%>%
  distinct() %>% 
  mutate(LastName = if_else(FirstName == "Kate", "McDONALD", LastName))
write_csv(lvp_tb, "../cleandata/data_new/liverpool_event.csv")


### 2023 ANTWERP 52nd FIG Artistic Gymnastics World Championships ##############
atwp_path <- "../pdf/antwerp_event" 
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "D_Score","E_Score", 
                  "Penalty", "Score")
atwp_ls_raw <- get_gym_tables(folder_path = atwp_path)
atwp_ls_qr <- unlist(atwp_ls_raw, recursive = F, use.names = TRUE) %>% 
  map(remove_qr_from_total) %>% 
  imap(function(df, name) {
    if (grepl("final", name)) {
      df <- df[,-(ncol(df)-1:0)]
    }
    return(df)
  })
atwp_ls <- align_tables(raw_table_list = atwp_ls_qr, col_names = col_names_vt)
atwp_tb <- transform_table(table_list = atwp_ls, 
                           Date = "30 Sep 2023 - 8 Oct 2023", 
                           Competition = "2023 52nd FIG Artistic Gymnastics World Championships", 
                           Location = "Antwerp, Belgium") %>% 
  arrange(LastName, FirstName, Competition, Apparatus)

# 单独处理两个LastName提取有问题的运动员
atwp_tb <- atwp_tb %>%
  mutate(LastName = case_when(
    LastName == "" & FirstName == "Kate" ~ "McDONALD",
    TRUE ~ LastName
  )) %>% 
  mutate(LastName = case_when(
    LastName == "" & FirstName == "Vi Luong" ~ "VAN",
    TRUE ~ LastName
  )) %>% 
  arrange(LastName, FirstName, Competition, Apparatus)

# 对数据框atwp_tb，检查如果有完全相同的行，则删去其中一行
atwp_tb <- atwp_tb %>%
  distinct()

write_csv(atwp_tb, "../cleandata/data_new/antwerp_event.csv")



### CHENGDU 2023 FISU World University Games ###################################
uni_path <- "../pdf/23univgames"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "D_Score","E_Score", 
                  "Penalty", "Score")

uni_ls_raw <- get_gym_tables(folder_path = uni_path) %>% 
  unlist(recursive = F, use.names = TRUE)
uni_ls <- align_tables(raw_table_list = uni_ls_raw, col_names = col_names_vt)
uni_tb <- transform_table(table_list = uni_ls, 
                          Date = "1-5 Aug 2023", 
                          Competition = "2023 FISU World University Games", 
                          Location = "Chengdu, China")
## 修改总分Score有时舍弃最后一位小数的问题
uni_tb <- uni_tb %>% 
  mutate(Score = D_Score + E_Score - if_else(is.na(Penalty), 0, Penalty))
write_csv(uni_tb, "../cleandata/data_new/univgames_23.csv")



### HANGZHOU 2022 19th Asian Games #############################################
asian_path <- "../pdf/23asiangames_event"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "D_Score","E_Score", 
                  "Penalty", "Score")

asian_ls_raw <- get_gym_tables(folder_path = asian_path) %>% 
  unlist(recursive = F, use.names = TRUE)

asian_ls_raw1

## 杭州亚运和成都大运会的区别在于，final最后多一列qual rank，VT final还多一列start order
asian_ls_raw1 <- imap(asian_ls_raw, ~ if (grepl("final_VT", .y)) select(.x, -ncol(.x)) else .x) %>% 
  imap(~ if (grepl("final", .y)) select(.x, -ncol(.x)) else .x)

asian_ls <- align_tables(raw_table_list = asian_ls_raw1, col_names = col_names_vt)

asian_tb <- transform_table(table_list = asian_ls, 
                            Date = "24-29 Sep 2023", 
                            Competition = "HANGZHOU 2022 19th Asian Games", 
                            Location = "Hangzhou, China") %>% 
  filter(!D_Score == 0) %>% 
  mutate(LastName = ifelse(FirstName == "De Leon Justine Ace", "DE LEON", LastName)) %>%
  mutate(FirstName = ifelse(LastName == "DE LEON", "Justine Ace", FirstName)) %>% 
  mutate(LastName = ifelse(FirstName == "Thanh Tung", "LE", LastName))

write_csv(asian_tb, "../cleandata/data_new/asiangames_23.csv")


### BIRMINGHAM 2022 Commonwealth Games #########################################
## 和利物浦的51st FIG Artistic Gymnastics World Championships格式相同

comm_path <- "../pdf/22commgames"
col_names_vt <- c("Rank", "Bib", "Name", "NOC", "vault", "D_Score","E_Score", 
                  "Penalty", "Score")
comm_ls_raw <- get_gym_tables(folder_path = comm_path) %>% 
  unlist(recursive = F, use.names = TRUE)

hb_path <- "../pdf/22comm_m_qual_HB/m_qual_HB.pdf"
hb_area <- list(c(223.3226, 99.9136, 680.9744, 494.8661))
comm_m_qual_hb <- extract_tables(hb_path, area = hb_area, guess = FALSE, 
                                 output = "matrix") %>% 
  map(as.data.frame)
comm_ls_raw$m_qual_hb <- comm_m_qual_hb[[1]]

comm_ls_qr <- comm_ls_raw %>%
  map(remove_qr_from_total) %>% 
  imap(function(df, name) {
    if (grepl("final", name)) {
      df <- df[,-(ncol(df)-1:0)]
    }
    return(df)
  })
comm_ls <- align_tables(raw_table_list = comm_ls_qr, col_names = col_names_vt)
comm_tb <- transform_table(table_list = comm_ls, 
                           Date = "29 Jul-2 Aug 2022", 
                           Competition = "BIRMINGHAM 2022 Commonwealth Games", 
                           Location = "Birmingham, England") %>% 
  mutate(LastName = if_else(FirstName == "Kate", "McDONALD", LastName))
  
write_csv(comm_tb, "../cleandata/data_new/commgames_22.csv")


### SAN JOSE 2023 U.S. Championships ###########################################
m_path <- "../pdf/23uschamps/m_23champs_sraa.pdf"
w_path <- "../pdf/23uschamps/w_23champs_sraa.pdf"
Date = "26-27 Aug 2023"
Competition = "2023 U.S. Championships"
Location = "San Jose, CA"
champ23_tb <- extract_uschampionship_data(m_path, w_path, Date, Competition, Location)
write_csv(champ23_tb, "../cleandata/data_new/uschamps_23.csv")


### TAMPA 2022 U.S. Championships ##############################################
m_path <- "../pdf/22uschamps/m_22champs_sraa.pdf"
w_path <- "../pdf/22uschamps/w_22champs_sraa.pdf"
Date = "19-20 Aug 2022"
Competition = "2022 U.S. Championships"
Location = "Tampa, FL"
champ22_tb <- extract_uschampionship_data(m_path, w_path, Date, Competition, Location)
write_csv(champ22_tb, "../cleandata/data_new/uschamps_22.csv")

### 2023 Winter Cup ###############################################################
m_path <- "../pdf/23wintercup/m_23wc_aa.pdf"
w_path <- "../pdf/23wintercup/w_23wc_sr.pdf"
Date = "24-26 Feb 2023"
Competition = "2023 Winter Cup"
Location = "Louisville, Kentucky"
wc23_tb <- extract_wintercup_data(m_path, w_path, Date, Competition, Location)
write_csv(wc23_tb, "../cleandata/data_new/wintercup_23.csv")

### 2022 Winter Cup ###############################################################
m_path <- "../pdf/22wintercup/m_22wc_sraa.pdf"
w_path <- "../pdf/22wintercup/w_22wc_sr.pdf"
Date = "25-27 Feb 2022"
Competition = "2022 Winter Cup"
Location = "Frisco, Texas"
wc22_tb <- extract_wintercup_data(m_path, w_path, Date, Competition, Location)
write_csv(wc22_tb, "../cleandata/data_new/wintercup_22.csv")

### 2023 Core Hydration Classic ################################################
m_path <- "../pdf/23classic/m_23classic_sr.pdf"
w_path <- "../pdf/23classic/w_23classic_sr.pdf"
Date = "4-6 Aug 2023"
Competition = "2023 Core Hydration Classic"
Location = "Hoffman Estates, Illinois"
classic23_tb <- extract_wintercup_data(m_path, w_path, Date, Competition, Location)
write_csv(classic23_tb, "../cleandata/data_new/classic_23.csv")

### 2022 U.S. Classic ##########################################################
m_path <- "../pdf/22classic/m_22classic_sr.pdf"
w_path <- "../pdf/22classic/w_22classic_sr.pdf"
Date = "28-31 July 2022"
Competition = "2022 U.S. Classic"
Location = "West Valley City, Utah"
classic22_tb <- extract_wintercup_data(m_path, w_path, Date, Competition, Location)
classic22_tb$LastName <- gsub("\\*", "", classic22_tb$LastName)
write_csv(classic22_tb, "../cleandata/data_new/classic_22.csv")