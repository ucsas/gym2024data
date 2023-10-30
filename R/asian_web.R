process_web_tb_ls <- function(tables_list, gender) {
  if (gender == "m") {
    round <- c(rep("final",6), rep("qual",6))
    appar <- rep(c("FX", "PH", "SR", "VT", "PB", "HB"),2)
  } else if (gender == "w") {
    round <- c(rep("final",4), rep("qual",4))
    appar <- rep(c("VT", "UB", "BB", "FX"),2)
  } else {
    stop("Invalid gender. Please use 'm' or 'w'.")
  }
  
  ## add new column Round for each df
  tables_list <- lapply(1:length(tables_list), function(i) {
    tables_list[[i]]$Round <- round[i]
    return(tables_list[[i]])
  })
  
  ## add new column Apparatus for each df
  tables <- lapply(1:length(tables_list), function(i) {
    tables_list[[i]]$Apparatus <- appar[i]
    return(tables_list[[i]])
  }) %>% 
    lapply(function(df) {
      df$Gender <- gender
      return(df)
    })
  
  return(tables)
}


### 修改transform_web_tb函数的姓名提取算法
# 中日韩越南香港台湾是姓在前，其他皆为姓在后（包括新加坡蒙古匈牙利），
# 香港有些情况会英文名在前（当香港且姓名为四个单词，如A B C D，姓为 B，名为 C D A）
# 网页数据在最后还要把姓氏全大写，以匹配其他数据

transform_web_tb <- function(table_list, Date, Competition, Location, NOCkey) {
  tel_tb <- list_rbind(table_list)
  merged_tb <- merge(tel_tb, NOCkey, by.x = "Nation", by.y = "Full_Name", 
                     all.x = TRUE)
  
  tel_tb <- merged_tb %>% 
    mutate(Penalty = ND, D_Score = D, E_Score = E, Country = Country_Abbr, 
           Score = Total,
           Date = Date, Competition = Competition, Location = Location,
           Penalty = ifelse(Penalty < 0, -Penalty, Penalty)) %>% 
    mutate(LastName = if_else(Country %in% last_name_first_noc,
                              str_split_fixed(Athlete, " ", 2)[,1],
                              word(Athlete, -1)),
           FirstName = if_else(Country %in% last_name_first_noc,
                               str_split_fixed(Athlete, " ", 2)[,2],
                               str_replace(Athlete, LastName, ""))) %>% 
    mutate(
      # 处理HKD的特殊情况
      LastName = case_when(
        Country == "HKG" & str_count(Athlete, boundary("word")) == 4 ~ 
          word(Athlete, 2),
        TRUE ~ LastName),
      FirstName = case_when(
        Country == "HKG" & str_count(Athlete, boundary("word")) == 4 ~ 
          str_c(word(Athlete, 3), word(Athlete, 4), word(Athlete, 1), sep = " "),
        TRUE ~ FirstName
      )) %>% 
    relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
             Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
    select(!Athlete:Country_Abbr)
  
  return(tel_tb)
}



### 2022 9th Senior Artistic Gymnastics Asian Championships ####################

url_m <- "https://thegymter.net/2022/06/21/2022-asian-championships-mens-results/"
url_w <- "https://thegymter.net/2022/06/20/2022-asian-championships-results/"

asian_tb_list_m <- url_m %>%
  read_html() %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[-c(1, 8)]
asian_tb_list_w <- url_w %>%
  read_html() %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[-c(1, 6)]

asian22_m <- process_web_tb_ls(tables_list = asian_tb_list_m, gender = "m")
asian22_w <- process_web_tb_ls(tables_list = asian_tb_list_w, gender = "w")
asian22_tb_ls <- c(asian22_m, asian22_w) %>% 
  update_vt()
asian22_tb <- transform_web_tb(table_list = asian22_tb_ls, 
                              Date = "15-18 June 2022",
                              Competition = "2022 9th Senior Artistic Gymnastics Asian Championships",
                              Location = "Doha, Qatar",
                              NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(asian22_tb, "../cleandata/data_new/asian_22.csv")
