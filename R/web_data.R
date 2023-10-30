source("function.R")

## to replace full names in source data with abbreviations
noc_key <- read.csv("noc_key.csv")
new_rows <- data.frame(
  Full_Name = c("Belarus*", "Norway", "Albania", "Armenia", "Czech Republic", "Great Britain", "Hong Kong", "Turkey", "South Korea", "Russia*", "Taiwan"),
  Country_Abbr = c("BLR", "NOR", "ALB", "ARM", "CZE", "GBR", "HKG", "TUR", "KOR", "RUS", "TPE")
)
noc_data <- rbind(noc_key, new_rows)

last_name_first_noc <- c("CHN", "JPN", "KOR", "PRK", "VIE", "HKG", "TPE")

### 2022 Cottbus World Cup ################################################

url1 <- "https://thegymter.net/2022/03/02/2022-cottbus-world-cup-mens-results/"
url2 <- "https://thegymter.net/2022/03/03/2022-cottbus-world-cup-results/"

cot22_m <- get_web_tb(url1, gender = "m")
cot22_w <- get_web_tb(url2, gender = "w")
cot22_tb_ls <- c(cot22_m, cot22_w) %>% 
  update_vt()
cot22_tb <- transform_web_tb(table_list = cot22_tb_ls, 
                           Date = "24-27 Feb 2022",
                           Competition = "2022 Cottbus World Cup",
                           Location = "Cottbus, Germany",
                           NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(cot22_tb, "../cleandata/data_new/cottbus_22.csv")





### 2022 Doha World Cup ################################################

url1 <- "https://thegymter.net/2022/03/05/2022-doha-world-cup-mens-results/"
url2 <- "https://thegymter.net/2022/03/05/2022-doha-world-cup-results/"

doha22_m <- get_web_tb(url1, gender = "m")
doha22_w <- get_web_tb(url2, gender = "w")
doha22_tb_ls <- c(doha22_m, doha22_w) %>% 
  update_vt()

doha22_tb_ls <- doha22_tb_ls %>% 
  map( ~{
    # For Rank, change "—" into NA, then make this column numeric
    .x$Rank[.x$Rank == "—"] <- NA
    .x$Rank <- as.numeric(.x$Rank)
    return(.x)
  }) %>% 
  map( ~{
    if (!is.numeric(.x$D)) {
      .x$D <- as.numeric(.x$D)
    }
    return(.x)
  }) %>% 
  map( ~{
    # 将E和Total列由字符串转为数值类型
    .x$E <- as.numeric(.x$E)
    .x$Total <- as.numeric(.x$Total)
    return(.x)
  }) %>% 
  map( ~ {
    # 检查数据框是否包含名为“Average”的列
    if ("Average" %in% names(.x)) {
      .x <- .x %>%
        select(-Average)  # 删除名为“Average”的列
    }
    return(.x)
  })

doha22_tb <- transform_web_tb(table_list = doha22_tb_ls, 
                             Date = "2-5 Mar 2022",
                             Competition = "2022 Doha World Cup",
                             Location = "Doha, Qatar",
                             NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(doha22_tb, "../cleandata/data_new/doha_22.csv")



### 2022 Cairo World Cup ################################################

url1 <- "https://thegymter.net/2022/03/20/2022-cairo-world-cup-mens-results/"
url2 <- "https://thegymter.net/2022/03/20/2022-cairo-world-cup-results/"

cairo22_m <- get_web_tb(url1, gender = "m")
cairo22_w <- get_web_tb(url2, gender = "w")
cairo22_tb_ls <- c(cairo22_m, cairo22_w) %>% 
  update_vt()
cairo22_tb <- transform_web_tb(table_list = cairo22_tb_ls, 
                             Date = "17-20 Mar 2022",
                             Competition = "2022 Cairo World Cup",
                             Location = "Cairo, Egypt",
                             NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(cairo22_tb, "../cleandata/data_new/Cairo_22.csv")


### 2022 Baku World Cup ################################################

url1 <- "https://thegymter.net/2022/04/05/2022-baku-world-cup-mens-results/"
url2 <- "https://thegymter.net/2022/04/04/2022-baku-world-cup-results/"

Baku22_m <- get_web_tb(url1, gender = "m")
Baku22_w <- get_web_tb(url2, gender = "w")
Baku22_tb_ls <- c(Baku22_m, Baku22_w) %>% 
  update_vt()
Baku22_tb <- transform_web_tb(table_list = Baku22_tb_ls, 
                               Date = "31 Mar 2022 - 3 Apr 2022",
                               Competition = "2022 Baku World Cup",
                               Location = "Baku, Azerbaijan",
                               NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(Baku22_tb, "../cleandata/data_new/Baku_22.csv")



### 2022 Varna World Challenge Cup ################################################

url1 <- "https://thegymter.net/2022/05/31/2022-varna-challenge-cup-mens-results/"
url2 <- "https://thegymter.net/2022/05/30/2022-varna-challenge-cup-results/"

varna22_m <- get_web_tb(url1, gender = "m")
varna22_w <- get_web_tb(url2, gender = "w")
varna22_tb_ls <- c(varna22_m, varna22_w) %>% 
  update_vt() %>% 
  map( ~{
    # 使用filter函数删除D列为"-"的行
    .x <- .x %>%
      filter(D != "—")
    return(.x)
  }) %>% 
  map( ~{
    # 将E和Total列由字符串转为数值类型
    .x$E <- as.numeric(.x$E)
    .x$D <- as.numeric(.x$D)
    return(.x)
  })
varna22_tb <- transform_web_tb(table_list = varna22_tb_ls, 
                              Date = "26-29 Mar 2022",
                              Competition = "2022 Varna World Challenge Cup",
                              Location = "Varna, Bulgaria",
                              NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(varna22_tb, "../cleandata/data_new/varna_22.csv")



### 2022 Osijek World Challenge Cup ################################################

url1 <- "https://thegymter.net/2022/06/13/2022-osijek-challenge-cup-mens-results/"
url2 <- "https://thegymter.net/2022/06/13/2022-osijek-challenge-cup-results/"

osijek22_m <- get_web_tb(url1, gender = "m")
osijek22_w <- get_web_tb(url2, gender = "w")
osijek22_tb_ls <- c(osijek22_m, osijek22_w) %>% 
  update_vt() %>% 
  map( ~{
    # 使用filter函数删除D列为"DNS"的行
    .x <- .x %>%
      filter(D != "DNS")
    return(.x)
  }) %>% 
  map( ~{
    # For Rank, change "—" into NA, then make this column numeric
    .x$Rank[.x$Rank == "—"] <- NA
    .x$Rank <- as.numeric(.x$Rank)
    return(.x)
  }) %>% 
  map( ~{
    # 将E和Total列由字符串转为数值类型
    .x$E <- as.numeric(.x$E)
    .x$D <- as.numeric(.x$D)
    .x$Total <- as.numeric(.x$Total)
    return(.x)
  }) %>% 
  map( ~ {
    # 检查数据框是否包含名为“Average”的列
    if ("Average" %in% names(.x)) {
      .x <- .x %>%
        select(-Average)  # 删除名为“Average”的列
    }
    return(.x)
  })

osijek22_tb <- transform_web_tb(table_list = osijek22_tb_ls, 
                                Date = "9-12 June 2022",
                                Competition = "2022 Osijek World Challenge Cup",
                                Location = "Osijek, Croatia",
                                NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(osijek22_tb, "../cleandata/data_new/osijek_22.csv")




### 2022 Koper World Challenge Cup ################################################

url1 <- "https://thegymter.net/2022/06/21/2022-koper-challenge-cup-mens-results/"
url2 <- "https://thegymter.net/2022/06/21/2022-koper-challenge-cup-results/"

koper22_m <- get_web_tb(url1, gender = "m")
koper22_w <- get_web_tb(url2, gender = "w")
koper22_tb_ls <- c(koper22_m, koper22_w) %>% 
  update_vt() %>% 
  map( ~{
    # 使用filter函数删除D列为"DNS"的行
    .x <- .x %>%
      filter(D != "DNS")
    return(.x)
  }) %>% 
  map( ~{
    # For Rank, change "—" into NA, then make this column numeric
    .x$Rank[.x$Rank == "—"] <- NA
    .x$Rank <- as.numeric(.x$Rank)
    return(.x)
  }) %>% 
  map( ~{
    # 将E和Total列由字符串转为数值类型
    .x$E <- as.numeric(.x$E)
    .x$D <- as.numeric(.x$D)
    .x$Total <- as.numeric(.x$Total)
    return(.x)
  }) %>% 
  map( ~ {
    # 检查数据框是否包含名为“Average”的列
    if ("Average" %in% names(.x)) {
      .x <- .x %>%
        select(-Average)  # 删除名为“Average”的列
    }
    return(.x)
  })

koper22_tb <- transform_web_tb(table_list = koper22_tb_ls, 
                                Date = "16-19 June 2022",
                                Competition = "2022 Koper World Challenge Cup",
                                Location = "Koper, Slovenia",
                                NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(koper22_tb, "../cleandata/data_new/koper_22.csv")


### 2022 Paris World Challenge Cup ################################################

url1 <- "https://thegymter.net/2022/09/27/2022-paris-challenge-cup-mens-results/"
url2 <- "https://thegymter.net/2022/09/27/2022-paris-challenge-cup-results/"

paris22_m <- get_web_tb(url1, gender = "m")
paris22_w <- get_web_tb(url2, gender = "w")
paris22_tb_ls <- c(paris22_m, paris22_w) %>% 
  update_vt() %>% 
  map( ~{
    # 使用filter函数删除D列为"DNS"的行
    .x <- .x %>%
      filter(D != "DNS")
    return(.x)
  }) %>% 
  map( ~{
    # For Rank, change "—" into NA, then make this column numeric
    .x$Rank[.x$Rank == "—"] <- NA
    .x$Rank <- as.numeric(.x$Rank)
    return(.x)
  }) %>% 
  map( ~{
    # 将E和Total列由字符串转为数值类型
    .x$E <- as.numeric(.x$E)
    .x$D <- as.numeric(.x$D)
    .x$Total <- as.numeric(.x$Total)
    return(.x)
  }) %>% 
  map( ~ {
    # 检查数据框是否包含名为“Average”的列
    if ("Average" %in% names(.x)) {
      .x <- .x %>%
        select(-Average)  # 删除名为“Average”的列
    }
    return(.x)
  })
  

paris22_tb <- transform_web_tb(table_list = paris22_tb_ls, 
                               Date = "24-25 Sept 2022",
                               Competition = "2022 Paris World Challenge Cup",
                               Location = "Paris, France",
                               NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(paris22_tb, "../cleandata/data_new/paris_22.csv")


### 2022 Szombathely World Challenge Cup ################################################

url1 <- "https://thegymter.net/2022/10/03/2022-szombathely-challenge-cup-mens-results/"
url2 <- "https://thegymter.net/2022/10/02/2022-szombathely-challenge-cup-results/"

szombathely22_m <- get_web_tb(url1, gender = "m")
szombathely22_w <- get_web_tb(url2, gender = "w")
szombathely22_tb_ls <- c(szombathely22_m, szombathely22_w) %>% 
  update_vt() %>% 
  map( ~{
    # 使用filter函数删除D列为"DNS"的行
    .x <- .x %>%
      filter(D != "DNS")
    return(.x)
  }) %>% 
  map( ~{
    # For Rank, change "—" into NA, then make this column numeric
    .x$Rank[.x$Rank == "—"] <- NA
    .x$Rank <- as.numeric(.x$Rank)
    return(.x)
  }) %>% 
  map( ~{
    # 将E和Total列由字符串转为数值类型
    .x$E <- as.numeric(.x$E)
    .x$D <- as.numeric(.x$D)
    .x$Total <- as.numeric(.x$Total)
    return(.x)
  }) %>% 
  map( ~ {
    # 检查数据框是否包含名为“Average”的列
    if ("Average" %in% names(.x)) {
      .x <- .x %>%
        select(-Average)  # 删除名为“Average”的列
    }
    return(.x)
  })
szombathely22_tb <- transform_web_tb(table_list = szombathely22_tb_ls, 
                               Date = "30 Sept 2022 - 2 Oct 2022",
                               Competition = "2022 Szombathely World Challenge Cup",
                               Location = "Szombathely, Hungary",
                               NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(szombathely22_tb, "../cleandata/data_new/szombathely_22.csv")


### 2022 Mersin World Challenge Cup ################################################

url1 <- "https://thegymter.net/2022/10/11/2022-mersin-challenge-cup-mens-results/"
url2 <- "https://thegymter.net/2022/10/10/2022-mersin-challenge-cup-results/"

mersin22_m <- get_web_tb(url1, gender = "m")
mersin22_w <- get_web_tb(url2, gender = "w")
mersin22_tb_ls <- c(mersin22_m, mersin22_w) %>% 
  update_vt() %>% 
  map( ~{
    # 使用filter函数删除D列为"DNS"的行
    .x <- .x %>%
      filter(D != "DNS")
    return(.x)
  }) %>% 
  map( ~{
    # For Rank, change "—" into NA, then make this column numeric
    .x$Rank[.x$Rank == "—"] <- NA
    .x$Rank <- as.numeric(.x$Rank)
    return(.x)
  }) %>% 
  map( ~{
    # 将E, D和Total列由字符串转为数值类型
    .x$E <- as.numeric(.x$E)
    .x$D <- as.numeric(.x$D)
    .x$Total <- as.numeric(.x$Total)
    return(.x)
  }) %>% 
  map( ~ {
    # 检查数据框是否包含名为“Average”的列
    if ("Average" %in% names(.x)) {
      .x <- .x %>%
        select(-Average)  # 删除名为“Average”的列
    }
    return(.x)
  })
mersin22_tb <- transform_web_tb(table_list = mersin22_tb_ls, 
                                     Date = "7-9 Oct 2022",
                                     Competition = "2022 Mersin World Challenge Cup",
                                     Location = "Mersin, Turkey",
                                     NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(mersin22_tb, "../cleandata/data_new/mersin_22.csv")




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



### 2023 10th Senior Artistic Gymnastics Asian Championships ####################

url_m <- "https://thegymter.net/2023/06/22/2023-asian-championships-mens-results/"
url_w <- "https://thegymter.net/2023/06/21/2023-asian-championships-results/"
asian23_tb_list_m <- url_m %>%
  read_html() %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[-c(1, 8, 15:28)]
asian23_tb_list_w <- url_w %>%
  read_html() %>%
  html_table(fill = TRUE, header = TRUE) %>%
  .[-c(1, 6, 11:20)]

asian23_m <- process_web_tb_ls(tables_list = asian23_tb_list_m, gender = "m")
asian23_w <- process_web_tb_ls(tables_list = asian23_tb_list_w, gender = "w")

asian23_tb_ls <- c(asian23_m, asian23_w) %>% 
  update_vt() %>% 
  map( ~{
    # 使用filter函数删除D列为"DNS"的行
    .x <- .x %>%
      filter(D != "DNS")
    return(.x)
  }) %>% 
  map( ~{
    # For Rank, change "—" into NA, then make this column numeric
    .x$Rank[.x$Rank == "—"] <- NA
    .x$Rank <- as.numeric(.x$Rank)
    return(.x)
  }) %>% 
  map( ~{
    # 将E, D和Total列由字符串转为数值类型
    .x$E <- as.numeric(.x$E)
    .x$D <- as.numeric(.x$D)
    .x$Total <- as.numeric(.x$Total)
    return(.x)
  }) %>% 
  map( ~ {
    # 检查数据框是否包含名为“Average”的列
    if ("Average" %in% names(.x)) {
      .x <- .x %>%
        select(-Average)  # 删除名为“Average”的列
    }
    return(.x)
  })
asian23_tb <- transform_web_tb(table_list = asian23_tb_ls, 
                               Date = "10-18 June 2023",
                               Competition = "2023 10th Senior Artistic Gymnastics Asian Championships",
                               Location = "Singapore",
                               NOCkey = noc_data) %>% 
  select(-Nation)

write_csv(asian23_tb, "../cleandata/data_new/asian_23.csv")

