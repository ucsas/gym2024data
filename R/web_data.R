source("function.R")

## to replace full names in source data with abbreviations
noc_key <- read.csv("noc_key.csv")
new_rows <- data.frame(
  Full_Name = c("Belarus*", "Norway", "Albania", "Armenia", "Czech Republic", "Great Britain", "Hong Kong", "Turkey", "South Korea", "Russia*"),
  Country_Abbr = c("BLR", "NOR", "ALB", "ARM", "CZE", "GBR", "HKG", "TUR", "KOR", "RUS")
)
noc_data <- rbind(noc_key, new_rows)


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



### 2022 Varna World Cup ################################################

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




