source("function.R")

### 2022 Cottbus World Cup ################################################

url1 <- "https://thegymter.net/2022/03/02/2022-cottbus-world-cup-mens-results/"
url2 <- "https://thegymter.net/2022/03/03/2022-cottbus-world-cup-results/"

## to replace full names in source data with abbreviations
noc_key <- read.csv("noc_key.csv")
new_rows <- data.frame(
  Full_Name = c("Belarus*", "Norway", "Albania", "Armenia", "Czech Republic", "Great Britain", "Hong Kong", "Turkey"),
  Country_Abbr = c("BLR", "NOR", "ALB", "ARM", "CZE", "GBR", "HKG", "TUR")
)
noc_data <- rbind(noc_key, new_rows)

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









