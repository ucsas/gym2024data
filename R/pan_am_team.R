noc <- c("Argentina", "Aruba", "Barbados", "Bolivia", "Brasil", "Canadá", 
         "Islas Caimán", "Chile", "Colombia", "Costa Rica", "Cuba", 
         "República Dominicana", "Ecuador", "El Salvador", "Guatemala", 
         "Haití", "Jamaica", "México", "Panamá", "Perú", "Puerto Rico", 
         "Trinidad y Tobago", "Uruguay", "Estados Unidos", "Venezuela")


### 2023 Senior Pan American Championships #####################################

### men final ##################################################################
# locate_areas("../pdf/pan_am/m_final.pdf")
pam_m_final_ls_raw <- extract_tables("../pdf/pan_am/m_final.pdf", guess = FALSE,
                                     area = list(c(121.2329,  30.8146, 699.7553, 538.6497 ),
                                                 c(114.40000,  28.53731, 727.08708, 536.37236),
                                                 c(136.03765,  33.09189, 282.94589, 536.37236 )))
pam_m_final_tb_raw <- map(pam_m_final_ls_raw, ~ {
  colnames(.x) <- paste0("V", 1:ncol(.x))
  as.data.frame(.x)
}) %>% 
  list_rbind() %>%
  mutate(V1 = ifelse(V1 == "", NA, V1)) %>%  # 将空字符串替换为NA
  fill(V1) %>%   # 使用上方第一个不为空值的值填充NA
  mutate(V2 = ifelse(V2 == "", NA, V2)) %>%  # 将空字符串替换为NA
  fill(V2)  # 使用上方第一个不为空值的值填充NA

pam_m_final_ls_country <- pam_m_final_tb_raw %>%
  split(.$V1) %>% # 按照上一步生成的国家排名唯一值来拆分数据
  set_names(map(., ~ .x[1, 3])) %>% # 把列表里每个数据框命名为国家名, 第一行第三列
  map(~ .x[-1, -1]) # 删掉每个数据框第一行：团体分数行; 也删掉第一列，团体排名列


# 对于pam_m_final_ls_country列表中的每一个数据框，按照V2这列的唯一值来拆分数据，
# 拆分成若干小数据框，将每个小数据框中V3这列的值（已知都是字符串形式）连接起来，
# 作为小数据框的名称。返回包含这些小数据框的列表

result <- list()

for (team_name in names(pam_m_final_ls_country)) {
  df <- pam_m_final_ls_country[[team_name]]
  split_dfs <- split(df, df$V2)
  
  named_dfs <- list()
  for (athlete_name in names(split_dfs)) {
    sub_df <- split_dfs[[athlete_name]]
    name <- paste(sub_df$V3, collapse = " ")
    named_dfs[[name]] <- sub_df
  }
  
  result[[team_name]] <- named_dfs
} 

# 墨西哥队Fabian这个人出现了两次，墨西哥队因此共6个人，需要单独处理
result$México$`DE LUNA HERNÁNDEZ Fabián DE LUNA HERNÁNDEZ Fabián`[1,5] <- result$México$`DE LUNA HERNÁNDEZ Fabián DE LUNA HERNÁNDEZ Fabián`[3,5]
result$México$`DE LUNA HERNÁNDEZ Fabián DE LUNA HERNÁNDEZ Fabián`[2,5] <- result$México$`DE LUNA HERNÁNDEZ Fabián DE LUNA HERNÁNDEZ Fabián`[4,5]
result$México$`DE LUNA HERNÁNDEZ Fabián DE LUNA HERNÁNDEZ Fabián` <- result$México$`DE LUNA HERNÁNDEZ Fabián DE LUNA HERNÁNDEZ Fabián`[1:2,]
names(result$México)[names(result$México) == "DE LUNA HERNÁNDEZ Fabián DE LUNA HERNÁNDEZ Fabián"] <- "DE LUNA HERNÁNDEZ Fabián"

### 开始循环
m_final_ls <- list()

for (i in 1:length(result)) {
  team <- result[[i]]
  Country = names(result)[i]
  for (k in 1:length(team)) {
    page <- team[[k]]
    Name <- names(team)[k]
    FX_d_score <- page[1,3]
    PH_d_score <- page[1,4]
    SR_d_score <- page[1,5]
    VT_d_score <- page[1,6]
    PB_d_score <- page[1,7]
    HB_d_score <- page[1,8]
    
    FX_e_pen <- page[2,3]
    PH_e_pen <- page[2,4]
    SR_e_pen <- page[2,5]
    VT_e_pen <- page[2,6]
    PB_e_pen <- page[2,7]
    HB_e_pen <- page[2,8]
    
    person_final_df <- data.frame(Name, Country, FX_d_score,PH_d_score,SR_d_score,VT_d_score,PB_d_score,HB_d_score,
                                  FX_e_pen,PH_e_pen,SR_e_pen,VT_e_pen, PB_e_pen,HB_e_pen)
    m_final_ls[[length(m_final_ls)+1]] <- person_final_df
  }
}

m_final_ls

pam_m_final_tb <- m_final_ls %>% 
  list_rbind() %>% 
  mutate(Round = "TeamFinal", .after = 2) %>% 
  separate(FX_d_score, into = c("D_FX", "Score_FX"), sep = " ") %>% 
  separate(PH_d_score, into = c("D_PH", "Score_PH"), sep = " ") %>% 
  separate(SR_d_score, into = c("D_SR", "Score_SR"), sep = " ") %>%
  separate(VT_d_score, into = c("D_VT", "Score_VT"), sep = " ") %>% 
  separate(PB_d_score, into = c("D_PB", "Score_PB"), sep = " ") %>%
  separate(HB_d_score, into = c("D_HB", "Score_HB"), sep = " ") %>% 
  separate(FX_e_pen, into = c("E_FX", "Pen_FX"), sep = " ") %>%
  separate(PH_e_pen, into = c("E_PH", "Pen_PH"), sep = " ") %>%
  separate(SR_e_pen, into = c("E_SR", "Pen_SR"), sep = " ") %>%
  separate(VT_e_pen, into = c("E_VT", "Pen_VT"), sep = " ") %>% 
  separate(PB_e_pen, into = c("E_PB", "Pen_PB"), sep = " ") %>%
  separate(HB_e_pen, into = c("E_HB", "Pen_HB"), sep = " ") %>% 
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m") %>% 
  filter(!D == "")




### 2. men qual ################################################################
locate_areas("../pdf/pan_am/m_qual.pdf")
pam_m_qual_ls_raw <- extract_tables("../pdf/pan_am/m_qual.pdf", guess = FALSE,
                                    area = list(c(107.56706,  34.27168, 730.50355, 545.52526 ),
                                                c(103.01177,  35.41036, 727.08708, 545.52526 ),
                                                c(103.01177,  28.57833, 710.00472, 544.38641 ),
                                                c(100.73412,  29.7170 , 662.17413, 545.52526 )))

pam_m_qual_ls_raw[[3]] <- pam_m_qual_ls_raw[[3]][, -5]

# 将列表里四个表格合并为一个表格，在那之前先从矩阵转化为数据框，在填充队伍排名
pam_m_qual_tb_raw <- map(pam_m_qual_ls_raw, ~ {
  colnames(.x) <- paste0("V", 1:ncol(.x))
  as.data.frame(.x)
}) %>% 
  list_rbind() %>%
  mutate(V1 = ifelse(V1 == "", NA, V1)) %>%  # 将空字符串替换为NA
  fill(V1) %>%   # 使用上方第一个不为空值的值填充NA
  mutate(V2 = ifelse(V2 == "", NA, V2)) %>%  # 将空字符串替换为NA
  fill(V2)  # 使用上方第一个不为空值的值填充NA

pam_m_qual_ls_country <- pam_m_qual_tb_raw %>%
  split(.$V1) %>% # 按照上一步生成的国家排名唯一值来拆分数据
  set_names(map(., ~ .x[1, 3])) %>% # 把列表里每个数据框命名为国家名, 第一行第三列
  map(~ .x[-1, -1]) # 删掉每个数据框第一行：团体分数行; 也删掉第一列，团体排名列


result_m_qual <- list()

for (team_name in names(pam_m_qual_ls_country)) {
  df <- pam_m_qual_ls_country[[team_name]]
  split_dfs <- split(df, df$V2)
  
  named_dfs <- list()
  for (athlete_name in names(split_dfs)) {
    sub_df <- split_dfs[[athlete_name]]
    name <- paste(sub_df$V3, collapse = " ")
    named_dfs[[name]] <- sub_df
  }
  
  result_m_qual[[team_name]] <- named_dfs
} 

### 开始循环
m_qual_ls <- list()

for (i in 1:length(result_m_qual)) {
  team <- result_m_qual[[i]]
  Country = names(result_m_qual)[i]
  for (k in 1:length(team)) {
    page <- team[[k]]
    Name <- names(team)[k]
    FX_d_score <- page[1,3]
    PH_d_score <- page[1,4]
    SR_d_score <- page[1,5]
    VT_d_score <- page[1,6]
    PB_d_score <- page[1,7]
    HB_d_score <- page[1,8]
    
    FX_e_pen <- page[2,3]
    PH_e_pen <- page[2,4]
    SR_e_pen <- page[2,5]
    VT_e_pen <- page[2,6]
    PB_e_pen <- page[2,7]
    HB_e_pen <- page[2,8]
    
    person_df <- data.frame(Name, Country, FX_d_score,PH_d_score,SR_d_score,VT_d_score,PB_d_score,HB_d_score,
                                  FX_e_pen,PH_e_pen,SR_e_pen,VT_e_pen, PB_e_pen,HB_e_pen)
    m_qual_ls[[length(m_qual_ls)+1]] <- person_df
  }
}

m_qual_ls

pam_m_qual_tb <- m_qual_ls %>% 
  list_rbind() %>% 
  mutate(Round = "TeamQual", .after = 2) %>% 
  separate(FX_d_score, into = c("D_FX", "Score_FX"), sep = " ") %>% 
  separate(PH_d_score, into = c("D_PH", "Score_PH"), sep = " ") %>% 
  separate(SR_d_score, into = c("D_SR", "Score_SR"), sep = " ") %>%
  separate(VT_d_score, into = c("D_VT", "Score_VT"), sep = " ") %>% 
  separate(PB_d_score, into = c("D_PB", "Score_PB"), sep = " ") %>%
  separate(HB_d_score, into = c("D_HB", "Score_HB"), sep = " ") %>% 
  separate(FX_e_pen, into = c("E_FX", "Pen_FX"), sep = " ") %>%
  separate(PH_e_pen, into = c("E_PH", "Pen_PH"), sep = " ") %>%
  separate(SR_e_pen, into = c("E_SR", "Pen_SR"), sep = " ") %>%
  separate(VT_e_pen, into = c("E_VT", "Pen_VT"), sep = " ") %>% 
  separate(PB_e_pen, into = c("E_PB", "Pen_PB"), sep = " ") %>%
  separate(HB_e_pen, into = c("E_HB", "Pen_HB"), sep = " ") %>% 
  pivot_longer(
    cols = !1:3, 
    names_to = c(".value", "Apparatus"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "m") %>% 
  filter(!D == "")


### 3. women final ##################################################################
locate_areas("../pdf/pan_am/w_final.pdf")
pam_w_final_ls_raw <- extract_tables("../pdf/pan_am/w_final.pdf", guess = FALSE,
                                     area = list(c(121.23294,  35.52481, 708.86590, 533.10396 ),
                                                 c(109.84471,  38.94069 , 700.89413, 533.10396 ),
                                                 c(112.12236,  37.80206, 161.09177, 531.96533 )))

# 对于矩阵pam_w_final_ls_raw[[3]]，第一列之前加一列空字符串列，第三列之前加两列空字符串列
# 获取矩阵
mat <- pam_w_final_ls_raw[[3]]
# 创建空字符串列
empty_col <- matrix("", nrow = nrow(mat), ncol = 1)
# 在第一列之前添加一列空字符串列
mat <- cbind(empty_col, mat)
# 在第三列（现在是第四列）之前添加两列空字符串列
mat <- cbind(mat[, 1:3], empty_col, empty_col, mat[, 4:ncol(mat)])
# 将更新后的矩阵存储回列表中
pam_w_final_ls_raw[[3]] <- mat


pam_w_final_tb_raw <- map(pam_w_final_ls_raw, ~ {
  colnames(.x) <- paste0("V", 1:ncol(.x))
  as.data.frame(.x)
}) %>% 
  list_rbind() %>%
  mutate(V1 = ifelse(V1 == "", NA, V1)) %>%  # 将空字符串替换为NA
  fill(V1) %>%   # 使用上方第一个不为空值的值填充NA
  mutate(V2 = ifelse(V2 == "", NA, V2)) %>%  # 将空字符串替换为NA
  fill(V2)  # 使用上方第一个不为空值的值填充NA

pam_w_final_ls_country <- pam_w_final_tb_raw %>%
  split(.$V1) %>% # 按照上一步生成的国家排名唯一值来拆分数据
  set_names(map(., ~ .x[1, 3])) %>% # 把列表里每个数据框命名为国家名, 第一行第三列
  map(~ .x[-1, -1]) # 删掉每个数据框第一行：团体分数行; 也删掉第一列，团体排名列


# 对于pam_w_final_ls_country列表中的每一个数据框，按照V2这列的唯一值来拆分数据，
# 拆分成若干小数据框，将每个小数据框中V3这列的值（已知都是字符串形式）连接起来，
# 作为小数据框的名称。返回包含这些小数据框的列表

result <- list()

for (team_name in names(pam_w_final_ls_country)) {
  df <- pam_w_final_ls_country[[team_name]]
  split_dfs <- split(df, df$V2)
  
  named_dfs <- list()
  for (athlete_name in names(split_dfs)) {
    sub_df <- split_dfs[[athlete_name]]
    name <- paste(sub_df$V3, collapse = " ")
    named_dfs[[name]] <- sub_df
  }
  
  result[[team_name]] <- named_dfs
} 


### 开始循环
w_final_ls <- list()

for (i in 1:length(result)) {
  team <- result[[i]]
  Country = names(result)[i]
  for (k in 1:length(team)) {
    page <- team[[k]]
    Name <- names(team)[k]
    VT_D <- page[1,3]
    UB_D <- page[1,5]
    BB_D <- page[1,7]
    FX_D <- page[1,9]
    
    VT_Score <- page[1,4]
    UB_Score <- page[1,6]
    BB_Score <- page[1,8]
    FX_Score <- page[1,10]
    
    VT_E <- page[2,3]
    UB_E <- page[2,5]
    BB_E <- page[2,7]
    FX_E <- page[2,9]
    
    VT_Penalty <- page[2,4]
    UB_Penalty <- page[2,6]
    BB_Penalty <- page[2,8]
    FX_Penalty <- page[2,10]
    
    person_final_df <- data.frame(Name, Country, VT_D,UB_D,BB_D,FX_D,
                                  VT_E,UB_E,BB_E,FX_E,
                                  VT_Penalty,UB_Penalty,BB_Penalty,FX_Penalty,
                                  VT_Score,UB_Score,BB_Score,FX_Score)
    w_final_ls[[length(w_final_ls)+1]] <- person_final_df
  }
}

w_final_ls

pam_w_final_tb <- w_final_ls %>% 
  list_rbind() %>% 
  mutate(Round = "TeamFinal", .after = 2) %>% 
  pivot_longer(
    cols = !1:3, 
    names_to = c("Apparatus",".value"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w") %>% 
  filter(!D == "")



### 4. women qual ##################################################################
locate_areas("../pdf/pan_am/w_qual.pdf")
pam_w_qual_ls_raw <- extract_tables("../pdf/pan_am/w_qual.pdf", guess = FALSE,
                                     area = list(c(115.5388,  32.5036, 706.5883, 535.0532 ),
                                                 c(107.56706,  33.64317, 720.25413, 535.05324 ),
                                                 c(107.56706,  31.36403, 720.25413, 536.19281 ),
                                                 c(107.5671,  32.5036, 716.8377, 533.9137),
                                                 c(108.70588,  31.36403, 315.97177, 536.19281)))

pam_w_qual_tb_raw <- map(pam_w_qual_ls_raw, ~ {
  colnames(.x) <- paste0("V", 1:ncol(.x))
  as.data.frame(.x)
}) %>% 
  list_rbind() %>%
  mutate(V1 = ifelse(V1 == "", NA, V1)) %>%  # 将空字符串替换为NA
  fill(V1) %>%   # 使用上方第一个不为空值的值填充NA
  mutate(V2 = ifelse(V2 == "", NA, V2)) %>%  # 将空字符串替换为NA
  fill(V2)  # 使用上方第一个不为空值的值填充NA

pam_w_qual_ls_country <- pam_w_qual_tb_raw %>%
  split(.$V1) %>% # 按照上一步生成的国家排名唯一值来拆分数据
  set_names(map(., ~ .x[1, 3])) %>% # 把列表里每个数据框命名为国家名, 第一行第三列
  map(~ .x[-1, -1]) # 删掉每个数据框第一行：团体分数行; 也删掉第一列，团体排名列

result <- list()

for (team_name in names(pam_w_qual_ls_country)) {
  df <- pam_w_qual_ls_country[[team_name]]
  split_dfs <- split(df, df$V2)
  
  named_dfs <- list()
  for (athlete_name in names(split_dfs)) {
    sub_df <- split_dfs[[athlete_name]]
    name <- paste(sub_df$V3, collapse = " ")
    named_dfs[[name]] <- sub_df
  }
  
  result[[team_name]] <- named_dfs
} 

### 开始循环
w_qual_ls <- list()

for (i in 1:length(result)) {
  team <- result[[i]]
  Country = names(result)[i]
  for (k in 1:length(team)) {
    page <- team[[k]]
    Name <- names(team)[k]
    VT_D <- page[1,3]
    UB_D <- page[1,5]
    BB_D <- page[1,7]
    FX_D <- page[1,9]
    
    VT_Score <- page[1,4]
    UB_Score <- page[1,6]
    BB_Score <- page[1,8]
    FX_Score <- page[1,10]
    
    VT_E <- page[2,3]
    UB_E <- page[2,5]
    BB_E <- page[2,7]
    FX_E <- page[2,9]
    
    VT_Penalty <- page[2,4]
    UB_Penalty <- page[2,6]
    BB_Penalty <- page[2,8]
    FX_Penalty <- page[2,10]
    
    person_final_df <- data.frame(Name, Country, VT_D,UB_D,BB_D,FX_D,
                                  VT_E,UB_E,BB_E,FX_E,
                                  VT_Penalty,UB_Penalty,BB_Penalty,FX_Penalty,
                                  VT_Score,UB_Score,BB_Score,FX_Score)
    w_qual_ls[[length(w_qual_ls)+1]] <- person_final_df
  }
}

pam_w_qual_tb <- w_qual_ls %>% 
  list_rbind() %>% 
  mutate(Round = "TeamQual", .after = 2) %>% 
  pivot_longer(
    cols = !1:3, 
    names_to = c("Apparatus",".value"), 
    names_sep = "_", 
    values_drop_na = TRUE
  ) %>% 
  mutate(Gender = "w") %>% 
  filter(!D == "")




### combine m_team_final and w_team_final ######################

# 创建一个名为country_conversion的向量，该向量将西班牙语国家名称映射到国家代码
country_conversion <- c(
  "Argentina" = "ARG",
  "Aruba" = "ARU",
  "Barbados" = "BAR",
  "Bolivia" = "BOL",
  "Brasil" = "BRA",
  "Canadá" = "CAN",
  "Islas Caimán" = "CAY",
  "Chile" = "CHI",
  "Colombia" = "COL",
  "Costa Rica" = "CRC",
  "Cuba" = "CUB",
  "Rep. Dominicana" = "DOM",
  "Ecuador" = "ECU",
  "El Salvador" = "ESA",
  "Guatemala" = "GUA",
  "Haití" = "HAI",
  "Jamaica" = "JAM",
  "México" = "MEX",
  "Panamá" = "PAN",
  "Perú" = "PER",
  "Puerto Rico" = "PUR",
  "Trinidad y Tobago" = "TTO",
  "Uruguay" = "URU",
  "Estados Unidos" = "USA",
  "Venezuela" = "VEN"
)


pam_team_final_tb <- bind_rows(pam_m_final_tb, pam_w_final_tb, pam_m_qual_tb, pam_w_qual_tb) %>% 
  mutate(FirstName = map_chr(str_extract_all(Name, "\\b[A-Z][a-z]+\\b"), 
                             ~ paste(.x, collapse = " "))) %>% 
  mutate(LastName = map_chr(str_extract_all(Name, "\\b[A-Z]+\\b"), 
                            ~ paste(.x, collapse = " "))) %>% 
  mutate(Penalty = as.numeric(Pen), # 把D E Score Rank转成数值
         E_Score = as.numeric(E),
         D_Score = as.numeric(D),
         Score = as.numeric(Score),
         Penalty = ifelse(Penalty < 0, -Penalty, Penalty),
         Rank = NA) %>% 
  filter(!is.na(D_Score)) %>% 
  mutate(Competition = "2023 Artistic Gymnastics Senior Pan American Championships", 
         Location = "Medellin, Colombia",
         Date = "26-28 May 2023") %>% # 加上赛事信息
  relocate(LastName, FirstName, Gender, Country, Date, Competition, Round, 
           Location, Apparatus, Rank, D_Score, E_Score, Penalty, Score ) %>% 
  select(!Name:Pen) %>%
  mutate(Country = recode(Country, !!!country_conversion))

# 修改姓名异常
pam_team_final_tb <- pam_team_final_tb %>%
  mutate(
    LastName = if_else(FirstName == "Daniel Angel", "VILLAFANE", LastName),
    LastName = if_else(FirstName == "Jose Carlos", "ESCANDÓN MARÍN", LastName),
    LastName = if_else(FirstName == "Esteban", "GIRÓN ÁLVAREZ", LastName),
    FirstName = if_else(FirstName == "Esteban", "Andrés Esteban", FirstName),
    LastName = if_else(FirstName == "Josue", "JUAREZ", LastName),
    LastName = if_else(FirstName == "Sarai", "RODRÍGUEZ GARCÍA", LastName),
    LastName = if_else(FirstName == "Alberto", "PÉREZ FERNÁNDEZ", LastName)
  ) %>% 
  mutate(
    FirstName = if_else(LastName == "DE LUNA", "Fabián", FirstName),
    LastName = if_else(LastName == "DE LUNA", "DE LUNA HERNÁNDEZ", LastName),
    FirstName = if_else(LastName == "SUE", "Sebastián Andrés", FirstName),
    LastName = if_else(LastName == "SUE", "SUE DOMÍNGUEZ", LastName),
  )


write_csv(pam_team_final_tb, "../cleandata/data_new/pan_america_team.csv")
