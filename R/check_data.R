### 检查combined_df 是否符合要求

## 关于浮点数导致的误差示例
print(4.4 + 8.066, digits = 20)
print(12.466, digits = 20)
all.equal(4.4 + 8.066, 12.466)

## 检查D_Score为空值的行
rows_with_na_score <- combined_df %>% 
  filter(is.na(Score))

rows_with_na_escore <- combined_df %>% 
  filter(is.na(E_Score))

## 检查D+E-Pen是否等于Score，使用near函数来处理浮点数比较时可能出现的微小差异
rows_not_matching <- combined_df %>%
  mutate(Penalty = if_else(is.na(Penalty), 0, Penalty)) %>%  # 将Penalty中的NA替换为0
  filter(!near(D_Score + E_Score - Penalty, Score)) %>%   # 检查条件是否满足，并过滤出不满足条件的行
  arrange(Competition, Round, Apparatus)

## 按照赛事分类，显示每项赛事错误行数
competition_counts <- rows_not_matching %>%
  group_by(Competition) %>%
  summarise(Count = n())

## 删除combined_df数据框中D_Score、E_Score和Score任一列为NA的行，查看删除了多少行（5行）
# 记录删除前的行数
nrow_before <- nrow(combined_df)
# 删除D_Score, E_Score, Score任一列为NA的行
combined_df <- combined_df %>%
  filter(!is.na(D_Score) & !is.na(E_Score) & !is.na(Score))
# 记录删除后的行数
nrow_after <- nrow(combined_df)
# 计算并打印删除的行数
rows_deleted <- nrow_before - nrow_after
print(rows_deleted)

## 获得各分数的描述性统计，确保在合理范围区间
combined_df %>%
  select_if(is.numeric) %>%  # 选择数值型变量
  select(last_col(offset = 4):last_col()) %>%  # 选择最后五列
  summary()  # 得到描述性统计

## 查看combined_df数据框中D_Score、E_Score和Score任一列为0的行
zero_score_rows <- combined_df %>%
  filter(D_Score == 0 | E_Score == 0 | Score == 0)



