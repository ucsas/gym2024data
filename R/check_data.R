### 检查combined_df 是否符合要求

## 关于浮点数导致的误差示例
print(4.4 + 8.066, digits = 20)
print(12.466, digits = 20)
all.equal(4.4 + 8.066, 12.466)


## 检查D+E-Pen是否等于Score，使用near函数来处理浮点数比较时可能出现的微小差异
rows_not_matching <- combined_df %>%
  mutate(Penalty = if_else(is.na(Penalty), 0, Penalty)) %>%  # 将Penalty中的NA替换为0
  filter(!near(D_Score + E_Score - Penalty, Score))  # 检查条件是否满足，并过滤出不满足条件的行

## 按照赛事分类，显示每项赛事错误行数
competition_counts <- rows_not_matching %>%
  group_by(Competition) %>%
  summarise(Count = n())




