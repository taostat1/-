setwd("D:/Study on Youth Suicide Rate")
rm(list = ls()) # clear everything
library(tidyverse)

# 读取CSV文件
df <- read.csv("心理测评数据-维度结果.csv")
class(df)
# 删除重复行
df_unique <- df %>%
  distinct()

# 清理列名
names(df_unique) <- gsub("`", "", names(df_unique))

df_unique = df_unique[,c("X.GLOABL_ID.","X.DIM_NAME.","X.DIM_SCORE.")]




library(dplyr)
library(tidyr)

# Step 1: Identify duplicates
duplicates <- df_unique %>%
  group_by(X.GLOABL_ID., X.DIM_NAME.) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L)

print(duplicates) # This will show you the duplicate rows

# Step 2: Resolve duplicates by summarizing (e.g., take the mean of duplicate scores)
df_unique <- df %>%
  group_by(X.GLOABL_ID., X.DIM_NAME.) %>%
  summarise(X.DIM_SCORE. = mean(X.DIM_SCORE.), .groups = "drop")

# Step 3: Pivot the data after resolving duplicates
df_new <- df_unique %>%
  pivot_wider(names_from = X.DIM_NAME., values_from = X.DIM_SCORE.)

# View the resulting dataframe
print(df_new)
colnames(df_new)[1] = "GLOBAL_ID"



df1 <- read.csv("心理测评数据-原始答题结果.csv")
df1 = df1[df1$IS_LIE == " false",]
colnames(df1)[1] = "GLOBAL_ID"
df1[1,"SELECTED_DATA"]

#分为成长版和普测版，然后再拼起来。
table(df1$SCALE_NAME)

df1_puce = df1[df1$SCALE_NAME == " 学生生活、学习、健康状况调查表(普测版)",]
df1_chengzhang = df1[df1$SCALE_NAME == " 学生生活、学习、健康状况调查表(成长版)",]

df1_puce$SELECTED_DATA

# 加载必要的库
library(jsonlite)  # 用于解析JSON字符串

# 假设 df1_puce 是你的原始数据框，SELECTED_DATA 是含有JSON字符串的列

# 清理字符串，移除多余的反斜杠，并将单引号转换为双引号
cleaned_data <- gsub("\\\\", "", df1_puce$SELECTED_DATA)
cleaned_data <- gsub("'", "\"", cleaned_data)

# 解析JSON字符串
json_data <- lapply(cleaned_data, fromJSON, simplifyVector = FALSE)

# 创建一个向量来保存新的列名
new_col_names <- c(
  "下列哪一个图案更符合你现在的心情？",
  "我有能力学习任何东西",
  "只要开始做一件事情，我就会完成它",
  "当我有开心的事情发生时，我总希望与家人谈论分享",
  "需要做的事，我大部分都能做",
  "我觉得自己能够应付出现的问题",
  "别人一句不经意的话，常常会让我纠结很久",
  "一旦我计划了要做某事，我就会按计划进行",
  "我很在意别人对我的评价",
  "相对于别人的感受和需要，我觉得自己的感受不重要",
  "你目前的身份是下面哪一种？",
  "我从未有过失眠的体验",
  "我从没有过自杀的想法和准备",
  "别人给我起难听的外号，骂我，或取笑、讽刺我",
  "我在半夜醒来，难以再入睡",
  "我感到不开心",
  "我不喜欢上学",
  "我会做一些不情愿做的事，尤其是为了别人",
  "我跟朋友闹矛盾后，内心会久久难以平复",
  "我觉得我的问题和困难都怨我自己",
  "我喜欢我们家里的家庭活动",
  "再困难我也想活着",
  "我只要有一段时间没有上网、看手机，就会莫名地情绪低落",
  "别人强迫向我要钱，或者拿走、损坏我的东西",
  "某些同学采用打、踢、推、撞等方式欺负我",
  "长时间网游，使我的身体健康状况越来越不如以前了",
  "晚上入睡时，我需要很长时间才能睡着",
  "当别人与自己观点、想法不一致时，即使内心不认同，我也倾向于赞同对方",
  "中国的首都是哪一座城市？",
  "我觉得学习很累很烦",
  "我从没做过故意弄伤自己的行为",
  "我做事提前制定计划，有条理",
  "我希望可以不用学习",
  "我的问题快把我压垮了",
  "由于上网使我与周围其他人的关系没以前好了，但我无法减少上网时间",
  "遇到困难时我更倾向于求助家人"
)

# 初始化一个新的空数据框，列名设置为 new_col_names
new_df <- as.data.frame(matrix(NA, nrow = nrow(df1_puce), ncol = length(new_col_names)))
names(new_df) <- new_col_names

# 根据 SelectData 填充数据
for (i in seq_along(json_data)) {
  for (j in seq_along(json_data[[i]])) {
    col_name <- new_col_names[json_data[[i]][[j]]$TopicNumber]
    new_df[i, col_name] <- json_data[[i]][[j]]$SelectData
  }
}

# 查看结果
print(new_df)

df1_1 = cbind(df1_puce, new_df)




# 加载必要的库
library(jsonlite)  # 用于解析JSON字符串

# 假设 df1_chengzhang 是你的原始数据框，SELECTED_DATA 是含有JSON字符串的列

# 清理字符串，移除多余的反斜杠，并将单引号转换为双引号
cleaned_data <- gsub("\\\\", "", df1_chengzhang$SELECTED_DATA)
cleaned_data <- gsub("'", "\"", cleaned_data)

# 解析JSON字符串
json_data <- lapply(cleaned_data, fromJSON, simplifyVector = FALSE)

# 创建一个向量来保存新的列名
new_col_names <- c(
  "下列哪一个图案更符合你现在的心情？",
  "一些不想要的念头和感受一直困扰着我",
  "只要开始做一件事情，我就会完成它",
  "晚上入睡时，我需要很长时间才能睡着",
  "我跟朋友闹矛盾后，内心会久久难以平复",
  "我在半夜醒来，难以再入睡",
  "我被一些麻烦事困住了",
  "我觉得我的问题和困难都怨我自己",
  "我认为有些人的存在没有价值",
  "身边人都不在意我",
  "你目前的身份是下面哪一种",
  "当一件事没做好，我会埋怨合作者",
  "别人给我起难听的外号，骂我，或取笑、讽刺我",
  "无论我做什么都得不到回应",
  "我从没有过自杀的想法和准备",
  "他人的言行常引起我的不满",
  "我很难入睡或睡得不安稳",
  "我从没有过伤害自己的想法",
  "我生病都没人照顾关心我",
  "我睡得很浅，容易被惊醒",
  "我常常觉得别人做得不够好",
  "一旦我计划了要做某事，我就会按计划进行",
  "我从没做过故意弄伤自己的行为",
  "最近我跟老师关系紧张",
  "我很在意别人对我的评价",
  "我的问题快把我压垮了",
  "中国的首都是哪一座城市",
  "别人强迫向我要钱，或者拿走、损坏我的东西",
  "再困难我也想活着",
  "偶尔听到别人谈论我，我会想自己是不是做错了什么",
  "我会一直坚持做作业直到完成为止",
  "别人一句不经意的话，常常会让我纠结很久",
  "某些同学采用打、踢、推、撞等方式欺负我",
  "没人在意我开不开心",
  "我是一个勤奋的人"
)

# 初始化一个新的空数据框，列名设置为 new_col_names
new_df <- as.data.frame(matrix(NA, nrow = nrow(df1_chengzhang), ncol = length(new_col_names)))
names(new_df) <- new_col_names

# 根据 SelectData 填充数据
for (i in seq_along(json_data)) {
  for (j in seq_along(json_data[[i]])) {
    col_name <- new_col_names[json_data[[i]][[j]]$TopicNumber]
    new_df[i, col_name] <- json_data[[i]][[j]]$SelectData
  }
}

# 查看结果
print(new_df)
df1_2 = cbind(df1_chengzhang, new_df)


library(dplyr)
df1_ <- bind_rows(df1_1, df1_2, .id = NULL)



# df1_ = rbind(df1_1, df1_2)


colnames(df1_)


df1 = df1_[,c("GLOBAL_ID","USER_ID", colnames(df1_)[15:69])]



A_with_USER_ID <- left_join(df_new, df1, by = "GLOBAL_ID")

library(readxl)

df2 <- read_excel("体测数据_2.xlsx", col_types = "text")
df2$age <- as.numeric(df2$age)
df2$sex <- as.numeric(df2$sex)
df2$scorelevel <- as.factor(df2$scorelevel)
colnames(df2)
df2 = df2[,c("ding_userid","age","sex","scorelevel","njname")]

df2$ding_userid = as.numeric(df2$ding_userid)
df2$ding_userid = as.character(df2$ding_userid)
df2$ding_userid
# 查看数据
A_with_USER_ID$USER_ID = gsub(" ", "", A_with_USER_ID$USER_ID)
B_with_USER_ID <- merge(A_with_USER_ID, df2, by.x = "USER_ID", by.y = "ding_userid", all.x = TRUE)

arr1 = A_with_USER_ID$USER_ID
arr2 = df2$ding_userid

# 计算相同的元素
common_elements <- intersect(arr1, arr2)

# 计算每个数组中独有的元素
unique_arr1 <- setdiff(arr1, arr2)
unique_arr2 <- setdiff(arr2, arr1)

# 输出结果
cat("相同的元素: ", common_elements, "\n")
cat("相同的元素数量: ", length(common_elements), "\n")
cat("arr1 独有的元素数量: ", length(unique_arr1), "\n")
cat("arr2 独有的元素数量: ", length(unique_arr2), "\n")



unique(B_with_USER_ID$njname)



B_with_USER_ID$ding_userid



df3 <- read.csv("情绪声呐数据.csv", fileEncoding = "GBK")

df3 <- df3 %>%
  mutate(
    sex = case_when(
      sex == "男" ~ 1,
      sex == "女" ~ 0,
      TRUE ~ NA_real_  # 处理其他情况，设置为NA
    )
  )

colnames(df3)
df3$grade_name
df3 = df3[,c("ding_userid","grade_name","sex")]


C_with_USER_ID <- merge(B_with_USER_ID, df3, by.x = "USER_ID", by.y = "ding_userid", all.x = TRUE)
table(C_with_USER_ID$grade_name)
C_with_USER_ID$njname










df = C_with_USER_ID
df$grade <- ifelse(is.na(df$njname), df$grade_name, df$njname)
unique(df$grade)

df <- df %>%
  mutate(
    grade = case_when(
      grade == "八年级2021级" ~ 13,
      grade == "三年级2021级" ~ 14,
      grade == "一年级2023级" ~ 15,
      grade == "六年级2023级" ~ 11,
      grade == "二年级2022级" ~ 16,
      grade == "2023级" ~ NA_real_,  # 使用NA_real_表示数值型的NA
      grade == "高一" ~ 15,
      grade == "高二" ~ 16,
      grade == "高三" ~ 17,
      grade == "七年级2023级" ~ 12,
      grade == "七年级2022级" ~ 12,
      TRUE ~ as.numeric(grade)  # 处理其他情况，保持原样（如果grade已经是数值型）
    )
  )

table(df$grade)
# 11   12   13   14   15   16   17 
# 34    8  615  405  628 1197  181 
# 636人普通版，2432人成长版



df <- df %>%
  mutate(
    sex = coalesce(sex.x, sex.y)
  )

colnames(df)
table(df$sex)

df <- df[, colSums(is.na(df)) < nrow(df)]

df <- df %>%
  filter(!is.na(sex) & !is.na(age))

df <- df %>%
  mutate(scorelevel = case_when(
    scorelevel == "及格" ~ 2,
    scorelevel == "良好" ~ 3,
    scorelevel == "优秀" ~ 4,
    scorelevel == "不及格" ~ 1,
    TRUE ~ NA_real_  # 如果有其他值，赋值为NA
  ))

table(df$grade)
#  -5   11   12*   13   14   15*   16 
# 115   34    8   615  364   510 1074 
# 518人普通版，2087人成长版
head(df)

#df <- df %>% 
#  distinct(USER_ID, GLOBAL_ID, .keep_all = TRUE)

table(df$grade)
#  11     13   14   15*   16 
#  3      605  332  394 1062 
# 394人普通版，2072人成长版
head(df)

df_common <- df[df$grade %in% c(12, 15), ]
df_grown <- df[df$grade %in% c(11, 13, 14, 16), ]

df_common <- df_common[,c(
  
  "人格因素","人际敏感","人际讨好","创伤事件",
  "厌学倾向","坚持性","家庭关系","心理健康","情绪状况","效度量表",
  "效能感","睡眠状况","网络依赖","自我伤害","自我伤害倾向","诱因" ,
  "不被关注","敌意",
  
  colnames(df)[21:75],
  
  "age","sex","scorelevel"
)]


df_grown <- df_grown[,c("USER_ID", "GLOBAL_ID",
                        
  "人格因素","人际敏感","人际讨好","创伤事件",
  "厌学倾向","坚持性","家庭关系","心理健康","情绪状况","效度量表",
  "效能感","睡眠状况","网络依赖","自我伤害","自我伤害倾向","诱因" ,
  "不被关注","敌意",
  
  colnames(df)[21:75],
  
  "age","sex","scorelevel"
)]
df_common <- df_common %>% 
  select(where(~ !all(is.na(.x))))

df_common = as.data.frame(df_common)
df_grown = as.data.frame(df_grown)

# 去掉含缺失值的列
df_common <- df_common[, colSums(is.na(df_common)) == 0]

# df_grown <- df_grown[!apply(df_grown[, 55:73], 1, function(x) all(is.na(x))), ]
# df_grown <- df_grown[, colSums(is.na(df_grown)) == 0]

# 去掉标准差为 0 的列
df_common <- df_common[, sapply(df_common, function(x) sd(x, na.rm = TRUE) > 0)]
# df_grown <- df_grown[, sapply(df_grown, function(x) sd(x, na.rm = TRUE) > 0)]

write.csv(df_common,"综合问卷(普通版).csv")
write.csv(df_grown,"综合问卷(成长版+个人信息+去重版).csv")

common_columns <- intersect(colnames(df_common), colnames(df_grown))
# 按公共列合并数据
df_combined <- merge(df_common[, common_columns], df_grown[, common_columns], by = common_columns, all = TRUE)
write.csv(df_combined,"综合问卷(综合版+去重版).csv")

# table(df1_$`中国的首都是哪一座城市？`)
# table(df1_1$`中国的首都是哪一座城市？`)
# table(df1_2$`中国的首都是哪一座城市？`)
# table(df1_$中国的首都是哪一座城市)
# df = df[df$`你目前的身份是下面哪一种？` == "1",]
# df = df[df$你目前的身份是下面哪一种 == "1",]
# df = df[df$`中国的首都是哪一座城市？` == "2",]
# df = df[df$中国的首都是哪一座城市 == "2",]

write.csv(df,"综合问卷(全面版).csv")


library(MplusAutomation)
library(mclust)
library(tidyLPA)
 # 厌学倾向
school_select <- select(df_common, '我不喜欢上学', '我觉得学习很累很烦', '我希望可以不用学习')
# 转为数值型变量
school_select <- school_select %>%
  mutate(across(everything(), as.numeric))
# 使用estimate_profiles进行聚类分析，范围是1到6个聚类
school_cluster <- estimate_profiles(school_select, 1:6)

plot_profiles(school_cluster, rawdata = F, sd = F, ci = F, add_line = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.4, vjust = 0.5))

fit_schoolresult <- get_fit(school_cluster)
cla_schoolresult <- school_cluster$model_1_class_4$dff
write.csv(fit_schoolresult, "fit_schoolresult.csv")
write.csv(cla_schoolresult, "cla_schoolresult.csv")

class_counts <- table(school_cluster$model_1_class_4$model$classification)
total_samples <- sum(class_counts)
class_proportions <- class_counts/total_samples
print(class_proportions)


school_result <- cbind(school_select, school_cluster$model_1_class_5$model$classification)

fit_temp <- get_fit(school_cluster)
print(colnames(fit_temp))

fit_df <- get_fit(school_cluster) %>%
  rename(
    Classes = Classes,  # 类别数量列名直接是Classes，无需修改
    AIC = AIC,
    BIC = BIC,
    aBIC = SABIC,      # 校正BIC对应列名是SABIC
    Entropy = Entropy,
    "BLRT(p)" = BLRT_p  # BLRT检验p值对应列名是BLRT_p
  )

# 计算每个类别数的潜在类别比例
prop_list <- lapply(1:6, function(k) {
  # 构建模型在school_cluster中的名称（如"model_1_class_1"、"model_1_class_2"）
  model_name <- paste0("model_1_class_", k)
  
  # 检查该模型是否存在
  if (!model_name %in% names(school_cluster)) {
    return(NA)  # 若模型不存在，返回NA
  }
  
  # 提取该模型的分类结果
  classifications <- school_cluster[[model_name]]$model$classification
  
  # 计算类别数量和比例
  class_counts <- table(classifications)
  class_props <- round(class_counts / sum(class_counts), 2)
  
  # 格式化为"0.12:0.15:0.70"形式
  paste(class_props, collapse = ": ")
})

# 后续代码保持不变，继续合并并输出表格
fit_table <- fit_df %>%
  mutate(
    "潜在类别比例" = unlist(prop_list)
  ) %>%
  select(Classes, AIC, BIC, aBIC, Entropy, "BLRT(p)", "潜在类别比例")

print(fit_table, row.names = FALSE)
write.csv(fit_table, "厌学倾向潜在类别分析结果表格.csv", row.names = FALSE, fileEncoding = "UTF-8")



# 网络依赖
internet_select <- select(df_common, "我只要有一段时间没有上网、看手机，就会莫名地情绪低落", 
                          "长时间网游，使我的身体健康状况越来越不如以前了", 
                          "由于上网使我与周围其他人的关系没以前好了，但我无法减少上网时间")
# 转为数值型变量
internet_select <- internet_select %>%
  mutate(across(everything(), as.numeric))
# 使用estimate_profiles进行聚类分析，范围是1到6个聚类
internet_cluster <- estimate_profiles(internet_select, 1:6)

plot_profiles(internet_cluster, rawdata = F, sd = F, ci = F, add_line = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.4, vjust = 0.5))
internet_schoolresult <- get_fit(internet_cluster)

fit_internetresult <- get_fit(internet_cluster)
cla_internetresult <- internet_cluster$model_1_class_4$dff
write.csv(fit_internetresult, "fit_internetresult.csv")
write.csv(cla_internetresult, "cla_internetresult.csv")

fit_temp <- get_fit(internet_cluster)
print(colnames(fit_temp))

fit_df <- get_fit(internet_cluster) %>%
  rename(
    Classes = Classes,  # 类别数量列名直接是Classes，无需修改
    AIC = AIC,
    BIC = BIC,
    aBIC = SABIC,      # 校正BIC对应列名是SABIC
    Entropy = Entropy,
    "BLRT(p)" = BLRT_p  # BLRT检验p值对应列名是BLRT_p
  )

# 计算每个类别数的潜在类别比例
prop_list <- lapply(1:6, function(k) {
  # 构建模型在internet_cluster中的名称（如"model_1_class_1"、"model_1_class_2"）
  model_name <- paste0("model_1_class_", k)
  
  # 检查该模型是否存在
  if (!model_name %in% names(internet_cluster)) {
    return(NA)  # 若模型不存在，返回NA
  }
  
  # 提取该模型的分类结果
  classifications <- internet_cluster[[model_name]]$model$classification
  
  # 计算类别数量和比例
  class_counts <- table(classifications)
  class_props <- round(class_counts / sum(class_counts), 2)
  
  # 格式化为"0.12:0.15:0.70"形式
  paste(class_props, collapse = ": ")
})

# 后续代码保持不变，继续合并并输出表格
fit_table <- fit_df %>%
  mutate(
    "潜在类别比例" = unlist(prop_list)
  ) %>%
  select(Classes, AIC, BIC, aBIC, Entropy, "BLRT(p)", "潜在类别比例")

print(fit_table, row.names = FALSE)
write.csv(fit_table, "网络依赖潜在类别分析结果表格.csv", row.names = FALSE, fileEncoding = "UTF-8")


# 创伤事件
injury_select <- select(df_common, "别人给我起难听的外号，骂我，或取笑、讽刺我", 
                        "别人强迫向我要钱，或者拿走、损坏我的东西", 
                        "某些同学采用打、踢、推、撞等方式欺负我")
# 转为数值型变量
injury_select <- injury_select %>%
  mutate(across(everything(), as.numeric))
# 使用estimate_profiles进行聚类分析，范围是1到6个聚类
injury_cluster <- estimate_profiles(injury_select, 1:6)

plot_profiles(injury_cluster, rawdata = F, sd = F, ci = F, add_line = T) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.4, vjust = 0.5))
fit_injuryresult <- get_fit(injury_cluster)

fit_injuryresult <- get_fit(injury_cluster)
cla_injuryresult <- injury_cluster$model_1_class_4$dff
write.csv(fit_injuryresult, "fit_injuryresult.csv")
write.csv(cla_injuryresult, "cla_injuryresult.csv")

fit_temp <- get_fit(injury_cluster)
print(colnames(fit_temp))

fit_df <- get_fit(injury_cluster) %>%
  rename(
    Classes = Classes,  # 类别数量列名直接是Classes，无需修改
    AIC = AIC,
    BIC = BIC,
    aBIC = SABIC,      # 校正BIC对应列名是SABIC
    Entropy = Entropy,
    "BLRT(p)" = BLRT_p  # BLRT检验p值对应列名是BLRT_p
  )

# 计算每个类别数的潜在类别比例
prop_list <- lapply(1:6, function(k) {
  # 构建模型在injury_cluster中的名称（如"model_1_class_1"、"model_1_class_2"）
  model_name <- paste0("model_1_class_", k)
  
  # 检查该模型是否存在
  if (!model_name %in% names(injury_cluster)) {
    return(NA)  # 若模型不存在，返回NA
  }
  
  # 提取该模型的分类结果
  classifications <- injury_cluster[[model_name]]$model$classification
  
  # 计算类别数量和比例
  class_counts <- table(classifications)
  class_props <- round(class_counts / sum(class_counts), 2)
  
  # 格式化为"0.12:0.15:0.70"形式
  paste(class_props, collapse = ": ")
})

# 后续代码保持不变，继续合并并输出表格
fit_table <- fit_df %>%
  mutate(
    "潜在类别比例" = unlist(prop_list)
  ) %>%
  select(Classes, AIC, BIC, aBIC, Entropy, "BLRT(p)", "潜在类别比例")

print(fit_table, row.names = FALSE)
write.csv(fit_table, "网络依赖潜在类别分析结果表格.csv", row.names = FALSE, fileEncoding = "UTF-8")


