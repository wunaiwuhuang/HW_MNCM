setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")

library(psych)        # 用于信度和因子分析
library(GPArotation)  # 用于因子旋转
library(lavaan)       # 用于验证性因子分析（CFA）

load("data_clean.rdata")
comorbidity_data <- data[, c(paste0("dis_",1:18))]   # 共病得分
comorbidity_data <- comorbidity_data[sapply(comorbidity_data, function(x) length(unique(x)) > 1)] #remove constant columns
nutrition_data   <- data[, c(paste0("nul_",1:7))]  # 营养评估

# 共病部分的信度分析
calpha<-psych::alpha(comorbidity_data)
cat("原始 Cronbach's α:", calpha$total$raw_alpha, "\n")
# 原始 Cronbach's α: 0.5891397 
cat("标准化 Cronbach's α:", calpha$total$std.alpha, "\n")
# 标准化 Cronbach's α: 0.5820412

# 营养状况部分的信度分析
nalpha<-psych::alpha(nutrition_data)
cat("原始 Cronbach's α:", nalpha$total$raw_alpha, "\n")
# 原始 Cronbach's α: 0.7813937 
cat("标准化 Cronbach's α:", nalpha$total$std.alpha, "\n")
# 标准化 Cronbach's α: 0.7975068


# if it is suitable for factor analysis
KMO(comorbidity_data)         # KMO > 0.6 较好,可以进行验证性检验
cortest.bartlett(comorbidity_data)

KMO(nutrition_data)
cortest.bartlett(nutrition_data)

# 先重命名列名方便建模（可选）
colnames(comorbidity_data) <- paste0("C", 1:15)
colnames(nutrition_data)   <- paste0("N", 1:7)

# 合并回主数据（如果要做整体 CFA）
df_model <- cbind(comorbidity_data, nutrition_data)

# 定义模型结构（一个共病因子 + 一个营养因子）
model <- '
  Comorbidity =~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + C11 + C12 + C13 + C14 + C15
  Nutrition   =~ N1 + N2 + N3 + N4 + N5 + N6 + N7
'

# 运行 CFA 模型
fit <- cfa(model, data = df_model)
cfa<-summary(fit, fit.measures = TRUE, standardized = TRUE)


# 提取标准化估计结果
std_solution <- standardizedSolution(fit)
# 筛选出 latent variables 的因子负荷
loadings_df <- subset(std_solution, op == "=~")
# 只保留变量名、因子、标准化因子负荷
loadings_df <- loadings_df[, c("lhs", "rhs", "est.std")]
colnames(loadings_df) <- c("Factor", "Item", "Standardized_Loading")
print(loadings_df)