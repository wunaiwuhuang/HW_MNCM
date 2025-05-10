setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")
load("data_clean.rdata")

# 加载所需包
library(mediation)

# 定义变量名
vars <- c("dialysis", "dis_all", "nul_all")

# 初始化结果数据框
mediation_results <- data.frame(
  X = character(),
  M = character(),
  Y = character(),
  ACME = numeric(),
  ADE = numeric(),
  Total_Effect = numeric(),
  Prop_Mediated = numeric(),
  p_ACME = numeric(),
  p_ADE = numeric(),
  p_Total_Effect = numeric(),
  stringsAsFactors = FALSE
)

# 三变量两两组合进行中介分析
for (X in vars) {
  for (M in vars[vars != X]) {
    for (Y in vars[vars != X & vars != M]) {

      # 1. mediator 模型
      model_mediator <- try(lm(as.formula(paste(M, "~", X)), data = data), silent = TRUE)
      if (inherits(model_mediator, "try-error")) next

      # 2. outcome 模型
      model_outcome <- try(lm(as.formula(paste(Y, "~", M, "+", X)), data = data), silent = TRUE)
      if (inherits(model_outcome, "try-error")) next

      # 3. mediation 分析
      mediation_result <- try(mediate(model_mediator, model_outcome, treat = X, mediator = M), silent = TRUE)
      if (inherits(mediation_result, "try-error")) next

      # 4. 保存结果，统一做 NULL 检查
      mediation_results <- rbind(mediation_results, data.frame(
        predictor = X,
        mediator = M,
        outcomer = Y,
        ACME = ifelse(is.null(mediation_result$d0), NA, mediation_result$d0),
        ADE = ifelse(is.null(mediation_result$z0), NA, mediation_result$z0),
        Total_Effect = ifelse(is.null(mediation_result$tau.coef), NA, mediation_result$tau.coef),
        Prop_Mediated = ifelse(is.null(mediation_result$prop.med), NA, mediation_result$prop.med),
        p_ACME = ifelse(is.null(mediation_result$d0.p), NA, mediation_result$d0.p),
        p_ADE = ifelse(is.null(mediation_result$z0.p), NA, mediation_result$z0.p),
        p_Total_Effect = ifelse(is.null(mediation_result$tau.p), NA, mediation_result$tau.p)
      ))
    }
  }
}

# 查看结果
print(mediation_results)
write.csv(mediation_results, "./results/17.mediation_results.csv", row.names = FALSE)

# X	M	Y	结论简述
# dialysis	dis_all	nul_all	直接有效，无中介效应
# dialysis	nul_all	dis_all	中介有效（p = 0.028），但总效应不显著
# dis_all	dialysis	nul_all	直接作用显著（p < 0.001），中介无效
# dis_all	nul_all	dialysis	中介作用显著（p = 0.018），但整体影响不显著
# nul_all	dialysis	dis_all	直接作用显著，中介不显著
# nul_all	dis_all	dialysis	没有显著中介或直接作用
# Mediation analysis revealed that dialysis duration had a significant direct effect on malnutrition (p = 0.020), without significant mediation through comorbidities. This suggests that the influence of dialysis duration on nutritional status is primarily direct.