setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")


library(dplyr)
library(ggplot2)
library(broom)
library(car)
library(pROC)
library(patchwork)

# 1. 模型运行函数（返回模型对象列表） --------------------------
run_regression_models <- function(data, y, vars, type = c("linear", "logistic"), multivariate = FALSE) {
  type <- match.arg(type)
  models <- list()
  
  if (!multivariate) {
    for (v in vars) {
      formula <- as.formula(paste(y, "~", v))
      model <- if (type == "linear") lm(formula, data = data) else glm(formula, data = data, family = binomial)
      models[[v]] <- model
    }
  } else {
    formula <- as.formula(paste(y, "~", paste(vars, collapse = " + ")))
    model <- if (type == "linear") lm(formula, data = data) else glm(formula, data = data, family = binomial)
    models[["multivariate"]] <- model
  }
  return(models)
}

# 2. 模型提取函数（提取表格） ----------------------------------------
extract_results_from_models <- function(models, type = c("linear", "logistic")) {
  type <- match.arg(type)
  results <- data.frame()

  for (v in names(models)) {
    model <- models[[v]]
    coef_summary <- summary(model)$coefficients

    # 判断是否是多变量模型（名称为 "multivariate"）
    if (v == "multivariate") {
      for (var_name in rownames(coef_summary)[-1]) {  # 去掉截距项
        est <- coef_summary[var_name, 1]
        se <- coef_summary[var_name, 2]
        pval <- coef_summary[var_name, 4]
        
        p_signif <- ifelse(pval < 0.001, "***",
                           ifelse(pval < 0.01, "**",
                                  ifelse(pval < 0.05, "*",
                                         ifelse(pval < 0.1, ".", ""))))

        if (type == "linear") {
          tval <- est / se
          ci_lower <- est - 1.96 * se
          ci_upper <- est + 1.96 * se
          results <- rbind(results, data.frame(
            Variable = var_name,
            Estimate = est,
            Std.Error = se,
            t.value = tval,
            P.value = pval,
            P.signif = p_signif,
            CI_lower = ci_lower,
            CI_upper = ci_upper,
            stringsAsFactors = FALSE
          ))
        } else {
          OR <- exp(est)
          Wald <- (est^2) / (se^2)
          ci_lower <- exp(est - 1.96 * se)
          ci_upper <- exp(est + 1.96 * se)
          results <- rbind(results, data.frame(
            Variable = var_name,
            Estimate = est,
            Std.Error = se,
            OR = OR,
            Wald_Chi2 = Wald,
            P.value = pval,
            P.signif = p_signif,
            CI_lower = ci_lower,
            CI_upper = ci_upper,
            stringsAsFactors = FALSE
          ))
        }
      }
    } else {
      # 单变量情况仍只提取第一个自变量
      est <- coef_summary[2, 1]
      se <- coef_summary[2, 2]
      pval <- coef_summary[2, 4]
      
      p_signif <- ifelse(pval < 0.001, "***",
                         ifelse(pval < 0.01, "**",
                                ifelse(pval < 0.05, "*",
                                       ifelse(pval < 0.1, ".", ""))))
      
      if (type == "linear") {
        tval <- est / se
        ci_lower <- est - 1.96 * se
        ci_upper <- est + 1.96 * se
        results <- rbind(results, data.frame(
          Variable = v,
          Estimate = est,
          Std.Error = se,
          t.value = tval,
          P.value = pval,
          P.signif = p_signif,
          CI_lower = ci_lower,
          CI_upper = ci_upper,
          stringsAsFactors = FALSE
        ))
      } else {
        OR <- exp(est)
        Wald <- (est^2) / (se^2)
        ci_lower <- exp(est - 1.96 * se)
        ci_upper <- exp(est + 1.96 * se)
        results <- rbind(results, data.frame(
          Variable = v,
          Estimate = est,
          Std.Error = se,
          OR = OR,
          Wald_Chi2 = Wald,
          P.value = pval,
          P.signif = p_signif,
          CI_lower = ci_lower,
          CI_upper = ci_upper,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  return(results)
}

# 3.森林图绘制 ----------------------------------------
plot_forest <- function(result_table, type = c("linear", "logistic"), title = "Forest Plot") {
  type <- match.arg(type)
  
  # 数据处理
  df <- result_table %>%
    mutate(
      effect_size = if(type == "linear") Estimate else OR,
      CI_lower = CI_lower,
      CI_upper = CI_upper,
      CI_text = sprintf("%.2f–%.2f", CI_lower, CI_upper),
      p_stars = case_when(
        P.value < 0.001 ~ "***",
        P.value < 0.01 ~ "**",
        P.value < 0.05 ~ "*",
        TRUE ~ ""
      ),
      p_label = ifelse(P.value < 0.001, 
                      "<0.001",
                      sprintf("%.3f", P.value)),
      p_label_star = paste0("P: ",p_label,"  " ,p_stars),
      label = paste0(Variable, " (", CI_text, ")")
    )
    df$label <- factor(df$label, levels = rev(df$label))
  
  # 设置x轴范围和参考线
  ref_line <- ifelse(type == "linear", 0, 1)
  
  min_ci <- min(df$CI_lower, na.rm = TRUE)
  scale_factor <- case_when(
    min_ci > 0 ~ 0.8,    # 全为正数时缩小
    min_ci < 0 ~ 1.2,    # 含负数时扩大
    TRUE ~ 1.0           # 默认情况（含0时）
    )
  min_x <- min_ci * scale_factor    
  max_x <- max(df$CI_upper, na.rm = TRUE) * 2.5
  
  # 创建森林图
p <- ggplot(df, aes(x = effect_size, y = label)) +
  geom_point(size = 2, color = "#0072B5") +  # 点缩小为2
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), 
                 height = 0.15, color = "#0072B5", linewidth = 0.5) +  # 误差线细一点
  geom_vline(xintercept = ref_line, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_text(
    aes(x = ifelse(type == "linear", max(effect_size) * 3, max(effect_size) * 3.5), 
        label = p_label_star),
    hjust = 1, size = 3.3, color = "black"  # 字体缩小为3.3
  ) +
  labs(
    x = ifelse(type == "linear", "Coefficient", "OR"), 
    y = NULL, 
    title = title
  ) +
  theme_minimal(base_size = 10) +  # 整体字体缩小
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),  # Y轴标签更小
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10),  # 标题也缩小
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank()
  )

  
  # 根据类型调整x轴
  if (type == "logistic") {
    p <- p + scale_x_log10(limits = c(min_x, max_x))
  } else {
    p <- p + scale_x_continuous(limits = c(min_x, max_x))
  }
  
  return(p)
}

# 1. Linear Regression for all ----------------------------------------------------
  load("data_clean.rdata")
  data <- data[,c("dis_all","nul_all","gender","age","bmi","nation","dialysis")]
  vars <- setdiff(colnames(data), "nul_all")

  # linear regression -- single variable
  lin_uni_models <- run_regression_models(data, "nul_all", vars, type = "linear", multivariate = FALSE)
  lin_uni_table <- extract_results_from_models(lin_uni_models, type = "linear")
  View(lin_uni_table)
  p1 <- plot_forest(lin_uni_table, type = "linear", title = "Linear Regression (Univariate)")
  write.csv(lin_uni_table, file = "./results/08.linear_univariate_forest.csv", row.names = FALSE)

  # linear regression -- multiple variable
  lin_multi_models <- run_regression_models(data, "nul_all", vars, type = "linear", multivariate = TRUE)
  lin_multi_table <- extract_results_from_models(lin_multi_models, type = "linear")
  View(lin_multi_table)
  p2<-plot_forest(lin_multi_table, type = "linear", title = "Linear Regression (Multivariate)")
  write.csv(lin_multi_table, file = "./results/08.linear_multi_forest.csv", row.names = FALSE)

  # 上下排列两个图
  p_combined <- p1 / p2 + plot_layout(heights = c(1, 1))
  # 保存合并图像，宽 12cm，总高设为约 10cm（视图内容可微调）
  ggsave(filename = "./results/09.linear_combined_forest.pdf", 
        plot = p_combined, width = 12, height = 10, units = "cm")
  # visualization
      multi_model <- lin_multi_models$multivariate  # 可用于后续诊断、VIF
      # 残差图（诊断图）
      pdf("./results/08.linear_multivariate_diagnostic_plots.pdf", width = 8, height = 8)
      par(mfrow = c(2, 2))
      plot(multi_model)  # 基本的4个诊断图：残差vs拟合值、QQ图、尺度位置、杠杆值
      dev.off()
      # VIF（方差膨胀因子）用于检查多重共线性
      vif_values <- vif(multi_model)
      print(vif_values) 
      write.csv(vif_values, file = "./results/08.linear_multivariate_vif.csv", row.names = TRUE)

# 2. Logistic Regression for all ----------------------------------------------------
  load("data_clean.rdata")
  data <- data[,c("dis_all","nul_all","gender","age","bmi","nation","dialysis")]
  data$nul_all_bin <- ifelse(data$nul_all > 10, 1, 0) #1 means yes
  vars <- setdiff(colnames(data), c("nul_all", "nul_all_bin"))

  # logistic regression -- single variable
  logi_uni_models <- run_regression_models(data, "nul_all_bin", vars, type = "logistic", multivariate = FALSE)
  logi_uni_table <- extract_results_from_models(logi_uni_models, type = "logistic")
  View(logi_uni_table)
  p1<-plot_forest(logi_uni_table, type = "logistic", title = "Logistic Regression (Univariate)")
  write.csv(logi_uni_table, file = "./results/10.logistic_univariate_forest.csv", row.names = FALSE)

  # logistic regression -- multiple variable
  logi_multi_models <- run_regression_models(data, "nul_all_bin", vars, type = "logistic", multivariate = TRUE)
  logi_multi_table <- extract_results_from_models(logi_multi_models, type = "logistic")
  View(logi_multi_table)
  p2<-plot_forest(logi_multi_table, type = "logistic", title = "Logistic Regression (Multivariate)")
  write.csv(logi_multi_table, file = "./results/10.logistic_multi_forest.csv", row.names = FALSE)
  
  # 上下排列两个图
  p_combined <- p1 / p2 + plot_layout(heights = c(1, 1))
  # 保存合并图像，宽 12cm，总高设为约 10cm（视图内容可微调）
  ggsave(filename = "./results/10.logistic_combined_forest.pdf", 
        plot = p_combined, width = 12, height = 10, units = "cm")
  # visualization
      multi_model <- logi_multi_models$multivariate  # 可用于后续诊断、VIF
      vif(multi_model)
      # ROC曲线           
      prob <- predict(multi_model, type = "response")
      roc_obj <- roc(data$nul_all_bin, prob)
      pdf("./results/10.logistic_multivariate_ROC.pdf", width = 6, height = 6)
      par(mfrow = c(1, 1))
      plot(roc_obj, col = "blue", main = "ROC Curve for Logistic Regression")
      dev.off()
      auc_value <- auc(roc_obj)
      print(auc_value)
      write.csv(data.frame(AUC = auc_value), "./results/10.logistic_multivariate_auc.csv", row.names = FALSE)
      vif_values <- vif(multi_model)
      print(vif_values)
      write.csv(vif_values, file = "./results/10.logistic_multivariate_vif.csv", row.names = TRUE)

# 3. Linear Regression for dis ----------------------------------------------------
  load("data_clean.rdata")
  data <- data[,c("nul_all",paste0("dis_",1:18))]
  data <- data[sapply(data, function(x) length(unique(x)) > 1)] #remove constant columns
  vars <- setdiff(colnames(data), "nul_all")

  # linear regression -- single variable
  lin_uni_models <- run_regression_models(data, "nul_all", vars, type = "linear", multivariate = FALSE)
  lin_uni_table <- extract_results_from_models(lin_uni_models, type = "linear")
  View(lin_uni_table)
  p1<-plot_forest(lin_uni_table, type = "linear", title = "Linear Regression (Univariate)")
  write.csv(lin_uni_table, file = "./results/11.linear_univariate_forest.csv", row.names = FALSE)

  # linear regression -- multiple variable
  lin_multi_models <- run_regression_models(data, "nul_all", vars, type = "linear", multivariate = TRUE)
  lin_multi_table <- extract_results_from_models(lin_multi_models, type = "linear")
  View(lin_multi_table)
  p2<-plot_forest(lin_multi_table, type = "linear", title = "Linear Regression (Multivariate)")
  write.csv(lin_multi_table, file = "./results/11.linear_multi_forest.csv", row.names = FALSE)

  # 上下排列两个图
  p_combined <- p1 / p2 + plot_layout(heights = c(1, 1))
  # 保存合并图像，宽 12cm，总高设为约 10cm（视图内容可微调）
  ggsave(filename = "./results/11.linear_combined_forest.pdf", 
        plot = p_combined, width = 12, height = 20, units = "cm")
  # visualization
      multi_model <- lin_multi_models$multivariate  # 可用于后续诊断、VIF
      # 残差图（诊断图）
      pdf("./results/11.linear_multivariate_diagnostic_plots.pdf", width = 8, height = 8)
      par(mfrow = c(2, 2))
      plot(multi_model)  # 基本的4个诊断图：残差vs拟合值、QQ图、尺度位置、杠杆值
      dev.off()
      # VIF（方差膨胀因子）用于检查多重共线性
      vif_values <- vif(multi_model)
      print(vif_values) 
      write.csv(vif_values, file = "./results/11.linear_multivariate_vif.csv", row.names = TRUE)

# 4. Logistic Regression for dis ----------------------------------------------------
  load("data_clean.rdata")
  data <- data[,c("nul_all",paste0("dis_",1:18))]
  data <- data[sapply(data, function(x) length(unique(x)) > 1)] #remove constant columns
  data$nul_all_bin <- ifelse(data$nul_all > 10, 1, 0)
  vars <- setdiff(colnames(data), c("nul_all", "nul_all_bin"))
  # 由于dis_13,dis_17计算结果溢出，故去除这两者
  vars <- vars[!vars %in% c("dis_13", "dis_17")]

  # logistic regression -- single variable
  logi_uni_models <- run_regression_models(data, "nul_all_bin", vars, type = "logistic", multivariate = FALSE)
  logi_uni_table <- extract_results_from_models(logi_uni_models, type = "logistic")
  View(logi_uni_table)
  p1<-plot_forest(logi_uni_table, type = "logistic", title = "Logistic Regression (Univariate)")
  write.csv(logi_uni_table, file = "./results/12.logistic_univariate_forest.csv", row.names = FALSE)

  # logistic regression -- multiple variable
  logi_multi_models <- run_regression_models(data, "nul_all_bin", vars, type = "logistic", multivariate = TRUE)
  logi_multi_table <- extract_results_from_models(logi_multi_models, type = "logistic")
  View(logi_multi_table)
  p2<-plot_forest(logi_multi_table, type = "logistic", title = "Logistic Regression (Multivariate)")
  write.csv(logi_multi_table, file = "./results/12.logistic_multi_forest.csv", row.names = FALSE)
  
  # 上下排列两个图
  p_combined <- p1 / p2 + plot_layout(heights = c(1, 1))
  # 保存合并图像，宽 12cm，总高设为约 10cm（视图内容可微调）
  ggsave(filename = "./results/12.logistic_combined_forest.pdf", 
        plot = p_combined, width = 12, height = 20, units = "cm")
  # visualization
      multi_model <- logi_multi_models$multivariate  # 可用于后续诊断、VIF
      vif(multi_model)
      # ROC曲线           
      prob <- predict(multi_model, type = "response")
      roc_obj <- roc(data$nul_all_bin, prob)
      pdf("./results/12.logistic_multivariate_ROC.pdf", width = 6, height = 6)
      par(mfrow = c(1, 1))
      plot(roc_obj, col = "blue", main = "ROC Curve for Logistic Regression")
      dev.off()
      auc_value <- auc(roc_obj)
      print(auc_value)
      write.csv(data.frame(AUC = auc_value), "./results/12.logistic_multivariate_auc.csv", row.names = FALSE)
      vif_values <- vif(multi_model)
      print(vif_values)
      write.csv(vif_values, file = "./results/12.logistic_multivariate_vif.csv", row.names = TRUE)