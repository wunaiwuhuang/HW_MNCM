# we choose dis_1,dis_2,dis_4,dis_6,dis_11
setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")
load("data_clean.rdata")

library(ggstatsplot)
library(ggpubr)
library(rlang)
library(patchwork)

data <- data[,c("dialysis","nul_all","nul_rank")]

# 1. dialysis and nul_all 之间 的相关性 --------------------------
    var_x <- "dialysis"
    var_y <- "nul_all"
    data <- as.data.frame(data)
    # 使用 ggscatterstats 绘图（动态变量名）
    p1<-ggscatterstats(data = data,
                x = !!sym(var_x),
                y = !!sym(var_y),
                type = "spearman",
                centrality.para = "mean",
                margins = "both",
                xfill = "#CC79A7",
                yfill = "#009E73",
                marginal.type = "histogram",
                title = paste("Relationship between", var_x, "and", var_y))

# 2. dialysis and nul_rank 之间 的相关性 --------------------------
    # 确保 nul_rank 为有序因子
    data$nul_rank <- factor(data$nul_rank, ordered = TRUE)
    # 筛选 dialysis <= 20 的数据用于展示
    data <- subset(data, dialysis <= 20)
    # 两两比较组合
    comparisons <- combn(levels(data$nul_rank), 2, simplify = FALSE)
    # 绘图 + Wilcoxon 两两检验
    p2<- ggboxplot(data,
                x = "nul_rank",
                y = "dialysis",
                color = "nul_rank",
                add = "jitter",
                palette = "jco") +
    stat_compare_means(comparisons = comparisons,
                        method = "wilcox.test",
                        label = "p.signif") +
    labs(title = "Dialysis and Malnutrition Rank",
        x = "Malnutrition Rank",
        y = "Dialysis")

# 左右排布
combined_plot <- p1 + p2 + plot_layout(ncol = 2)

# 输出为 12cm 宽度的 PDF 或 PNG（单位为 cm）
ggsave(filename = "./results/13.relation_dialysis_malnutrition_combined.pdf",
       plot = combined_plot,
       width =20, height = 12, units = "cm")  # 高度按比例设为6cm，视图内容可调