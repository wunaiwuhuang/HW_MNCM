setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")
load("data_clean.rdata")

library(ggpubr)
library(ggplot2)
library(patchwork)

data <- data[,c("dis_num","dis_rank","nul_all","nul_rank")]
# 将 dis_num 转换为分组变量
data$dis_num_cat <- cut(data$dis_num,
                        breaks = c(-1, 0, 1, 2, 3, Inf),
                        labels = c("0", "1", "2", "3", "4+"),
                        right = TRUE)

# 转换为有序因子，保证分组顺序正确
data$dis_num_cat <- factor(data$dis_num_cat, levels = c("0", "1", "2", "3", "4+"), ordered = TRUE)
# 确保 nul_rank 也是因子类型（有序因子更好）
data$nul_rank <- factor(data$nul_rank, ordered = TRUE)
# 确保 dis_rank 也是因子类型（有序因子更好）
data$dis_rank <- factor(data$dis_rank, ordered = TRUE)

# 公共样式缩小版
base_theme <- theme_minimal(base_size = 7) +
  theme(
    plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 7),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    plot.subtitle = element_text(size = 6, face = "italic", hjust = 0.5)
  )
  
# --- p1: dis_num_cat vs nul_all ---
comparisons <- combn(levels(data$dis_num_cat), 2, simplify = FALSE)
p1 <- ggboxplot(data, x = "dis_num_cat", y = "nul_all",
                color = "dis_num_cat", add = "jitter", add.params = list(size = 0.5),palette = "jco",
                size = 0.4, outlier.size = 0.5) +
  stat_compare_means(comparisons = comparisons, method = "wilcox.test", label = "p.signif", size = 2.5) +
  labs(title = "Malnutrition score across comorbidity burden",
       x = "Number of Comorbidities", y = "Malnutrition Score") +
  base_theme

# --- p2: dis_num_cat vs nul_rank ---
tab2 <- table(data$dis_num_cat, data$nul_rank)
fisher_result2 <- fisher.test(tab2, simulate.p.value = TRUE, B = 10000)
df_bar2 <- as.data.frame(tab2)
colnames(df_bar2) <- c("dis_num_cat", "nul_rank", "Freq")
p2 <- ggplot(df_bar2, aes(x = dis_num_cat, y = Freq, fill = nul_rank)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Number of Comorbidities", y = "Proportion", fill = "Malnutrition Rank",
       title = "Stacked Proportion: malnutrition rank",
       subtitle = paste0("Fisher test p = ", signif(fisher_result2$p.value, 3))) +
  base_theme

# --- p3: dis_rank vs nul_all ---
comparisons2 <- combn(levels(data$dis_rank), 2, simplify = FALSE)
p3 <- ggboxplot(data, x = "dis_rank", y = "nul_all",
                color = "dis_rank", add = "jitter", palette = "jco",add.params = list(size = 0.5),
                size = 0.4, outlier.size = 0.5) +
  stat_compare_means(comparisons = comparisons2, method = "wilcox.test", label = "p.signif", size = 2.5) +
  labs(title = "Malnutrition score across comorbidity rank",
       x = "Comorbidity Rank", y = "Malnutrition Score") +
  base_theme

# --- p4: dis_rank vs nul_rank ---
tab4 <- table(data$dis_rank, data$nul_rank)
fisher_result4 <- fisher.test(tab4, simulate.p.value = TRUE, B = 10000)
df_bar4 <- as.data.frame(tab4)
colnames(df_bar4) <- c("dis_rank", "nul_rank", "Freq")
p4 <- ggplot(df_bar4, aes(x = dis_rank, y = Freq, fill = nul_rank)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Comorbidity Rank", y = "Proportion", fill = "Malnutrition Rank",
       title = "Stacked Proportion: malnutrition rank",
       subtitle = paste0("Fisher test p = ", signif(fisher_result4$p.value, 3))) +
  base_theme

# 第一组：p1 + p2（左右排布）
combined1 <- p1 + p2 + plot_layout(ncol = 2)

ggsave(filename = "./results/14a.disnum_with_nul_combined.pdf",
       plot = combined1,
       width = 12, height = 6, units = "cm")

# 第二组：p3 + p4（左右排布）
combined2 <- p3 + p4 + plot_layout(ncol = 2)

ggsave(filename = "./results/14b.disrank_with_nul_combined.pdf",
       plot = combined2,
       width = 12, height = 6, units = "cm")