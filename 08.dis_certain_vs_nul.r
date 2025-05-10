setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")
load("data_clean.rdata")

library(ggplot2)
library(reshape2)
library(scales)
library(ggpubr)

# -------- 1. 相关性热图 --------
data_cor <- data[, c("dis_2", "dis_4", "dis_6", paste0("nul_", 1:7))]

dis_vars <- c("dis_2", "dis_4", "dis_6")
nul_vars <- paste0("nul_", 1:7)

cor_mat <- matrix(NA, nrow = length(dis_vars), ncol = length(nul_vars),
                  dimnames = list(dis_vars, nul_vars))
p_mat <- matrix(NA, nrow = length(dis_vars), ncol = length(nul_vars),
                dimnames = list(dis_vars, nul_vars))

for (i in dis_vars) {
  for (j in nul_vars) {
    test <- cor.test(as.numeric(data_cor[[i]]), data_cor[[j]], method = "spearman")
    cor_mat[i, j] <- test$estimate
    p_mat[i, j] <- test$p.value
  }
}

df_plot <- melt(cor_mat)
colnames(df_plot) <- c("dis", "nul", "cor")
df_plot$p_value <- melt(p_mat)$value
df_plot$label <- ifelse(df_plot$p_value <= 0.001, "***",
                        ifelse(df_plot$p_value <= 0.01, "**",
                               ifelse(df_plot$p_value <= 0.05, "*", "ns")))

p_heat <- ggplot(df_plot, aes(x = dis, y = nul, fill = cor)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-0.4, 0.4), name = "Spearman\nCorrelation") +
  geom_text(aes(label = label), color = "black", size = 3.5) +
  theme_minimal(base_size = 9) +
  labs(title = "Correlation between Disease Type and Nutrition Index", x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, size = 10)
  )

ggsave(filename = "./results/15.correlation_dis_nul.pdf", plot = p_heat, width =12, height = 8, units = "cm")

# -------- 2. dis_x vs nul_rank / nul_all --------
data_box <- data[, c("dis_2", "dis_4", "dis_6", "nul_rank", "nul_all")]
data_box$nul_rank <- factor(data_box$nul_rank, ordered = TRUE)

dis_vars <- c("dis_2", "dis_4", "dis_6")
plot_list <- list()

for (var in dis_vars) {
  data_box[[var]] <- factor(data_box[[var]], ordered = TRUE)

  # Proportion bar plot
  tab <- table(data_box[[var]], data_box$nul_rank)
  fisher_p <- tryCatch({
    fisher.test(tab, workspace = 2e8, hybrid = TRUE)$p.value
  }, error = function(e) {
    fisher.test(tab, simulate.p.value = TRUE, B = 1e5)$p.value
  })
  df_bar <- as.data.frame(tab)
  colnames(df_bar) <- c("dis_level", "nul_rank", "Freq")
  
  p_bar <- ggplot(df_bar, aes(x = dis_level, y = Freq, fill = nul_rank)) +
    geom_bar(stat = "identity", position = "fill", width = 0.7) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      x = var,
      y = "Proportion",
      fill = "Malnutrition Rank",
      title = paste("Malnutrition Rank across", var),
      subtitle = paste0("Fisher test p = ", signif(fisher_p, 3))
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.subtitle = element_text(size = 7, face = "italic", hjust = 0.5),
      plot.title = element_text(size = 8)
    )

  # Box plot
  levels_vec <- levels(data_box[[var]])
  comparisons <- combn(levels_vec, 2, simplify = FALSE)

  p_box <- ggboxplot(data_box, x = var, y = "nul_all",
                     color = var, add = "jitter", palette = "jco",
                     size = 0.25, outlier.size = 0.6,
                     add.params = list(size = 0.6, alpha = 0.6)) +
    stat_compare_means(comparisons = comparisons, method = "wilcox.test", 
                       label = "p.signif", size = 2.2) +
    labs(
      x = var,
      y = "Malnutrition Score",
      title = paste("Malnutrition Score across", var)
    ) +
    theme_minimal(base_size = 8) +
    theme(
      plot.title = element_text(size = 8)
    )

  # Combine
  p_combined <- ggarrange(p_bar, p_box, ncol = 2, widths = c(1.2, 1), labels = c("A", "B"))
  plot_list[[var]] <- p_combined
}

for (name in names(plot_list)) {
  ggsave(
    filename = paste0("./results/16.dis_with_nul_certain_", name, ".pdf"),
    plot = plot_list[[name]],
    width = 12,
    height = 6,
    units = "cm"
  )
}
