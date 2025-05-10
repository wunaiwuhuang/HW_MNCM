setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")
load("data_clean.rdata")
comorbidity_data <- data[, c(paste0("dis_",1:18))]   # 共病得分

library(UpSetR)

# if data > 0, than 1
comorbidity_data[comorbidity_data > 0] <- 1
# calculate each disease's number and frequency
comorbidity_freq <- colSums(comorbidity_data)
comorbidity_rate <- colMeans(comorbidity_data)
freq_df <- data.frame(
  disease = names(comorbidity_freq),
  frequency = comorbidity_freq,
  rate = comorbidity_rate
)
top6_diseases <- freq_df[order(-freq_df$frequency), "disease"][1:6]
top_comorbidity_data <- comorbidity_data[, top6_diseases]
upset(top_comorbidity_data, 
      nsets = 6, 
      nintersects = NA, 
      sets = rev(top6_diseases),
      order.by = "freq",
      main.bar.color = "steelblue", 
      sets.bar.color = "skyblue")

write.csv(freq_df, "./results/06.multimorbidity_freq.csv", row.names = FALSE)
cairo_pdf("./results/05.top6_comorbidities_upset.pdf", width = 4.72, height = 3.5)  # 12cm 宽度，适中高度
upset(top_comorbidity_data, 
      nsets = 6, 
      nintersects = NA, 
      sets = rev(top6_diseases),
      order.by = "freq",
      main.bar.color = "steelblue", 
      sets.bar.color = "skyblue")
dev.off()

############################# 中文版出图 #############################
# 中文名称对照表
disease_labels <- c(
  dis_1 = "心脏病", dis_2 = "高血压", dis_3 = "慢性肺疾病", dis_4 = "消化系统疾病",
  dis_5 = "轻微肝脏疾病", dis_6 = "糖尿病", dis_7 = "脑血管疾病", dis_8 = "周围性血管疾病",
  dis_9 = "结缔组织疾病", dis_10 = "中至重度肾脏疾病", dis_11 = "偏瘫", dis_12 = "痴呆",
  dis_13 = "淋巴瘤", dis_14 = "白血病", dis_15 = "五年内确诊任何肿瘤", 
  dis_16 = "严重肝脏疾病", dis_17 = "转移性实体肿瘤", dis_18 = "艾滋病"
)
