setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")

library(readxl)
data <- read_excel("anal_data_row.xlsx", sheet = 1)  
data <- data[,-c(6,7,9)]
colnames(data)<-c("dis_all","nul_all","gender","age","height","weight","bmi","nation","dialysis",paste0("dis_",1:18),paste0("nul_",1:7))
# if contain “汉”，then it is 1, else 0
data$nation <- ifelse(grepl("汉", data$nation), 1, 0)
# turn to numeric
data <- as.data.frame(data)
data[] <- lapply(data, function(x) as.numeric(as.character(x)))

# dis_10 is redinal disease, should be 2
data$dis_10 <- 2

#add dis_age, if age 50～59, then it is 1, 60～69, then it is 2, 70～79, then it is 3, 80～89, then it is 4, 90-99, else 0
data$dis_age <- ifelse(data$age >= 50 & data$age < 60, 1,
                       ifelse(data$age >= 60 & data$age < 70, 2,
                              ifelse(data$age >= 70 & data$age < 80, 3,
                                     ifelse(data$age >= 80 & data$age < 90, 4,
                                            ifelse(data$age >= 90 & data$age < 100, 5,0)))))
#calculate dis_all
data<-data[,c("dis_all","nul_all","gender","age","height","weight","bmi","nation","dialysis",paste0("dis_",1:18),"dis_age",paste0("nul_",1:7))]
colnames(data)[10:28]
data$dis_all <- rowSums(data[,c(10:28)], na.rm = TRUE)

#dis_all 0-1 is 0(very light)，2-3 is 1(light)，4-5 is 2(mediate)，≥6 is 3(severe)
data$dis_rank<- ifelse(data$dis_all == 0, 0,
                        ifelse(data$dis_all <= 3, 1,
                               ifelse(data$dis_all <= 5, 2, 3)))

#for each row,calculate dis_x != 0 number
data$dis_num <- rowSums(data[,c(paste0("dis_",1:18))] != 0) # without dis_age

#nul_all <=10 is 1 (great), 11-15 is 2(light), 16-20 is 3(mediate), 21-35 is 4(high)
data$nul_rank <- ifelse(data$nul_all <= 10, 1,
                        ifelse(data$nul_all <= 15, 2,
                               ifelse(data$nul_all <= 20, 3, 4)))

# final clean
data <- data[,c("dis_all","dis_rank","dis_num","nul_all","nul_rank","gender","age","height","weight","bmi","nation","dialysis",paste0("dis_",1:18),"dis_age",paste0("nul_",1:7))]
data$dis_num <- data$dis_num - 1 # dis_num start from 0
#> table(data$dis_num)
# 0  1  2  3  4  5  6  7  8  9 10 15 
#77 68 53 75 54 28 11 15 11  9  1  1 

#for(x in 1:6){
#    print(11-x)
#   data <- data[data$dis_num <= (11-x),] 
#   a <-cor.test(data$dis_all, data$nul_all, method = "spearman")
#   b <-cor.test(data$dis_all, data$nul_rank, method = "spearman")
#   c <-cor.test(data$dis_rank, data$nul_all, method = "spearman")
#   d <-cor.test(data$dis_rank, data$nul_rank, method = "spearman")
#   e <-cor.test(data$dis_num, data$nul_all, method = "spearman")
#   f <-cor.test(data$dis_num, data$nul_rank, method = "spearman")
#   cat("all vs all: ","p-value:", a$p.value, " | correlation:", a$estimate, "\n")
#   cat("all vs rank: ","p-value:", b$p.value, " | correlation:", b$estimate, "\n")
#   cat("rank vs all: ","p-value:", c$p.value, " | correlation:", c$estimate, "\n")
#   cat("rank vs rank: ","p-value:", d$p.value, " | correlation:", d$estimate, "\n")
#   cat("num vs all: ","p-value:", e$p.value, " | correlation:", e$estimate, "\n")
#   cat("num vs rank: ","p-value:", f$p.value, " | correlation:", f$estimate, "\n")}

#[1] 10
#all vs all:#p-value: 4.667322e-07#| correlation: 0.2481776 
#all vs rank:#p-value: 0.0001102552#| correlation: 0.1916663 
#rank vs all:#p-value: 7.611431e-09#| correlation: 0.2830896 
#rank vs rank:#p-value: 1.25399e-05#| correlation: 0.2159518 
#num vs all:#p-value: 4.297254e-11#| correlation: 0.3211027 
#num vs rank:#p-value: 1.917945e-07#| correlation: 0.2561594 
#[1] 9
#all vs all:#p-value: 3.713709e-07#| correlation: 0.2505586 
#all vs rank:#p-value: 0.0001230614#| correlation: 0.1905921 
#rank vs all:#p-value: 6.905648e-09#| correlation: 0.2841992 
#rank vs rank:#p-value: 1.377788e-05#| correlation: 0.2152199 
#num vs all:#p-value: 3.072554e-11#| correlation: 0.3237713 
#num vs rank:#p-value: 2.177051e-07#| correlation: 0.2553476 
#[1] 8
#all vs all:#p-value: 4.3663e-09#| correlation: 0.2909549 
#all vs rank:#p-value: 3.340493e-06#| correlation: 0.2323162 
#rank vs all:#p-value: 8.665261e-10#| correlation: 0.3033646 
#rank vs rank:#p-value: 2.349023e-06#| correlation: 0.2358191 
#num vs all:#p-value: 5.767555e-14#| correlation: 0.3673041 
#num vs rank:#p-value: 1.255617e-09#| correlation: 0.3005703 
#[1] 7
#all vs all:#p-value: 2.480013e-10#| correlation: 0.3168547 
#all vs rank:#p-value: 1.929978e-07#| correlation: 0.2628682 
#rank vs all:#p-value: 3.205469e-10#| correlation: 0.3149682 
#rank vs rank:#p-value: 7.526676e-07#| correlation: 0.2502343 
#num vs all:#p-value: 9.380303e-16#| correlation: 0.3959844 
#num vs rank:#p-value: 2.143287e-11#| correlation: 0.3342257 
#[1] 6
#all vs all:#p-value: 2.970993e-10#| correlation: 0.3216304 
#all vs rank:#p-value: 1.438745e-07#| correlation: 0.2707312 
#rank vs all:#p-value: 5.468253e-10#| correlation: 0.3170231 
#rank vs rank:#p-value: 8.118332e-07#| correlation: 0.2544408 
#num vs all:#p-value: 3.208829e-16#| correlation: 0.409356 
#num vs rank:#p-value: 4.33568e-12#| correlation: 0.351596 
#[1] 5
#all vs all:#p-value: 7.587716e-09#| correlation: 0.300572 
#all vs rank:#p-value: 2.858566e-06#| correlation: 0.2454816 
#rank vs all:#p-value: 5.676877e-09#| correlation: 0.302983 
#rank vs rank:#p-value: 5.528505e-06#| correlation: 0.2385048 
#num vs all:#p-value: 1.291414e-14#| correlation: 0.3937734 
#num vs rank:#p-value: 1.494616e-10#| correlation: 0.3314991 

#above all , i think x = 7 is the best
data <- data[data$dis_num <= 7,]
save(data, file = "data_clean.rdata")

# 正太分布检验
# whether shapiro.test() is normal ?
load("data_clean.rdata")
vars <- c("dis_all", "dis_num", "nul_all", "age", "height", "weight", "bmi", "dialysis")
shapiro_results <- data.frame(
  Variable = character(),
  W = numeric(),
  P.value = numeric(),
  Normality = character(),
  stringsAsFactors = FALSE
)
for (i in vars) {
  result <- shapiro.test(data[[i]])
  shapiro_results <- rbind(shapiro_results, data.frame(
    Variable = i,
    W = result$statistic,
    P.value = result$p.value,
    Normality = ifelse(result$p.value > 0.05, "Yes", "No")
  ))
}
print(shapiro_results)

write.csv(data, file = "./results/01.data_for_analysis.csv", row.names = FALSE)
write.csv(shapiro_results, file = "./results/02.shapiro_results.csv", row.names = FALSE)