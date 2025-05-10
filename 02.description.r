setwd("D:\\skills\\生信接单\\20250504第八单帮写论文")
load("data_clean.rdata")

library(dplyr)
library(knitr)
library(kableExtra)
library(tidyr)

# 1. 连续变量三线表 ----------------------------------------------------

cont_vars <- c("dis_all", "nul_all", "age", "height", "weight", "bmi", "dialysis")
desc_data <- data[, cont_vars]

desc_table <- desc_data %>%
  summarise(across(everything(), list(
    n = ~sum(!is.na(.)),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE)
  ), .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_pattern = "^(.*)__(.*)$",
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# 输出三线表
kable(desc_table, format = "html", caption = "Table: Descriptive Statistics of Continuous Variables") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") %>%
  add_header_above(c(" " = 1, "Descriptive Statistics" = 5)) %>%
  row_spec(0, bold = TRUE)


# 2. 分类变量频数表 ----------------------------------------------------

# 处理 age 分组 & dis_num 分组
data_cat <- data %>%
  mutate(
    age_group = ifelse(age < 50, "<50", ">=50"),
    dis_num_group = case_when(
      dis_num == 0 ~ "0",
      dis_num == 1 ~ "1",
      dis_num == 2 ~ "2",
      dis_num == 3 ~ "3",
      dis_num >= 4 ~ "≥4",
      TRUE ~ NA_character_
    )
  )

cat_vars <- c("dis_rank", "dis_num_group", "nul_rank", "gender", "age_group", "nation")

freq_tables <- lapply(cat_vars, function(var) {
  df <- data_cat %>%
    group_by(across(all_of(var))) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(percent = round(n / sum(n) * 100, 1),
           Level = as.character(!!sym(var)),  # 统一 Level 为字符
           Variable = var) %>%
    dplyr::select(Variable, Level, n, percent)
  df
})

cat_table <- bind_rows(freq_tables) %>%
  dplyr::select(Variable, Level, n, percent) %>%
  mutate(Variable = ifelse(duplicated(Variable), NA, Variable))


write.csv(desc_table, "./results/03.desc_table.csv", row.names = FALSE)
write.csv(cat_table, "./results/04.cat_table.csv", row.names = FALSE)
