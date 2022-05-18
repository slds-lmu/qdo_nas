library(data.table)  # 1.14.2
results = readRDS("instance.rds")
# Table 3
results$archive$best()[, c("niche", "params", "val_err")]  # baseline results in MobileNetPruning.ipynb
