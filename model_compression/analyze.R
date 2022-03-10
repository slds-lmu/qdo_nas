library(data.table)  # 1.14.2
results = readRDS("instance.rds")
results$archive$best()[, c("niche", "params", "val_err")]  # baseline results in MobileNetPruning.ipynb
