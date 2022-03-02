library(data.table)
results = readRDS("instance.rds")
results$archive$best()[, c("niche", "params", "val_err")]  # baseline in MobileNetPruning.ipynb
