library(data.table)  # 1.14.2
library(ggplot2)  # 3.3.5
library(pammtools)  # 0.5.7
library(mlr3misc)  # 0.10.0

# NOTE: ablation was only run for 100 evals and 30 repls

# data
results = readRDS("results/results_ablation.rds")
results$method = factor(paste0(results$surrogate, "_", results$acqopt), levels = c("ranger_mutation", "ranger_random", "nn_mutation", "nn_random"), labels = c("Random Forest + Mutation", "Random Forest + Random", "NN + Mutation", "NN + Random"))

# sum of validation error over niches
results_sum = results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl)]
results_sum_agg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(cumbudget, method)]
results_sum_agg[, init_budget := 10L * 108L]
results_sum_agg[, max_budget := 100L * 108L]

# anytime performance of summed validation error
g = ggplot(aes(x = cumbudget, y = mean, colour = method, fill = method), data = results_sum_agg[cumbudget >= init_budget & cumbudget <= max_budget]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Total Budget used (Epochs)", y = "Average Validation Error Summed over Niches", colour = "Variant", fill = "Variant") +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("plots/anytime_ablation.png", plot = g, device = "png", width = 8, height = 6)

# missing niches analysis
results[, missing := (incumbent == 100), by = .(niche, cumbudget, method, repl)]
results_missing = results[cumbudget == 100L * 108L, .(mean_missing = mean(missing)), by = .(niche, cumbudget, method)]
results_missing$niche = factor(results_missing$niche, levels = paste0("niche", 1:5), labels = paste0("Niche ", 1:5))
results_missing[mean_missing == 0, mean_missing := NA_real_]

