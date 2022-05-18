library(data.table)  # 1.14.2
library(ggplot2)  # 3.3.5
library(pammtools)  # 0.5.7
library(mlr3misc)  # 0.10.0

# data
results = readRDS("results/results.rds")
results$feature_var = factor(results$feature_var, levels = c("latency", "flops"), labels = c("Latency", "Flops"))
results$niches = factor(results$niches, levels = c("small", "medium", "large"), labels = c("Small", "Medium", "Large"))
results$method = factor(results$method, levels = c("bop", "parego", "random_search"), labels = c("BOP-Elites*", "ParEGO*", "Random"))

results_2Fx = readRDS("results/results_2Fx.rds")
results_2Fx[, feature_var := "Latency x Size"]
results_2Fx$niches = factor(results_2Fx$niches, levels = c("small", "medium", "large"), labels = c("Small", "Medium", "Large"))
results_2Fx$method = factor(results_2Fx$method, levels = c("bop", "parego", "random_search"), labels = c("BOP-Elites*", "ParEGO*", "Random"))

results = rbind(results, results_2Fx)

# sum of validation error over niches
results_sum = results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl, scenario, instance, feature_var, niches)]
results_sum_agg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(cumbudget, method, scenario, instance, feature_var, niches)]
results_sum_agg[, init_budget := 10L]
results_sum_agg[, max_budget := 100L]
results_sum_agg[, header := paste0(niches, " | ", feature_var)]
results_sum_agg[, header := factor(header, levels = levels(factor(header))[c(8, 7, 9, 5, 4, 6, 2, 1, 3)])]

# Figure 3
g = ggplot(aes(x = cumbudget, y = mean, colour = method, fill = method), data = results_sum_agg[cumbudget >= init_budget & cumbudget <= max_budget]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Total Budget used (Architecture Evaluations)", y = "Average Validation Error Summed over Niches", colour = "Optimizer", fill = "Optimizer") +
  facet_wrap(~ header, scales = "free") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("plots/anytime_mobilenet.png", plot = g, device = "png", width = 9, height = 5.25)

# niche boundaries
library(xtable)
library(bbotk)
source("niches_overlapping.R")

problems = list(ofa_note10_small_nb, ofa_note10_medium_nb, ofa_note10_large_nb,
  ofa_flops_small_nb, ofa_flops_medium_nb, ofa_flops_large_nb)

format_nb = function(nb) {
  tmp = nb$niche_boundaries[[1L]]
  paste0("[", tmp[1L], ", ", tmp[2L], ")")
}

boundaries = map_dtr(problems, function(problem) {
  nbs = map(problem$niches, format_nb)
  n_fill = 10 - length(nbs)
  if (n_fill > 0) {
    nbs = c(nbs, replicate(n_fill, expr = "", simplify = FALSE))
  }
  names(nbs) = paste0("Niche ", seq_along(nbs))
  data.table(nbs = nbs, niche = names(nbs))
})
boundaries[, scenario := rep(c("MobileNetV3 Latency", "MobileNetV3 Flops"), each = 30)]
boundaries[, instance := "ImageNet"]
boundaries[, niches := rep(rep(c("Small", "Medium", "Large"), each = 10), 2)]
boundaries[, problem := paste0(scenario, "_", instance, "_", niches)]
boundaries[, niche := factor(niche, levels = paste0("Niche ", 1:10))]

print(xtable(dcast(boundaries, problem ~ niche, value.var = "nbs")[c(6, 5, 4, 3, 2, 1)]), include.rownames = FALSE)

source("niches_overlapping_2F.R")

problems = list(ofa_note10_small_nb, ofa_note10_medium_nb, ofa_note10_large_nb)

format_nb = function(nb) {
  tmp1 = nb$niche_boundaries[[1L]]
  tmp2 = nb$niche_boundaries[[2L]]
  paste0("[", tmp1[1L], ", ", tmp1[2L], ")", " x ", "[", tmp2[1L], ", ", tmp2[2L], ")")
}

boundaries = map_dtr(problems, function(problem) {
  nbs = map(problem$niches, format_nb)
  n_fill = 10 - length(nbs)
  if (n_fill > 0) {
    nbs = c(nbs, replicate(n_fill, expr = "", simplify = FALSE))
  }
  names(nbs) = paste0("Niche ", seq_along(nbs))
  data.table(nbs = nbs, niche = names(nbs))
})
boundaries[, scenario := "MobileNetV3 Latency x Size"]
boundaries[, instance := "ImageNet"]
boundaries[, niches := rep(c("Small", "Medium", "Large"), each = 10)]
boundaries[, problem := paste0(scenario, "_", instance, "_", niches)]
boundaries[, niche := factor(niche, levels = paste0("Niche ", 1:10))]

# Table 4
print(xtable(dcast(boundaries, problem ~ niche, value.var = "nbs")[c(3, 2, 1)]), include.rownames = FALSE)

