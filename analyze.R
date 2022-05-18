library(data.table)  # 1.14.2
library(ggplot2)  # 3.3.5
library(pammtools)  # 0.5.7
library(mlr3misc)  # 0.10.0

# data
results = readRDS("results/results.rds")
results = results[method != "smsego"]
results = results[overlapping == TRUE]
results$scenario = factor(results$scenario, labels = c("NAS-Bench-101", "NAS-Bench-201"))
results$instance = factor(results$instance, labels = c("Cifar-10", "Cifar-100", "ImageNet16-120"))
results$niches = factor(results$niches, labels = c("Small", "Medium", "Large"))
results$method = factor(results$method, levels = c("bohb_qdo", "hb_qdo", "bop", "bohb_mo", "hb_mo", "parego", "random_search"), labels = c("BOP-ElitesHB", "qdHB", "BOP-Elites*", "ParEGOHB", "moHB*", "ParEGO*", "Random"))

best = readRDS("results/best.rds")
best = best[method != "smsego"]
best = best[overlapping == TRUE]
best$scenario = factor(best$scenario, labels = c("NAS-Bench-101", "NAS-Bench-201"))
best$instance = factor(best$instance, labels = c("Cifar-10", "Cifar-100", "ImageNet16-120"))
best$niches = factor(best$niches, labels = c("Small", "Medium", "Large"))
best$method = factor(best$method, levels = c("bohb_qdo", "hb_qdo", "bop", "bohb_mo", "hb_mo", "parego", "random_search"), labels = c("BOP-ElitesHB", "qdHB", "BOP-Elites*", "ParEGOHB", "moHB*", "ParEGO*", "Random"))

# safety check: best full must be <= best half
#diffs = map_dtr(unique(best$scenario), function(scenario_) {
#  map_dtr(unique(best$instance), function(instance_) {
#    map_dtr(unique(best$niches), function(niches_) {
#      map_dtr(unique(best$method), function(method_) {
#        map_dtr(unique(best$repl), function(repl_) {
#          res_full = best[scenario == scenario_ & instance == instance_ & niches == niches_ & method == method_ & repl == repl_ & type == "full"]
#          if (nrow(res_full) == 0L) {  # not all instances present for all scenarios
#            return(data.table())
#          }
#          res_half = best[scenario == scenario_ & instance == instance_ & niches == niches_ & method == method_ & repl == repl_ & type == "half"]
#          setorderv(res_full, "niche")
#          setorderv(res_half, "niche")
#          stopifnot(dim(res_full)[1L] == dim(res_half)[[1L]])
#          data.table(diff =res_full$val_loss - res_half$val_loss)
#        })
#      })
#    })
#  })
#})

pareto = readRDS("results/pareto.rds")
pareto = pareto[method != "smsego"]
pareto = pareto[overlapping == TRUE]
pareto$scenario = factor(pareto$scenario, labels = c("NAS-Bench-101", "NAS-Bench-201"))
pareto$instance = factor(pareto$instance, labels = c("Cifar-10", "Cifar-100", "ImageNet16-120"))
pareto$niches = factor(pareto$niches, labels = c("Small", "Medium", "Large"))
pareto$method = factor(pareto$method, levels = c("bohb_qdo", "hb_qdo", "bop", "bohb_mo", "hb_mo", "parego", "random_search"), labels = c("BOP-ElitesHB", "qdHB", "BOP-Elites*", "ParEGOHB", "moHB*", "ParEGO*", "Random"))

# sum of validation error over niches
results_sum = results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl, scenario, instance, niches)]
results_sum_agg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(cumbudget, method, scenario, instance, niches)]
results_sum_agg[scenario == "NAS-Bench-101", init_budget := 10L * 108L]
results_sum_agg[scenario == "NAS-Bench-201", init_budget := 10L * 200L]
results_sum_agg[scenario == "NAS-Bench-101", max_budget := 200L * 108L]
results_sum_agg[scenario == "NAS-Bench-201", max_budget := 200L * 200L]
results_sum_agg[, header := paste0(niches, " ", scenario, " ", instance)]
results_sum_agg[, header := factor(header, levels = levels(factor(header))[c(9:12, 5:8, 1:4)])]

# Figure 2
# anytime performance of summed validation error
g = ggplot(aes(x = cumbudget, y = mean, colour = method, fill = method), data = results_sum_agg[cumbudget >= init_budget & cumbudget <= max_budget]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Total Budget used (Epochs)", y = "Average Validation Error Summed over Niches", colour = "Optimizer", fill = "Optimizer") +
  facet_wrap(~ header, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("plots/anytime.png", plot = g, device = "png", width = 12, height = 7)

# average best final valid and test loss of the best final val loss architecture per niche
best_agg = best[, .(mean_test = mean(test_loss), se_test = sd(test_loss) / sqrt(.N), mean_val = mean(val_loss), se_val = sd(val_loss) / sqrt(.N)), by = .(scenario, instance, method, niches, niche, type)]
best_agg$niche = factor(best_agg$niche, levels = paste0("niche", 1:10), labels = paste0("Niche ", 1:10))
best_agg[, header := paste0(niches, " ", scenario, " ", instance)]
best_agg[, header := factor(header, levels = levels(factor(header))[c(9:12, 5:8, 1:4)])]

# Figure 6
g = ggplot(aes(x = method, y = mean_val, colour = niche), data = best_agg[type == "full"]) +
  scale_y_log10() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_val - se_val, ymax = mean_val + se_val), width = 0.2) +
  labs(x = "Optimizer", y = "Average Validation Error", colour = "") +
  facet_wrap(~ header, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = rel(0.85), angle = 45, hjust = 1))
ggsave("plots/best_val.png", plot = g, device = "png", width = 12, height = 9)

# Figure 7
g = ggplot(aes(x = method, y = mean_test, colour = niche), data = best_agg[type == "full"]) +
  scale_y_log10() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_test - se_test, ymax = mean_test + se_test), width = 0.2) +
  labs(x = "Optimizer", y = "Average Test Error", colour = "") +
  facet_wrap(~ header, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = rel(0.85), angle = 45, hjust = 1))
ggsave("plots/best_test.png", plot = g, device = "png", width = 12, height = 9)

# best summed validation and test performance over niches
best_sum = best[, .(overall_test = sum(test_loss), overall_val = sum(val_loss)), by = .(method, repl, scenario, instance, niches, type)]
best_sum_agg = best_sum[, .(mean_test = mean(overall_test), se_test = sd(overall_test) / sqrt(.N), mean_val = mean(overall_val), se_val = sd(overall_val) / sqrt(.N)), by = .(method, scenario, instance, niches, type)]
best_sum_agg[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]

# Table 1
methods = c("BOP-ElitesHB", "qdHB", "BOP-Elites*", "ParEGOHB", "moHB*", "ParEGO*", "Random")
# average rank based on the average final test loss summed over niches
ranks_valid = map_dtr(unique(best_sum_agg$scenario), function(scenario_) {
  map_dtr(unique(best_sum_agg$instance), function(instance_) {
    map_dtr(unique(best_sum_agg$niches), function(niches_) {
      res = best_sum_agg[scenario == scenario_ & instance == instance_ & niches == niches_ & type == "full"]
      if (nrow(res) == 0L) {  # not all instances present for all scenarios
        return(data.table())
      }
      setorderv(res, "mean_val")
      data.table(rank = match(methods, res$method), method = methods, scenario = scenario_, instance = instance_, niches = niches_)
    })
  })
})
ranks_valid[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
ranks_valid_agg = ranks_valid[, .(mean = mean(rank), se = sd(rank) / sqrt(.N)), by = .(method)]
# average rank based on the average final test loss summed over niches
ranks_test = map_dtr(unique(best_sum_agg$scenario), function(scenario_) {
  map_dtr(unique(best_sum_agg$instance), function(instance_) {
    map_dtr(unique(best_sum_agg$niches), function(niches_) {
      res = best_sum_agg[scenario == scenario_ & instance == instance_ & niches == niches_ & type == "full"]
      if (nrow(res) == 0L) {  # not all instances present for all scenarios
        return(data.table())
      }
      setorderv(res, "mean_test")
      data.table(rank = match(methods, res$method), method = methods, scenario = scenario_, instance = instance_, niches = niches_)
    })
  })
})
ranks_test[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
ranks_test_agg = ranks_test[, .(mean = mean(rank), se = sd(rank) / sqrt(.N)), by = .(method)]

# CD plots
# Figure 5 a) and b)
library(scmamp)  # 0.3.2
tmp = - as.matrix(dcast(best_sum_agg[type == "full"], problem ~ method, value.var = "mean_val")[, -1])
friedmanTest(tmp)  # val: Friedman's chi-squared = 53.464, df = 6, p-value = 9.46e-10
png("plots/cd_val.png", width = 6, height = 2, units = "in", res = 300, pointsize = 10)
plotCD(tmp)
dev.off()

tmp = - as.matrix(dcast(best_sum_agg[type == "full"], problem ~ method, value.var = "mean_test")[, -1])
friedmanTest(tmp)  # test: Friedman's chi-squared = 52.143, df = 6, p-value = 1.745e-09
png("plots/cd_test.png", width = 6, height = 2, units = "in", res = 300, pointsize = 10)
plotCD(tmp)
dev.off()

# anovas
# Table 5 and 6
library(xtable)  # 1.8-4
bold = function(x) {paste("\\textbf{", x, "}", sep = "")}

aov_dat_half = copy(best_sum_agg[type == "half" & method %nin% c("qdHB", "moHB*")])
aov_dat_half[, multifidelity := as.factor(method %in% c("BOP-ElitesHB", "qdHB", "ParEGOHB", "moHB*"))]
aov_dat_half[, qdo := as.factor(method %in% c("BOP-ElitesHB", "qdHB", "BOP-Elites*"))]
aov_dat_half[, model_based:= as.factor(method %in% c("BOP-ElitesHB", "BOP-Elites*", "ParEGOHB", "ParEGO*"))]
aov_dat_half[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
aov_model_half = aov(mean_val ~ problem + multifidelity + qdo + model_based, data = aov_dat_half)
summary(aov_model_half)
print(xtable(summary(aov_model_half)), sanitize.colnames.function = bold, booktabs = TRUE)
TukeyHSD(aov_model_half, c("multifidelity", "qdo", "model_based"))
lm_half = lm(mean_val ~ problem + multifidelity + qdo + model_based, data = aov_dat_half)
qqnorm(lm_half$residuals)
qqline(lm_half$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))
shapiro.test(residuals(lm_half))

aov_dat_full = copy(best_sum_agg[type == "full" & method %nin% c("qdHB", "moHB*")])
aov_dat_full[, multifidelity := as.factor(method %in% c("BOP-ElitesHB", "qdHB", "ParEGOHB", "moHB*"))]
aov_dat_full[, qdo := as.factor(method %in% c("BOP-ElitesHB", "qdHB", "BOP-Elites*"))]
aov_dat_full[, model_based:= as.factor(method %in% c("BOP-ElitesHB", "BOP-Elites*", "ParEGOHB", "ParEGO*"))]
aov_dat_full[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
aov_model_full = aov(mean_val ~ problem + multifidelity + qdo + model_based, data = aov_dat_full)
summary(aov_model_full)
print(xtable(summary(aov_model_full)), sanitize.colnames.function = bold, booktabs = TRUE)
TukeyHSD(aov_model_full, c("multifidelity", "qdo", "model_based"))
lm_full = lm(mean_val ~ problem + multifidelity + qdo + model_based, data = aov_dat_full)
qqnorm(lm_full$residuals)
qqline(lm_full$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))
shapiro.test(residuals(lm_full))

# mo analysis
worst = data.table(scenario = c("NAS-Bench-101", "NAS-Bench-201"), y1 = c(100, 100), y2 = c(49979275, 0.0283))
best_pareto = map_dtr(unique(pareto$scenario), function(scenario_) {
  map_dtr(unique(pareto$instance), function(instance_) {
    map_dtr(unique(pareto$niches), function(niches_) {
      res = pareto[scenario == scenario_ & instance == instance_ & niches == niches_]
      if (nrow(res) == 0L) {  # not all instances present for all scenarios
        return(data.table())
      }
      new_pareto = emoa::nondominated_points(do.call(cbind, res$pareto))
      data.table(pareto = list(new_pareto), scenario = scenario_, instance = instance_, niches = niches_)
    })
  })
}, .fill = TRUE)

get_hvi = function(pareto_, scenario_, instance_, niches_) {
  ref = t(worst[scenario == scenario_, c("y1", "y2")])
  best_pareto_ = best_pareto[scenario == scenario_ & instance == instance_ & niches == niches_]$pareto[[1L]]
  tmp = pareto_[[1L]]
  if (scenario_ == "NAS-Bench-101") {
    ref[2L, ] = log(ref[2L, ])
    tmp[2L, ] = log(tmp[2L, ])
    best_pareto_[2L, ] = log(best_pareto_[2L, ])
  }
  emoa::hypervolume_indicator(tmp, o = best_pareto_, ref = ref)
}

# Figure 12
pareto[, hvi := get_hvi(pareto, scenario, instance, niches), by = .(method, scenario, instance, niches, repl)]
pareto_agg = pareto[, .(mean_hvi = mean(hvi), se_hvi = sd(hvi) / sqrt(.N)), by = .(method, scenario, instance, niches)]
pareto_agg[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
tmp = - as.matrix(dcast(pareto_agg, problem ~ method, value.var = "mean_hvi")[, -1])
friedmanTest(tmp)  # Friedman's chi-squared = 41.607, df = 6, p-value = 2.198e-07
png("plots/cd_hvi.png", width = 6, height = 2, units = "in", res = 300, pointsize = 10)
plotCD(tmp)
dev.off()

pareto_agg[, header := paste0(" ", scenario, " ", instance)]
pareto_agg[, header := factor(header)]

# Figure 11
g = ggplot(aes(x = method, y = mean_hvi, colour = niches), data = pareto_agg) +
  scale_y_log10() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_hvi - se_hvi, ymax = mean_hvi + se_hvi), width = 0.2) +
  labs(x = "Optimizer", y = "Average HVI", colour = "Niches") +
  facet_wrap(~ header, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = rel(0.85), angle = 45, hjust = 1))
ggsave("plots/hvi.png", plot = g, device = "png", width = 12, height = 9/3)

pareto_long = map_dtr(unique(pareto$scenario), function(scenario_) {
  map_dtr(unique(pareto$instance), function(instance_) {
    map_dtr(unique(pareto$niches), function(niches_) {
      map_dtr(unique(pareto$method), function(method_) {
        res = pareto[scenario == scenario_ & instance == instance_ & niches == niches_ & method == method_]
        if (nrow(res) == 0L) {  # not all instances present for all scenarios
          return(data.table())
        }
        tmp = emoa::nondominated_points(do.call(cbind, res$pareto))
        cbind(as.data.table(t(tmp)), data.table(scenario = scenario_, instance = instance_, niches = niches_, method = method_))
      })
    })
  })
}, .fill = TRUE)

pareto_long[, num_params := log(num_params)]
pareto_long[scenario == "NAS-Bench-101", y2 := num_params]
pareto_long[scenario == "NAS-Bench-201", y2 := latency]
pareto_long[, header := paste0(niches, " ", scenario, " ", instance)]
pareto_long[, header := factor(header, levels = levels(factor(header))[c(9:12, 5:8, 1:4)])]

# Figure 13
g = ggplot(aes(x = y2, y = val_loss, colour = method), data = pareto_long[method %in% c("BOP-Elites*", "ParEGO*", "Random")]) +
  geom_point(alpha = 0.7) +
  geom_step(aes(linetype = method), direction = "hv", lwd = 1, alpha = 0.7) +
  labs(x = "Log(# Params) / Latency", y = "Validation Error", colour = "Optimizer", linetype = "Optimizer") +
  facet_wrap(~ header, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("plots/pareto.png", plot = g, device = "png", width = 12, height = 7)


# ert for mo/qdo to mo targets after half of optimization budget
# Table 7
comparisons = list(c(qdo_method = "BOP-ElitesHB", mo_method = "ParEGOHB"), c(qdo_method = "qdHB", mo_method = "moHB*"), c(qdo_method = "BOP-Elites*", mo_method = "ParEGO*"))
ert_comparisons = map_dtr(comparisons, function(comparison) {
  qdo_method = comparison["qdo_method"]
  mo_method = comparison["mo_method"]
  targets = setNames(best_sum_agg[type == "half" & method == mo_method][, c("problem", "mean_val")], c("problem", "target"))
  competitor = results_sum[method == qdo_method]
  competitor[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
  competitor = merge(competitor, targets, by = "problem")
  reached = competitor[, .(target_reached = (overall <= target)), by = .(problem, cumbudget, repl)]
  reached[target_reached == TRUE, rt := min(cumbudget), by = .(problem, cumbudget, repl)]
  ert = map_dtr(unique(reached$problem), function(problem_) {
    tmp = reached[problem == problem_]
    tmpx = tmp[, .(rt = min(rt, na.rm = TRUE)), by = .(repl)]
    tmpx[, successful := is.finite(rt)]
    tmpx[successful == FALSE, rt := max(tmp$cumbudget)]  # max budget if not successful but this doesn't happen anyhow
    data.table(ert = sum(tmpx$rt) / sum(tmpx$successful), problem = problem_, qdo_method = qdo_method, mo_method = mo_method)
  })
})
results_sum_agg[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
max_budget = results_sum_agg[, .(max_budget = max(cumbudget)), by = .(problem, scenario, instance, niches)][, c("problem", "max_budget", "scenario", "instance", "niches")]
ert_comparisons = merge(ert_comparisons, max_budget, by = "problem")
ert_comparisons[, ert_fraction := ert / max_budget]
ert_comparisons[, ert_fraction_scaled := 0.5 / ert_fraction]  # 0.5 is the ert for mo to reach the mo target after 0.5 of budget
ert_comparisons[, qdo_method := factor(qdo_method, levels = c("BOP-ElitesHB", "qdHB", "BOP-Elites*"))]
ert_comparisons_agg = ert_comparisons[, .(mean_ert = mean(ert_fraction_scaled), sd_ert = sd(ert_fraction_scaled), se_ert = sd(ert_fraction_scaled) / sqrt(.N)), by = .(qdo_method)]

print(xtable(dcast(ert_comparisons, problem ~ qdo_method, value.var = "ert_fraction_scaled")[c(3, 2, 1, 6, 5, 4, 9, 8, 7, 12, 11, 10)]), include.rownames = FALSE)

# Figure 9
# missing niches analysis
best[, missing := (val_loss == 100), by = .(niche, method, repl, scenario, instance, niches)]
best_missing = best[, .(mean_missing = mean(missing)), by = .(niche, method, scenario, instance, niches)]
best_missing$niche = factor(best_missing$niche, levels = paste0("niche", 1:10), labels = paste0("Niche ", 1:10))
best_missing[, header := paste0(niches, " ", scenario, " ", instance)]
best_missing[, header := factor(header, levels = levels(factor(header))[c(9:12, 5:8, 1:4)])]
best_missing[mean_missing == 0, mean_missing := NA_real_]
g = ggplot(aes(x = method, y = mean_missing, colour = niche, fill = niche), data = best_missing[niches != "Small"]) +  # small has 0
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Optimizer", y = "Rel. Freq. Niche Missed", colour = "", fill = "") +
  facet_wrap(~ header, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = rel(0.85), angle = 45, hjust = 1))
ggsave("plots/missing.png", plot = g, device = "png", width = 12, height = (9/3)*2)

# niche boundaries
library(bbotk)
source("niches_overlapping.R")

problems = list(nb101_small_nb, nb101_medium_nb, nb101_large_nb,
  nb201_cifar10_small_nb, nb201_cifar10_medium_nb, nb201_cifar10_large_nb,
  nb201_cifar100_small_nb, nb201_cifar100_medium_nb, nb201_cifar100_large_nb,
  nb201_imagenet_small_nb, nb201_imagenet_medium_nb, nb201_imagenet_large_nb
)

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
boundaries[, scenario := rep(c("NAS-Bench-101", "NAS-Bench-201"), c(30, 30 * 3))]
boundaries[, instance := rep(c("Cifar-10", "Cifar-10", "Cifar-100", "ImageNet16-120"), each = 30)]
boundaries[, niches := rep(rep(c("Small", "Medium", "Large"), each = 10), 4)]
boundaries[, problem := paste0(scenario, "_", instance, "_", niches)]
boundaries[, niche := factor(niche, levels = paste0("Niche ", 1:10))]

# Table 4
print(xtable(dcast(boundaries, problem ~ niche, value.var = "nbs")[c(3, 2, 1, 6, 5, 4, 9, 8, 7, 12, 11, 10)]), include.rownames = FALSE)

