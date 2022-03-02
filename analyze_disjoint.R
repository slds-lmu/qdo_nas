library(data.table)
library(ggplot2)
library(pammtools)
library(mlr3misc)

# data
results = readRDS("results/results.rds")
results = results[method != "smsego"]
results = results[overlapping == FALSE]
results$scenario = factor(results$scenario, labels = c("NAS-Bench-101", "NAS-Bench-201"))
results$instance = factor(results$instance, labels = c("Cifar-10", "Cifar-100", "ImageNet16-120"))
results$niches = factor(results$niches, labels = c("Small", "Medium", "Large"))
results$method = factor(results$method, levels = c("bohb_qdo", "hb_qdo", "bop", "bohb_mo", "hb_mo", "parego", "random_search"), labels = c("BOP-ElitesHB", "qdHB", "BOP-Elites*", "ParEGOHB", "moHB*", "ParEGO*", "Random"))

best = readRDS("results/best.rds")
best = best[method != "smsego"]
best = best[overlapping == FALSE]
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
pareto = pareto[overlapping == FALSE]
pareto$scenario = factor(pareto$scenario, labels = c("NAS-Bench-101", "NAS-Bench-201"))
pareto$instance = factor(pareto$instance, labels = c("Cifar-10", "Cifar-100", "ImageNet16-120"))
pareto$niches = factor(pareto$niches, labels = c("Small", "Medium", "Large"))
pareto$method = factor(pareto$method, levels = c("bohb_qdo", "hb_qdo", "bop", "bohb_mo", "hb_mo", "parego", "random_search"), labels = c("BOP-ElitesHB", "qdHB", "BOP-Elites*", "ParEGOHB", "moHB*", "ParEGO*", "Random"))

# sum of validation error over niches
results_sum = results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl, scenario, instance, niches)]
results_sum_agg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(cumbudget, method, scenario, instance, niches)]
results_sum_agg[scenario == "NAS-Bench-101", init_budget := 10L * 108L]
results_sum_agg[scenario == "NAS-Bench-201", init_budget := 10L * 200L]
results_sum_agg[scenario == "NAS-Bench-101", max_budget := 100L * 108L]
results_sum_agg[scenario == "NAS-Bench-201", max_budget := 100L * 200L]

# anytime performance of summed validation error
g = ggplot(aes(x = cumbudget, y = mean, colour = method, fill = method), data = results_sum_agg[cumbudget >= init_budget & cumbudget <= max_budget]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Total Budget used (Epochs)", y = "Average Validation Error Summed over Niches", colour = "Optimizer", fill = "Optimizer") +
  facet_wrap(~ niches + scenario + instance, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("plots/anytime_disjoint.png", plot = g, device = "png", width = 12, height = 9)

# average best final test loss of the best final val loss architecture per niche
best_agg = best[, .(mean_test = mean(test_loss), se_test = sd(test_loss) / sqrt(.N), mean_val = mean(val_loss), se_val = sd(val_loss) / sqrt(.N)), by = .(scenario, instance, method, niches, niche, type)]
best_agg$niche = factor(best_agg$niche, levels = paste0("niche", 1:10), labels = paste0("Niche ", 1:10))

# switch mean_test and se_test for mean_val and test_val to get final validation performance
g = ggplot(aes(x = method, y = mean_test, colour = niche), data = best_agg[type == "full"]) +
  scale_y_log10() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_test - se_test, ymax = mean_test + se_test), width = 0.2) +
  labs(x = "Optimizer", y = "Average Test Error", colour = "") +
  facet_wrap(~ niches + scenario + instance, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = rel(0.5), angle = 90, hjust = 0))
ggsave("plots/best_test_disjoint.png", plot = g, device = "png", width = 12, height = 9)

# best summed validation and test performance over niches
best_sum = best[, .(overall_test = sum(test_loss), overall_val = sum(val_loss)), by = .(method, repl, scenario, instance, niches, type)]
best_sum_agg = best_sum[, .(mean_test = mean(overall_test), se_test = sd(overall_test) / sqrt(.N), mean_val = mean(overall_val), se_val = sd(overall_val) / sqrt(.N)), by = .(method, scenario, instance, niches, type)]
best_sum_agg[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]

# average rank based on the average final test loss summed over niches
methods = c("BOP-ElitesHB", "qdHB", "BOP-Elites*", "ParEGOHB", "moHB*", "ParEGO*", "Random")
ranks = map_dtr(unique(best_sum_agg$scenario), function(scenario_) {
  map_dtr(unique(best_sum_agg$instance), function(instance_) {
    map_dtr(unique(best_sum_agg$niches), function(niches_) {
      res = best_sum_agg[scenario == scenario_ & instance == instance_ & niches == niches_ & type == "full"]
      if (nrow(res) == 0L) {  # not all instances present for all scenarios
        return(data.table())
      }
      setorderv(res, "mean_val")  # switch mean_test to mean_val to get final validation performance ranking
      data.table(rank = match(methods, res$method), method = methods, scenario = scenario_, instance = instance_, niches = niches_)
    })
  })
})
ranks[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
ranks_agg = ranks[, .(mean = mean(rank), se = sd(rank) / sqrt(.N)), by = .(method)]

# CD plots
library(scmamp)
tmp = - as.matrix(dcast(best_sum_agg[type == "full"], problem ~ method, value.var = "mean_test")[, -1])  # switch mean_test to mean_val to get final validation performance ranking
png("plots/cd_test_disjoint.png", width = 6, height = 2, units = "in", res = 300, pointsize = 10)
plotCD(tmp)
dev.off()

# concensus rankings
library(relations)
all_ranks = map(unique(best_sum_agg$problem), function(problem_) {
  agg = best_sum_agg[type == "full" & problem == problem_]
  setorderv(agg, "mean_test")
  ranking(agg$method, domain = agg$optimizer, decreasing = FALSE, complete = TRUE)
})
all_ranks = relation_ensemble(list = all_ranks)
consensus = relation_consensus(all_ranks, method = "SD/L")
ids = tryCatch(relation_class_ids(consensus), error = function(ec) NA_integer_)

# anovas
library(xtable)
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

pareto[, hvi := get_hvi(pareto, scenario, instance, niches), by = .(method, scenario, instance, niches, repl)]
pareto_agg = pareto[, .(mean_hvi = mean(hvi), se_hvi = sd(hvi) / sqrt(.N)), by = .(method, scenario, instance, niches)]
pareto_agg[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
tmp = - as.matrix(dcast(pareto_agg, problem ~ method, value.var = "mean_hvi")[, -1])
png("plots/cd_hvi_disjoint.png", width = 6, height = 2, units = "in", res = 300, pointsize = 10)
plotCD(tmp)
dev.off()

pareto_ranks = map(unique(pareto_agg$problem), function(problem_) {
  agg = pareto_agg[problem == problem_]
  setorderv(agg, "mean_hvi")
  ranking(agg$method, domain = agg$optimizer, decreasing = FALSE, complete = TRUE)
})
pareto_ranks = relation_ensemble(list = pareto_ranks)
consensus = relation_consensus(pareto_ranks, method = "SD/L")
ids = tryCatch(relation_class_ids(consensus), error = function(ec) NA_integer_)

g = ggplot(aes(x = method, y = mean_hvi, colour = niches), data = pareto_agg) +
  scale_y_log10() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_hvi - se_hvi, ymax = mean_hvi + se_hvi), width = 0.2) +
  labs(x = "Optimizer", y = "Average HVI", colour = "Niches") +
  facet_wrap(~ scenario + instance, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(size = rel(0.5), angle = 90, hjust = 0))
ggsave("plots/hvi_disjoint.png", plot = g, device = "png", width = 12, height = 3)

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
g = ggplot(aes(x = y2, y = val_loss, colour = method), data = pareto_long[method %in% c("BOP-Elites*", "ParEGO*", "Random")]) +
  geom_point(alpha = 0.7) +
  geom_step(aes(linetype = method), direction = "hv", lwd = 1, alpha = 0.7) +
  labs(x = "Log(Number of Parameters) / Latency", y = "Validation Error", colour = "Optimizer", colour = "Optimizer", linetype = "Optimizer") +
  facet_wrap(~ niches + scenario + instance, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("plots/pareto_disjoint.png", plot = g, device = "png", width = 12, height = 9)


# ert for mo/qdo to mo targets after half of optimization budget
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

