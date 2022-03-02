library(data.table)
library(ggplot2)
library(pammtools)
library(mlr3misc)

results = readRDS("results.rds")
results = results[method != "smsego"]
results = results[overlapping == TRUE]
results$scenario = factor(results$scenario, labels = c("NAS-Bench-101", "NAS-Bench-201"))
results$instance = factor(results$instance, labels = c("Cifar-10", "Cifar-100", "ImageNet16-120"))
results$niches = factor(results$niches, labels = c("Small", "Medium", "Large"))
results$method = factor(results$method, levels = c("bohb_qdo", "hb_qdo", "bop", "bohb_mo", "hb_mo", "parego", "random_search"), labels = c("BOP-ElitesHB", "qdHB", "BOP-Elites*", "ParEGOHB", "moHB*", "ParEGO*", "Random"))

nb101 = results[scenario == "NAS-Bench-101"]
nb101_times = lapply(unique(nb101$instance), function(instance_) {
  setNames(
  lapply(unique(nb101$niches), function(niches_) {
    tmp = nb101[instance == instance_ & niches == niches_]
    quantile(unique(tmp$cumtime), seq(0.01, 1, length.out = 100))
  }), as.character(unique(nb101$niches)))
})
nb101_times = setNames(nb101_times, as.character(unique(nb101$instance)))

nb201 = results[scenario == "NAS-Bench-201"]
nb201_times = lapply(unique(nb201$instance), function(instance_) {
  setNames(
  lapply(unique(nb201$niches), function(niches_) {
    tmp = nb201[instance == instance_ & niches == niches_]
    quantile(unique(tmp$cumtime), seq(0.01, 1, length.out = 100))
  }), as.character(unique(nb201$niches)))
})
nb201_times = setNames(nb201_times, as.character(unique(nb201$instance)))

times = list("NAS-Bench-101" = nb101_times, "NAS-Bench-201" = nb201_times)

get_incumbent_cumtime = function(incumbent, scenario, instance, niches, cumtime) {
  times = times[[scenario]][[instance]][[niches]]
  map_dbl(times, function(time) {
    ind = which(cumtime <= time)
    if (length(ind) == 0L) {
      max(incumbent)
    } else {
      min(incumbent[ind])
    }
  })
}

results_time = results[, .(incumbent_time = get_incumbent_cumtime(incumbent, scenario, instance, niches, cumtime), time = times[[scenario]][[instance]][[niches]]), by = .(method, scenario, instance, niches, niche, repl)]
results_time_sum = results_time[, .(overall = sum(incumbent_time)), by = .(time, method, repl, scenario, instance, niches)]
results_time_agg = results_time_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(time, method, scenario, instance, niches)]

g = ggplot(aes(x = time, y = mean, colour = method, fill = method), data = results_time_agg[time > 1e+1]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Time (s)", y = "Average Validation Error Summed over Niches", colour = "Optimizer", fill = "Optimizer") +
  facet_wrap(~ niches + scenario + instance, scales = "free", ncol = 4L) +
  theme_minimal() +
  theme(legend.position = "bottom")
ggsave("anytime.png", plot = g, device = "png", width = 12, height = 9)

results_time_agg[, full_time := max(time), by = .(scenario, instance, niches)]
results_time_agg[, half_time := times[[scenario]][[instance]][[niches]][50L], by = .(scenario, instance, niches)]
results_time_agg[time == full_time, type := "full"]
results_time_agg[time == half_time, type := "half"]

best_sum_agg = rbind(results_time_agg[time == full_time], results_time_agg[time == half_time])
best_sum_agg[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]

library(scmamp)
tmp = - as.matrix(dcast(best_sum_agg[type == "half"], problem ~ method, value.var = "mean")[, -1])  # switch mean_test to mean_val to get final validation performance ranking
plotCD(tmp)


library(xtable)
bold = function(x) {paste("\\textbf{", x, "}", sep = "")}

aov_dat_half = copy(best_sum_agg[type == "half" & method %nin% c("qdHB", "moHB*")])
aov_dat_half[, multifidelity := as.factor(method %in% c("BOP-ElitesHB", "qdHB", "ParEGOHB", "moHB*"))]
aov_dat_half[, qdo := as.factor(method %in% c("BOP-ElitesHB", "qdHB", "BOP-Elites*"))]
aov_dat_half[, model_based:= as.factor(method %in% c("BOP-ElitesHB", "BOP-Elites*", "ParEGOHB", "ParEGO*"))]
aov_dat_half[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
aov_model_half = aov(mean ~ problem + multifidelity + qdo + model_based, data = aov_dat_half)
summary(aov_model_half)
print(xtable(summary(aov_model_half)), sanitize.colnames.function = bold, booktabs = TRUE)
TukeyHSD(aov_model_half, c("multifidelity", "qdo", "model_based"))
lm_half = lm(mean ~ problem + multifidelity + qdo + model_based, data = aov_dat_half)
qqnorm(lm_half$residuals)
qqline(lm_half$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))
shapiro.test(residuals(lm_half))

aov_dat_full = copy(best_sum_agg[type == "full" & method %nin% c("qdHB", "moHB*")])
aov_dat_full[, multifidelity := as.factor(method %in% c("BOP-ElitesHB", "qdHB", "ParEGOHB", "moHB*"))]
aov_dat_full[, qdo := as.factor(method %in% c("BOP-ElitesHB", "qdHB", "BOP-Elites*"))]
aov_dat_full[, model_based:= as.factor(method %in% c("BOP-ElitesHB", "BOP-Elites*", "ParEGOHB", "ParEGO*"))]
aov_dat_full[, problem := as.factor(paste0(scenario, "_", instance, "_", niches))]
aov_model_full = aov(mean ~ problem + multifidelity + qdo + model_based, data = aov_dat_full)
summary(aov_model_full)
print(xtable(summary(aov_model_full)), sanitize.colnames.function = bold, booktabs = TRUE)
TukeyHSD(aov_model_full, c("multifidelity", "qdo", "model_based"))
lm_full = lm(mean ~ problem + multifidelity + qdo + model_based, data = aov_dat_full)
qqnorm(lm_full$residuals)
qqline(lm_full$residuals, datax = FALSE, distribution = qnorm, probs = c(0.25, 0.75))
shapiro.test(residuals(lm_full))

