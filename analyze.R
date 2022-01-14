library(data.table)
library(ggplot2)
library(pammtools)

results = readRDS("results.rds")

results_agg = results[, .(mean = mean(incumbent), se = sd(incumbent) / sqrt(.N)), by = .(niche, cumbudget, method, scenario, instance, niches)]
results_sum = results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl, scenario, instance, niches)]
results_sum_agg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(cumbudget, method, scenario, instance, niches)]

g = ggplot(aes(x = cumbudget, y = mean, colour = method, fill = method), data = results_sum_agg) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.5) +
  scale_y_log10() +
  facet_wrap(~ scenario + instance + niches, scales = "free")

results[, max_cumbudget := max(cumbudget), by = .(method, repl, scenario, instance, niches)]
results_na = results[cumbudget == max_cumbudget, .(niche_missing = (incumbent == 100)), by = .(method, repl, niche, scenario, instance, niches)]
results_na_agg = results_na[, .(mean_missing = mean(niche_missing), se_missing = sd(niche_missing) / sqrt(.N)), by = .(method, niche, scenario, instance, niches)]

ggplot(aes(x = method, y = mean_missing, fill = niche), data = results_na_agg) +
  geom_bar(stat = "identity") +
  facet_wrap(~ scenario + instance + niches, scales = "free")

pareto = readRDS("pareto.rds")
x = pareto[method == "parego" & scenario == "nb101" & instance == "cifar10" & niches == "small"]
y = pareto[method == "smsego" & scenario == "nb101" & instance == "cifar10" & niches == "small"]
z = pareto[method == "random_search" & scenario == "nb101" & instance == "cifar10" & niches == "small"]
plot(x$pareto[[1L]][1L, ], x$pareto[[1L]][2L, ], col = "red", pch = 19)
points(y$pareto[[1L]][1L, ], y$pareto[[1L]][2L, ], col = "green", pch = 19)
points(z$pareto[[1L]][1L, ], z$pareto[[1L]][2L, ], col = "grey", pch = 19)

ref = t(c(100, 20))
emoa::dominated_hypervolume(x$pareto[[1L]], ref)
emoa::dominated_hypervolume(y$pareto[[1L]], ref)
emoa::dominated_hypervolume(z$pareto[[1L]], ref)
