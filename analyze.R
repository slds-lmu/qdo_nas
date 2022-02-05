library(data.table)
library(ggplot2)
library(pammtools)
library(mlr3misc)

results = readRDS("results.rds")

results_nb101 = results[scenario == "nb101"]
nb101_niches = as.character(unique(results_nb101$niches))
nb101_times = setNames(map(nb101_niches, function(niche_) {
  tmp = results_nb101[niches == niche_]
  quantile(tmp$cumtime, seq(0, 1, len = 101L))
}), nm = nb101_niches)

results_nb201 = results[scenario == "nb201"]
nb201_niches = as.character(unique(results_nb201$niches))
nb201_times = setNames(map(nb201_niches, function(niche_) {
  tmp = results_nb201[niches == niche_]
  quantile(tmp$cumtime, seq(0, 1, len = 101L))
}), nm = nb201_niches)

get_incumbent_cumtime = function(incumbent, cumtime, niches, scenario) {
  times = switch(scenario, nb101 = nb101_times, nb201 = nb201_times)
  map_dbl(times[[as.character(niches)]], function(time_) {
    ind = which(cumtime <= time_)
    if (length(ind) == 0L) {
      100
    } else {
      min(incumbent[ind])
    }
  })
}

results_nb101_time = results_nb101[, .(incumbent_time = get_incumbent_cumtime(incumbent, cumtime, niches, "nb101"), time = nb101_times[[as.character(niches)]]), by = .(niche, method, repl, scenario, instance, niches)]
results_nb201_time = results_nb201[, .(incumbent_time = get_incumbent_cumtime(incumbent, cumtime, niches, "nb201"), time = nb201_times[[as.character(niches)]]), by = .(niche, method, repl, scenario, instance, niches)]

results_time = rbind(results_nb101_time, results_nb201_time)

results_sum = results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl, scenario, instance, niches)]
results_sum_agg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(cumbudget, method, scenario, instance, niches)]

results_sum = results_time[, .(overall = sum(incumbent_time)), by = .(time, method, repl, scenario, instance, niches)]
results_sum_agg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(time, method, scenario, instance, niches)]

# FIXME: cutoff values once max time used is reached
g = ggplot(aes(x = time, y = mean, colour = method, fill = method), data = results_sum_agg) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.5) +
  scale_y_log10() +
  facet_wrap(~ scenario + instance + niches, scales = "free")

results[, max_cumbudget := max(cumbudget), by = .(method, repl, scenario, instance, niches)]
results_na = results[cumbudget == max_cumbudget, .(niche_missing = (incumbent == 100)), by = .(method, repl, niche, scenario, instance, niches)]
results_na_agg = results_na[, .(mean_missing = mean(niche_missing), se_missing = sd(niche_missing) / sqrt(.N)), by = .(method, niche, scenario, instance, niches)]

g = ggplot(aes(x = method, y = mean_missing, fill = niche), data = results_na_agg) +
  geom_bar(stat = "identity") +
  facet_wrap(~ scenario + instance + niches, scales = "free")

### best
methods_ = c("bop", "parego", "smsego", "random_search", "bohb_qdo", "hb_qdo", "bohb_mo", "hb_mo")
results_sum = results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl, scenario, instance, niches)]
ranks_sum = map_dtr(unique(results_sum$scenario), function(scenario_) {
  map_dtr(unique(results_sum$instance), function(instance_) {
    map_dtr(unique(results_sum$niches), function(niches_) {
      map_dtr(unique(results_sum$repl), function(repl_) {
        tmp = results_sum[scenario == scenario_ & instance == instance_ & niches == niches_ & repl == repl_]
        if (nrow(tmp) == 0L) {
          return(NULL)
        }
        tmp = tmp[, .(best = min(overall)), by = .(method)]
        setorderv(tmp, "best")
        data.table(method = methods_, rank = match(methods_, tmp$method), repl = repl_, niches = niches_, instance = instance_, scenario = scenario_)
      })
    })
  })
})
ranks_sum_agg = ranks_sum[, .(mean = mean(rank), se = sd(rank) / sqrt(.N)), by = .(method, scenario, instance)]

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
