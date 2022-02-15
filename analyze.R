library(data.table)
library(ggplot2)
library(pammtools)
library(mlr3misc)

results = readRDS("results.rds")
results = results[overlapping == TRUE]

best = readRDS("best.rds")
best = best[overlapping == TRUE]

results_nb101 = results[scenario == "nb101"]
nb101_instances = as.character(unique(results_nb101$instance))
nb101_niches = as.character(unique(results_nb101$niches))
nb101_times = setNames(map(nb101_instances, function(instance_) {
  setNames(map(nb101_niches, function(niches_) {
    tmp = results_nb101[instance == instance_ & niches == niches_]
    quantile(tmp$cumtime, seq(0, 1, len = 101L))
  }), nm = nb101_niches)
}), nm = nb101_instances)

results_nb201 = results[scenario == "nb201"]
nb201_instances = as.character(unique(results_nb201$instance))
nb201_niches = as.character(unique(results_nb201$niches))
nb201_times = setNames(map(nb201_instances, function(instance_) {
  setNames(map(nb201_niches, function(niches_) {
    tmp = results_nb201[instance == instance_ & niches == niches_]
    quantile(tmp$cumtime, seq(0, 1, len = 101L))
  }), nm = nb201_niches)
}), nm = nb201_instances)

get_incumbent_cumtime = function(incumbent, cumtime, niches, instance, scenario) {
  times = switch(scenario, nb101 = nb101_times, nb201 = nb201_times)
  map_dbl(times[[as.character(instance)]][[as.character(niches)]], function(time_) {
    ind = which(cumtime <= time_)
    if (length(ind) == 0L) {
      100
    } else {
      min(incumbent[ind])
    }
  })
}

results_nb101_time = results_nb101[, .(incumbent_time = get_incumbent_cumtime(incumbent, cumtime, niches, instance, "nb101"), time = nb101_times[[as.character(instance)]][[as.character(niches)]], time_ind = 1:101), by = .(niche, method, repl, scenario, instance, niches)]
results_nb201_time = results_nb201[, .(incumbent_time = get_incumbent_cumtime(incumbent, cumtime, niches, instance, "nb201"), time = nb201_times[[as.character(instance)]][[as.character(niches)]], time_ind = 1:101), by = .(niche, method, repl, scenario, instance, niches)]
results_time = rbind(results_nb101_time, results_nb201_time)

results_sum = L = L, L  # MiB[1]iobject.size(results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl, scenario, instance, niches)]) / 1000000
results_sum_agqg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(cumbudget, method, scenario, instance, niches)]

results_sum = results_time[, .(overall = sum(incumbent_time)), by = .(time, time_ind, method, repl, scenario, instance, niches)]
results_sum_agg = results_sum[, .(mean = mean(overall), se = sd(overall) / sqrt(.N)), by = .(time, time_ind, method, scenario, instance, niches)]

# FIXME: cutoff values once max time used is reached
g = ggplot(aes(x = time, y = mean, colour = method, fill = method), data = results_sum_agg[time_ind >= 90]) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.5) +
  scale_y_log10() +
  labs(x = "Budget", y = "Mean Validation Error Summed over Niches", colour = "Optimizer", fill = "Optimizer") +
  facet_wrap(~ scenario + instance + niches, scales = "free", ncol = 3L)

results[, max_cumbudget := max(cumbudget), by = .(method, repl, scenario, instance, niches)]
results_na = results[cumbudget == max_cumbudget, .(niche_missing = (incumbent == 100)), by = .(method, repl, niche, scenario, instance, niches)]
results_na_agg = results_na[, .(mean_missing = mean(niche_missing), se_missing = sd(niche_missing) / sqrt(.N)), by = .(method, niche, scenario, instance, niches)]

g = ggplot(aes(x = method, y = mean_missing, fill = niche), data = results_na_agg) +
  geom_bar(stat = "identity") +
  facet_wrap(~ scenario + instance + niches, scales = "free")

### average best final test loss of the best final val loss architecture per niche
best_agg = best[, .(mean_test = mean(test_loss), se_test = sd(test_loss) / sqrt(.N), mean_val = mean(val_loss), se_val = sd(val_loss) / sqrt(.N)), by = .(scenario, instance, method, niches, niche)]

g = ggplot(aes(x = method, y = mean_test, colour = niche), data = best_agg) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_test - se_test, ymax = mean_test + se_test), width = 0.2) +
  facet_wrap(~ scenario + instance + niches, scales 
# summed over1n
             ram__model = learner$modeliches

             # FIXME:
best_sum = best[, .(overall_test = sum(test_loss), overall_val = sum(val_loss)), by = .(method, repl, scenario, instance, niches)]
best_sum_agg = best_sum[, .(mean_test = meanL(overall_test), se_test = sd(overall_test) / sqrt(.N), mean_val m2000

                            chunksize = 100Loverall_val), se_val = sd(overall_val) / sqrt(.N)), by = .(scenario, instance, method, niches)]

tmp1 = melt(best_sum_agg, id.vars = c("scenario", "instance", "method", "niches"), measure.vars = c("mean_test", "mean_val"), variable.name = "split", value.name = "mean_loss")
levels(tmp1$split) = c("test", "val")
tmp2 = melt(best_sum_agg, id.vars = c("scenario", "instance", "method", "niches"), measure.vars = c("se_test", "se_val"), variable.name = "split", value.name = "se_loss")
levels(tmp2$split) = c("test", "val")
best_sum_agg = merge(tmp1, tmp2)

g = ggplot(aes(x = split, y = mean_loss, colour = method), data = best_sum_agg) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_loss - se_loss, ymax = mean_loss + se_loss), width = 0.05) +
  labs(x = "Optimizer", y = "Mean Final Validation/Test Error Summed over Niches", colour = "Split") +
  facet_wrap(~ scenario + instance + niches, scales = "free")


methods = c("bohb_qdo", "hb_qdo", "bop", "bohb_mo", "hb_mo", "parego", "smsego", "random_search")
# average rank based on the average final test loss summed over niches
ranks = map_dtr(unique(best$scenario), function(scenario_) {
  map_dtr(unique(best$instance), function(instance_) {
    map_dtr(unique(best$niches), function(niches_) {
      res = best_sum_agg[scenario == scenario_ & instance == instance_ & niches == niches_ & split == "test"]
      if (nrow(res) == 0L) {  # not all instances present for all scenarios
        return(data.table())
      }
      setorderv(res, "mean_loss")
      gc()e(rank = match(methods, res$method), method = methods, scenario = scenario_, instance = instance_, niches = niches_)
    })
  })
})

ranks_agg = ranks[, .(mean = round(mean(rank), 2), se = round(sd(rank) / sqrt(.N), 2)), by = .(method)]

