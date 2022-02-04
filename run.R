library(batchtools)
library(R6)
library(checkmate)
library(bbotk)
library(mlr3mbo)
library(mlr3misc)
library(data.table)
library(mlr3)
library(mlr3learners)
source("helpers.R")
source("scenarios.R")
source("niches.R")
source("optimizers.R")

packages = c("R6", "checkmate", "bbotk", "mlr3mbo", "mlr3misc", "data.table", "mlr3", "mlr3learners")

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

root = here::here()
benchmark_dir = file.path(root)

source_files = map_chr(c("helpers.R", "scenarios.R", "optimizers.R"), function(x) file.path(benchmark_dir, x))
for (sf in source_files) {
  source(sf)
}

reg = makeExperimentRegistry(file.dir = "/dss/dssfs02/lwp-dss-0001/pr74ze/pr74ze-dss-0000/ru84tad2/registry_qdo_nas", packages = packages, source = source_files)
# reg = makeExperimentRegistry(file.dir = NA, packages = packages, source = source_files)
saveRegistry(reg)

# add algorithms
addAlgorithm("bop", fun = bop_wrapper)
addAlgorithm("parego", fun = parego_wrapper)
addAlgorithm("smsego", fun = smsego_wrapper)
addAlgorithm("random_search", fun = random_search_wrapper)
addAlgorithm("bohb_qdo", fun = bohb_qdo_wrapper)
addAlgorithm("hb_qdo", fun = hb_qdo_wrapper)
addAlgorithm("bohb_mo", fun = bohb_mo_wrapper)
addAlgorithm("hb_mo", fun = hb_mo_wrapper)

repls = 100L

# setup scenarios and instances
nb101 = expand.grid(scenario = "nb101", instance = "cifar10", niches = c("small", "medium", "large"), overlapping = c(TRUE, FALSE), repls = repls)
nb201 = expand.grid(scenario = "nb201", instance = c("cifar10", "cifar100", "ImageNet16-120"), niches = c("small", "medium", "large"), overlapping = c(TRUE, FALSE), repls = repls)
setup = setDT(rbind(nb101, nb201))
setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", setup[i, ]$niches, "_", setup[i, ]$overlapping, collapse = "_")
  addProblem(prob_id, data = list(scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, niches = setup[i, ]$niches, overlapping = setup[i, ]$overlapping, repls = setup[i, ]$repls))
  setNames(list(setup[i, ]), nm = prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add jobs for optimizers
optimizers = data.table(algorithm = c("bop", "parego", "smsego", "random_search", "bohb_qdo", "hb_qdo", "bohb_mo", "hb_mo"))

for (i in seq_len(nrow(optimizers))) {
  algo_designs = setNames(list(optimizers[i, ]), nm = optimizers[i, ]$algorithm)

  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = algo_designs,
    repls = 1L  # actual repls are handled within
  )
  addJobTags(ids, as.character(optimizers[i, ]$algorithm))
}

# walltime estimate: ~ 5000 for 100 repls (default)
jobs = findJobs()
resources.serial.default = list(walltime = 3600L * 24L, memory = 16000, ntasks = 1L, ncpus = 1L, nodes = 1L)
submitJobs(jobs, resources = resources.serial.default)

done = findDone()
results = reduceResultsList(done, function(x, job) {
  tmp = rbindlist(x$res)
  tmp[, method := job$pars$algo.pars$algorithm]
  tmp[, scenario := job$instance$scenario]
  tmp[, instance := job$instance$instance]
  tmp[, niches := job$instance$niches]
  tmp
})
results = rbindlist(results, fill = TRUE)
saveRDS(results, "results.rds")

pareto = reduceResultsList(done, function(x, job) {
  tmp = emoa::nondominated_points(do.call(cbind, x$pareto))
  data.table(pareto = list(tmp), method = job$pars$algo.pars$algorithm, scenario = job$instance$scenario, instance = job$instance$instance, niches = job$instance$niches)
})
pareto = rbindlist(pareto, fill = TRUE)
saveRDS(pareto, "pareto.rds")

best = reduceResultsList(done, function(x, job) {
  tmp = rbindlist(x$best)
  tmp[, method := job$pars$algo.pars$algorithm]
  tmp[, scenario := job$instance$scenario]
  tmp[, instance := job$instance$instance]
  tmp[, niches := job$instance$niches]
  tmp
})
best = rbindlist(best, fill = TRUE)
saveRDS(best, "best.rds")

