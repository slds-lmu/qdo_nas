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

source_files = map_chr(c("helpers.R", "scenarios.R", "niches.R", "optimizers.R"), function(x) file.path(benchmark_dir, x))
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

# setup scenarios and instances
nb101 = expand.grid(scenario = "nb101", instance = "cifar10", niches = c("small", "medium", "large"))
nb201 = expand.grid(scenario = "nb201", instance = c("cifar10", "cifar100", "imagenet"), niches = c("small", "medium", "large"))
setup = setDT(rbind(nb101, nb201))
setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", paste0(setup[i, ]$niches, collapse = "_"))
  addProblem(prob_id, data = list(scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, niches = setup[i, ]$niches))
  setNames(list(setup[i, ]), nm = prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add jobs for optimizers
optimizers = data.table(algorithm = c("bop", "parego", "smsego", "random_search", "bohb_qdo", "hb_qdo"))

for (i in seq_len(nrow(optimizers))) {
  algo_designs = setNames(list(optimizers[i, ]), nm = optimizers[i, ]$algorithm)

  probdesigns = if (optimizers[i, ]$algorithm %in% c("bohb_qdo", "hb_qdo")) {
    prob_designs[!grepl("nb201", x = names(prob_designs))]
  } else {
    prob_designs
  }
    
  ids = addExperiments(
    prob.designs = probdesigns,
    algo.designs = algo_designs,
    repls = 1L
  )
  addJobTags(ids, as.character(optimizers[i, ]$algorithm))
}

# walltime estimate: ~ 5000 for 100 repls (default)

jobs = findJobs()
resources.serial.default = list(memory = 16384L, walltime = 3600L * 24L, ntasks = 1L, ncpus = 1L, nodes = 1L)
submitJobs(jobs, resources = resources.serial.default)

done = findDone()
results = reduceResultsList(done, function(x, job) {
  tmp = x
  tmp[, method := job$pars$algo.pars$algorithm]
  tmp[, scenario := job$instance$scenario]
  tmp[, instance := job$instance$instance]
  tmp[, niches := job$instance$niches]
  tmp
})
results = rbindlist(results, fill = TRUE)
saveRDS(results, "results.rds")

