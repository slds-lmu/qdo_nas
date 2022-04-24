library(batchtools)
library(R6)
library(checkmate)
library(bbotk)
library(mlr3mbo)
library(mlr3misc)
library(data.table)
library(mlr3)
library(mlr3learners)
source("helpers_mobilenet.R")
source("scenarios_2F.R")
source("optimizers_2F.R")

packages = c("R6", "checkmate", "bbotk", "mlr3mbo", "mlr3misc", "data.table", "mlr3", "mlr3learners")

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

root = here::here()
benchmark_dir = file.path(root)

source_files = map_chr(c("helpers_mobilenet.R", "scenarios_2F.R", "optimizers_2F.R"), function(x) file.path(benchmark_dir, x))
for (sf in source_files) {
  source(sf)
}

#reg = makeExperimentRegistry(file.dir = "FIXME", packages = packages, source = source_files)
reg = makeExperimentRegistry(file.dir = NA, packages = packages, source = source_files)  # interactive session
saveRegistry(reg)
# reg = loadRegistry("registry_qdo_nas_ofa_2Fx_clean")  # to inspect the original registry on the cluster
# tab = getJobTable()

# add algorithms
addAlgorithm("bop", fun = bop_wrapper)
addAlgorithm("parego", fun = parego_wrapper)
addAlgorithm("random_search", fun = random_search_wrapper)

repls = 100

# setup scenarios and instances
ofa = expand.grid(scenario = "mobilenet", instance = "imagenet", niches = c("small", "medium", "large"), repls = repls)
setup = setDT(ofa)
setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", setup[i, ]$niches, collapse = "_")
  addProblem(prob_id, data = list(scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, niches = setup[i, ]$niches, repls = setup[i, ]$repls))
  setNames(list(setup[i, ]), nm = prob_id)
})
nn = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = nn

# add jobs for optimizers
optimizers = data.table(algorithm = c("bop", "parego", "random_search"))

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
resources.serial.default = list(walltime = 3600L * 24, memory = 4000, ntasks = 1L, ncpus = 1L, nodes = 1L)
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
saveRDS(results, "results/results_2Fx.rds")

tab = getJobTable()
as.numeric(sum(tab$time.running), units = "hours")  # 95.48457 CPUh

