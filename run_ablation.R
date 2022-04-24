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

packages = c("R6", "checkmate", "bbotk", "mlr3mbo", "mlr3misc", "data.table", "mlr3", "mlr3learners")

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

root = here::here()
benchmark_dir = file.path(root)

source_files = map_chr(c("helpers.R", "scenarios.R"), function(x) file.path(benchmark_dir, x))
for (sf in source_files) {
  source(sf)
}
source_files = c(source_files, file.path(benchmark_dir, "LearnerRegrBananasNN.R"))

#reg = makeExperimentRegistry(file.dir = "FIXME", packages = packages, source = source_files)
reg = makeExperimentRegistry(file.dir = NA, packages = packages, source = source_files)  # interactive session
saveRegistry(reg)
# reg = loadRegistry("registry_qdo_nas_ablation_clean")  # to inspect the original registry on the cluster
# tab = getJobTable()

eval_wrapper = function(job, data, instance, ...) {
  # loading reticulate etc. is done in LearnerRegrBananasNN.R

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)
  random = import("random", convert = FALSE)
  numpy = import("numpy", convert = FALSE)

  random$seed(job$seed)
  numpy$random$seed(job$seed)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")

  if (scenario == "nb101") {
    ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)
    fullbudget = 108L
    y_var = "val_loss"
    feature_var = "num_params"
    n_paths = 40L
    nb = switch(as.character(instance$niches), "small" = nb101_small_nb, "medium" = nb101_medium_nb, "large" = nb101_large_nb)
    instance = make_nb101_qdo(ss, nb = nb, n_evals = 100L)
  } else if (scenario == "nb201") {
    # cifar10, cifar100, ImageNet16-120
    ss = naszilla$nas_benchmarks$Nasbench201(dataset = dataset)
    fullbudget = 200L
    y_var = "val_loss"
    feature_var = "latency"
    n_paths = 30L
    nb = if (dataset == "cifar10") {
      switch(as.character(instance$niches), "small" = nb201_cifar10_small_nb, "medium" = nb201_cifar10_medium_nb, "large" = nb201_cifar10_large_nb)
    } else if (dataset == "cifar100") {
      switch(as.character(instance$niches), "small" = nb201_cifar100_small_nb, "medium" = nb201_cifar100_medium_nb, "large" = nb201_cifar100_large_nb)
    } else if (dataset == "ImageNet16-120") {
      switch(as.character(instance$niches), "small" = nb201_imagenet_small_nb, "medium" = nb201_imagenet_medium_nb, "large" = nb201_imagenet_large_nb)
    }
    instance = make_nb201_qdo(ss, nb = nb, n_evals = 100L)
  }

  xs = list(...)

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  surrogate = if (xs$surrogate == "ranger") {
    SurrogateLearners$new(list(ranger, ranger$clone(deep = TRUE)))
  } else {
    SurrogateLearners$new(list(ranger, LearnerRegrBananasNN$new()))
  }
  surrogate$y_cols = c(y_var, feature_var)
  surrogate$x_cols = paste0("P", 1:n_paths)
  surrogate$archive = instance$archive
  
  ejie = AcqFunctionEJIE$new(surrogate, niches = nb, worst = 100)
  ejie$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:n_paths)))
  }
  
  acq_optimizer = if (xs$acqopt == "random") {
    AcqOptimizer$new(OptimizerNASRandom$new(ss = ss), terminator = trm("none"))  # OptimizerNASRandom terminates on its own
  } else {
    AcqOptimizer$new(OptimizerNAS$new(ss = ss), terminator = trm("none"))  # OptimizerNAS terminates on its own
  }
  acq_optimizer$acq_function = ejie

  set.seed(job$seed)
  instance$archive$clear()
  design = map_dtr(1:10, function(i) data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))))
  instance$eval_batch(design)
  bayesopt_bop(instance, acq_function = ejie, acq_optimizer = acq_optimizer, sampler = NASSampler$new(ss = ss))
  pareto = emoa::nondominated_points(t(instance$archive$data[, c(instance$archive$cols_y, instance$archive$cols_g), with = FALSE]))
  instance$archive$data[, epoch := fullbudget]
  instance$archive$data[, runtime_fit := runtime]
  instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
  data = instance$archive$data
  best = instance$archive$best()
  res = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
  list(res = res, pareto = pareto, data = data, best = best)
}  

# add algorithms
addAlgorithm("eval", fun = eval_wrapper)

# setup scenarios and instances
nb101 = expand.grid(scenario = "nb101", instance = "cifar10", niches = "medium", overlapping = TRUE, repls = 1L)  # we now submit repls via batchtools
setup = setDT(nb101)
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
ablation = setDT(expand.grid(surrogate = c("ranger", "nn"), acqopt = c("random", "mutation")))

for (i in seq_len(nrow(ablation))) {
  algo_designs = setNames(list(ablation[i, ]), nm = "eval")

  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = algo_designs,
    repls = 30L
  )
  addJobTags(ids, as.character(paste0(ablation[i, ]$surrogate, "_", ablation[i, ]$acqopt)))
}

jobs = getJobTable()
jobs[, walltime := 6000L]
jobs[, memory := 8000L]
jobs[grepl("nn", tags), walltime := 3600L * 10L]
jobs[grepl("nn", tags), memory := 16000L]

resources.serial.default = list(ntasks = 1L, ncpus = 1L, nodes = 1L)
submitJobs(jobs[, c("job.id", "walltime", "memory")], resources = resources.serial.default)

done = findDone()
results = reduceResultsList(done, function(x, job) {
  tmp = x$res
  tmp[, surrogate := job$algo.pars$surrogate]
  tmp[, acqopt := job$algo.pars$acqopt]
  tmp[, repl := job$repl]
  tmp[, scenario := job$instance$scenario]
  tmp[, instance := job$instance$instance]
  tmp[, niches := job$instance$niches]
  tmp[, overlapping := job$instance$overlapping]
  tmp
})
results = rbindlist(results, fill = TRUE)
saveRDS(results, "results/results_ablation.rds")

tab = getJobTable()
as.numeric(sum(tab$time.running), units = "hours")  # 345.264 CPUh

