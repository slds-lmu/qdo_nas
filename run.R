library(batchtools)
library(R6)
library(checkmate)
library(bbotk)
library(mlr3mbo)
library(reticulate)
library(mlr3misc)
library(data.table)
library(mlr3)
library(mlr3learners)
source("helpers.R")
source("scenarios.R")
source("optimizers.R")

packages = c("bbotk", "mlr3mbo", "mlr3", "mlr3learners")

RhpcBLASctl::blas_set_num_threads(1L)
RhpcBLASctl::omp_set_num_threads(1L)

root = here::here()
benchmark_dir = file.path(root)

source_files = map_chr(c("helpers.R", "scenarios.R", "optimizers.R"), function(x) file.path(benchmark_dir, x))
for (sf in source_files) {
  source(sf)
}

reg = makeExperimentRegistry(file.dir = "/gscratch/lschnei8/registry_qdo_nas", packages = packages, source = source_files)
# reg = makeExperimentRegistry(file.dir = NA, packages = packages, source = source_files)
saveRegistry(reg)

# add algorithms
addAlgorithm("bop", fun = bop_wrapper)
addAlgorithm("random_search", fun = random_search_wrapper)
addAlgorithm("parego", fun = parego_wrapper)
addAlgorithm("smsego", fun = smsego_wrapper)
addAlgorithm("random_search", fun = random_search_wrapper)
addAlgorithm("bohb_qdo", fun = bohb_qdo_wrapper)
addAlgorithm("hb_qdo", fun = hb_qdo_wrapper)

# setup scenarios and instances
nb
setup = setDT(rbind(data.table(scenario = "nb101", instance = "cifar10"), expand.grid(scenario = "nb201", instance = c("cifar10", "cifar100", "imagenet"))))
setup[, id := seq_len(.N)]

