library(R6)
library(checkmate)
library(paradox)
library(bbotk)
library(mlr3mbo)
library(mlr3misc)
library(data.table)
library(mlr3)
library(mlr3misc)
library(mlr3learners)
library(mlr3pipelines)

set.seed(2906)

reticulate::use_python("FIXME_python_path_of_your_virtualenv")
reticulate::use_virtualenv("FIXME_python_path_of_your_virtualenv")

source("helpers.R")
source("param_set.R")

# mobilenet_v2_torchhub: 2343480 params init
boundaries = 2343480 * c(0.4, 0.5, 0.6, 0.7)

niche1 = NicheBoundaries$new("niche1", niche_boundaries = list(params = c(boundaries[1L], boundaries[2L])))
niche2 = NicheBoundaries$new("niche2", niche_boundaries = list(params = c(boundaries[1L], boundaries[3L])))
niche3 = NicheBoundaries$new("niche3", niche_boundaries = list(params = c(boundaries[1L], boundaries[4L])))
nb = NichesBoundaries$new("nb_mobilenet_v2_torchhub", niches_boundaries = list(niche1 = niche1, niche2 = niche2, niche3 = niche3))

eval_wrapper = function(xss) {
  library(reticulate)
  py_run_file("prepare_R.py")
  
  # comment the lines below if you really want to run this on a cpu
  py$torch$cuda$is_available()
  py$torch$cuda$device(py$torch$cuda$current_device())
  py$torch$cuda$device_count()
  py$torch$cuda$get_device_name(py$torch$cuda$current_device())

  py$run_pruning_from_R(xss)
}

objective = ObjectiveRFunDt$new(
  fun = function(xdt) {
    tmp = map_dtr(seq_len(NROW(xdt)), function(i) {
      xss = as.list(xdt[i, ])
      xss$finetune_epochs = as.integer(xss$finetune_epochs)  # NOTE: manual fix due to hyperband loosing int representation
      xss$model_type = "mobilenet_v2_torchhub"
      xss = xss[!is.na(xss)]
      out = encapsulate("callr", .f = eval_wrapper, .args = list(xss = xss), .pkgs = "reticulate")
      results = out$result

      if (is.null(results)) {
        results = list(99, 0, 99, 0, 99, 2343480 + 1L)
      }
      names(results) = c("test_loss", "test_acc", "val_loss", "val_acc", "flops", "params")
      results$test_err = (1 - results$test_acc) * 100
      results$val_err = (1 - results$val_acc) * 100
      gc()
      as.data.table(results)
    })
    tmp[, niche := nb$get_niche_dt(tmp[, "params"])]
    tmp
  },
  domain = domain,
  codomain = ParamSet$new(list(
    ParamDbl$new("val_err", tags = "minimize"),
    ParamDbl$new("params", tags = "feature"),
    ParamUty$new("niche", tags = "niche"))
  ),
  properties = "single-crit",
  id = "prune_mobilenet_v2_torchhub",
  check_values = FALSE
)

instance = OptimInstanceQDOSingleCrit$new(
  objective = objective,
  search_space = search_space,
  terminator = trm("none"),
  check_values = FALSE
)

ranger = lrn("regr.ranger")
ranger$param_set$values$se.method = "jack"
learner = ranger
learner = GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>% po("imputeoor") %>>% learner)
surrogate = default_surrogate(instance, learner = learner, n_learner = 2L)
surrogate$y_cols = c("val_err", "params")
surrogate$x_cols = instance$archive$cols_x
surrogate$archive = instance$archive

ejie = AcqFunctionEJIE$new(surrogate, niches = nb, worst = 100)
  
acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 10000L), trm("evals", n_evals = 100000L))
acq_optimizer$acq_function = ejie

optimizer = OptimizerBOHBQDO$new(niches = nb)
optimizer$param_set$values$eta = 3
optimizer$param_set$values$acq_function = ejie
optimizer$param_set$values$acq_optimizer = acq_optimizer
optimizer$param_set$values$repeats = FALSE

optimizer$optimize(instance)
saveRDS(instance, "instance.rds")

