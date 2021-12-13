devtools::load_all("bbotk")
devtools::load_all("mlr3mbo")
library(reticulate)
use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")

source("helpers.R")

# https://github.com/ntienvu/TW_NAS/blob/main/nas_bench/cell.py
naszilla = import("naszilla", convert = FALSE)
ss = naszilla$nas_benchmarks$Nasbench101()

# FIXME: changed from overlapping
nb1 = NicheBoundaries$new("niche1", niche_boundaries = list(num_params = c(0, 882570)))
nb2 = NicheBoundaries$new("niche2", niche_boundaries = list(num_params = c(882570, 1756298)))
nb3 = NicheBoundaries$new("niche3", niche_boundaries = list(num_params = c(1756298, 2793866)))
nb4 = NicheBoundaries$new("niche4", niche_boundaries = list(num_params = c(2793866, 10000000)))
nb5 = NicheBoundaries$new("niche5", niche_boundaries = list(num_params = c(10000000, Inf)))
nb = NichesBoundaries$new("test", niches_boundaries = list(niche1 = nb1, niche2 = nb2, niche3 = nb3, niche4 = nb4, niche5 = nb5))

################################################################################################### BOP

do_bop = function(design) {

  domain = ps(arch = p_uty())
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:40)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:40)))))
  }
  
  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(val_loss = py_to_r(cell$get_val_loss(ss$nasbench)), num_params = py_to_r(cell$get_num_params(ss$nasbench)), cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""))
      }))
      tmp[, niche := nb$get_niche_dt(tmp[, "num_params"])]
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("num_params", tags = "feature"),
      ParamFct$new("niche", levels = c("niche1", "niche2", "niche3", "niche4", "niche5"), special_vals = list(NA_character_), tags = "niche"))
    ),
    properties = "single-crit",
    id = "test",
    check_values = FALSE
  )
  
  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = 100L),
    check_values = FALSE
  )
  
  instance$eval_batch(design)
  
  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 2L)
  surrogate$y_cols = c("val_loss", "num_params")
  surrogate$x_cols = paste0("P", 1:40)
  surrogate$archive = instance$archive
  
  ejie = AcqFunctionEJIE$new(surrogate, niches = nb)
  ejie$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:40)))
  }
  
  acq_optimizer = AcqOptimizer$new(OptimizerNAS$new(), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = ejie

  bayesopt_bop(instance, acq_function = ejie, acq_optimizer = acq_optimizer, sampler = nas_sampler)
  tmp = instance$archive$best()
  tmp[, method := "bop"]
  tmp
}

################################################################################################### ParEGO

do_parego = function(design) {

  domain = ps(arch = p_uty())
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:40)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:40)))))
  }
  
  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(val_loss = py_to_r(cell$get_val_loss(ss$nasbench)), num_params = py_to_r(cell$get_num_params(ss$nasbench)), cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""))
      }))
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("num_params", tags = "minimize"))
    ),
    properties = "multi-crit",
    id = "test",
    check_values = FALSE
  )
  
  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = 100L),
    check_values = FALSE
  )

  instance$eval_batch(design)
  
  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 1L)
  surrogate$y_cols = "y_scal"
  surrogate$x_cols = paste0("P", 1:40)
  surrogate$archive = instance$archive
  
  ei = AcqFunctionEI$new(surrogate)
  ei$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:40)))
  }
  
  # setting "y_scal" is needed for OptimizerNAS to know which column to select
  acq_optimizer = AcqOptimizer$new(OptimizerNAS$new(y_col = "y_scal"), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = ei
  
  bayesopt_parego(instance, acq_function = ei, acq_optimizer = acq_optimizer)  # FIXME: also needs custom sampler
  tmp = instance$archive$best()
  tmp[, method := "parego"]
  tmp
}

################################################################################################### SmsEGO

do_smsego = function(design) {

  domain = ps(arch = p_uty())
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:40)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:40)))))
  }
  
  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(val_loss = py_to_r(cell$get_val_loss(ss$nasbench)), num_params = py_to_r(cell$get_num_params(ss$nasbench)), cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""))
      }))
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("num_params", tags = "minimize"))
    ),
    properties = "multi-crit",
    id = "test",
    check_values = FALSE
  )
  
  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = 100L),
    check_values = FALSE
  )
  
  instance$eval_batch(design)
  
  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner)
  surrogate$y_cols = c("val_loss", "num_params")
  surrogate$x_cols = paste0("P", 1:40)
  surrogate$archive = instance$archive
  
  sms = AcqFunctionSmsEgo$new(surrogate)
  sms$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:40)))
  }
  
  # seeting "y_scal" is needed for OptimizerNAS to know which column to select
  acq_optimizer = AcqOptimizer$new(OptimizerNAS$new(), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = sms
  
  bayesopt_smsego(instance, acq_function = sms, acq_optimizer = acq_optimizer)  # FIXME: also needs custom sampler
  tmp = instance$archive$best()
  tmp[, method := "smsego"]
  tmp
}

################################################################################################### random

do_random = function(design) {

  domain = ps(arch = p_uty())
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:40)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:40)))))
  }
  
  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(val_loss = py_to_r(cell$get_val_loss(ss$nasbench)), num_params = py_to_r(cell$get_num_params(ss$nasbench)), cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""))
      }))
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("num_params", tags = "minimize"))
    ),
    properties = "multi-crit",
    id = "test",
    check_values = FALSE
  )
  
  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = 100L),
    check_values = FALSE
  )
  
  instance$eval_batch(design)

  points = map_dtr(seq_len((100L - NROW(design))), function(i) data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))))
  for (i in seq_len(NROW(points))) {
    instance$eval_batch(points[i, ])
  }
  tmp = instance$archive$best()
  tmp[, method := "random"]
  tmp
}

results = map_dtr(1:100, function(r) {
  set.seed(r)
  design = map_dtr(1:10, function(i) data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))))
  bop = do_bop(design)
  parego = do_parego(design)
  smsego = do_smsego(design)
  random = do_random(design)
  tmp = rbind(bop, parego, smsego, random, fill = TRUE)
  tmp[, repl := r]
  tmp
})

results_add = map_dtr(1:100, function(r) {
  set.seed(r)
  design = map_dtr(1:10, function(i) data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))))
  tmp = do_bop(design)
  tmp[, repl := r]
  tmp[, method := "bop_noverlap"]
  tmp
})

results[, dominated := bbotk::is_dominated(rbind(val_loss, num_params)), by = .(method)]
results_clean = results[dominated == FALSE, ]

library(ggplot2)
g = ggplot(aes(x = num_params, y = val_loss, colour = method), data = results_clean) +
  geom_point(alpha = 0.5, size = 3) +
  geom_step(alpha = 1) +
  geom_vline(xintercept = c(map_dbl(nb$niches, function(n) n$niche_boundaries$num_params[2L])[-5L]))
ggsave("nb101.png", plot = g, width = 15, height = 5)

results[num_params < nb1$niche_boundaries$num_params[2L],  best_n1 := min(val_loss), by = .(method, repl)]
results[num_params < nb2$niche_boundaries$num_params[2L],  best_n2 := min(val_loss), by = .(method, repl)]
results[num_params < nb3$niche_boundaries$num_params[2L],  best_n3 := min(val_loss), by = .(method, repl)]
results[num_params < nb4$niche_boundaries$num_params[2L],  best_n4 := min(val_loss), by = .(method, repl)]
results[num_params < nb5$niche_boundaries$num_params[2L],  best_n5 := min(val_loss), by = .(method, repl)]

agg = results[, .(niche1 = mean(best_n1, na.rm = TRUE), niche2 = mean(best_n2, na.rm = TRUE), niche3 = mean(best_n3, na.rm = TRUE), niche4 = mean(best_n4, na.rm = TRUE), niche5 = mean(best_n5, na.rm = TRUE)), by = .(method)]

results_na = results[, .(n1_missing = all(is.na(best_n1)), n2_missing = all(is.na(best_n2)), n3_missing = all(is.na(best_n3)), n4_missing = all(is.na(best_n4)), n5_missing = all(is.na(best_n5))), by = .(method, repl)]

results_na_agg = results_na[, .(mean_n1_missing = mean(n1_missing), mean_n2_missing = mean(n2_missing), mean_n3_missing = mean(n3_missing), mean_n4_missing = mean(n4_missing), mean_n5_missing = mean(n5_missing)), by = .(method)]

