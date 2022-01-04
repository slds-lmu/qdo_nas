devtools::load_all("bbotk")
devtools::load_all("mlr3mbo")
library(reticulate)
use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")

source("helpers.R")

# https://github.com/ntienvu/TW_NAS/blob/main/nas_bench/cell.py
naszilla = import("naszilla", convert = FALSE)
ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)

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
  instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, "num_params", with = FALSE])]
  instance$archive$data[, epoch := 108]

  tmp = cummin_per_niche(instance$archive, nb = nb, y_var = "val_loss")
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
  instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, "num_params", with = FALSE])]
  instance$archive$data[, epoch := 108]

  tmp = cummin_per_niche(instance$archive, nb = nb, y_var = "val_loss")
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
  instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, "num_params", with = FALSE])]
  instance$archive$data[, epoch := 108]

  tmp = cummin_per_niche(instance$archive, nb = nb, y_var = "val_loss")
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
  instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, "num_params", with = FALSE])]
  instance$archive$data[, epoch := 108]

  tmp = cummin_per_niche(instance$archive, nb = nb, y_var = "val_loss")
  tmp[, method := "random"]
  tmp
}

################################################################################################### BOHB

do_bohb_qdo = function(design) {

  domain = ps(arch = p_uty(), epoch = p_int(lower = 4L, upper = 108, tags = "budget"))
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:40)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:40)))), epoch = x$epoch)
  }

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(val_loss = py_to_r(cell$get_val_loss(ss$nasbench, epochs = x$epoch)), num_params = py_to_r(cell$get_num_params(ss$nasbench)), cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""))
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
    terminator = trm("budget", budget = 108*100),
    check_values = FALSE
  )

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

  optimizer = OptimizerBOHBQDO$new()
  optimizer$param_set$values$eta = 3
  optimizer$param_set$values$surrogate = surrogate
  optimizer$param_set$values$acq_function = ejie
  optimizer$param_set$values$acq_optimizer = acq_optimizer
  optimizer$param_set$values$sampler = list(sample = nas_sampler_hb)
  optimizer$param_set$values$repeats = TRUE

  optimizer$optimize(instance)
  tmp = cummin_per_niche(instance$archive, nb = nb)
  tmp[, method := "bohb"]
  tmp
}

################################################################################################### HB

do_hb_qdo = function(design) {

  domain = ps(arch = p_uty(), epoch = p_int(lower = 4L, upper = 108, tags = "budget"))
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:40)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:40)))), epoch = x$epoch)
  }

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(val_loss = py_to_r(cell$get_val_loss(ss$nasbench, epochs = x$epoch)), num_params = py_to_r(cell$get_num_params(ss$nasbench)), cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""))
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
    terminator = trm("budget", budget = 108*100),
    check_values = FALSE
  )

  optimizer = OptimizerHyperbandQDO$new()
  optimizer$param_set$values$eta = 3
  optimizer$param_set$values$sampler = list(sample = nas_sampler_hb)
  optimizer$param_set$values$repeats = TRUE

  optimizer$optimize(instance)
  tmp = cummin_per_niche(instance$archive, nb = nb)
  tmp[, method := "hb"]
  tmp
}

################################################################################################### run

results = map_dtr(1:100, function(r) {
  set.seed(r)
  design = map_dtr(1:10, function(i) data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))))
  bop = do_bop(design)
  parego = do_parego(design)
  smsego = do_smsego(design)
  random = do_random(design)
  bohb = do_bohb_qdo(design)
  hb = do_hb_qdo(design)

  tmp = rbind(bop, parego, smsego, random, bohb, hb, fill = TRUE)
  tmp[, repl := r]
  tmp
})

results[, cumbudget := cumbudget / 108]
results_agg = results[, .(mean = mean(incumbent), se = sd(incumbent) / sqrt(.N)), by = .(cumbudget, method, niche)]
results_sum = results[, .(overall = sum(incumbent)), by = .(cumbudget, method, repl)]
results_sum_agg = results_sum[, .(overall_mean = mean(overall), overall_se = sd(overall) / sqrt(.N)), by = .(cumbudget, method)]


library(ggplot2)
library(pammtools)
g = ggplot(aes(x = cumbudget, y = mean, colour = niche, fill = niche), data = results_agg) +
  geom_step() +
  geom_stepribbon(aes(ymin = mean - se, ymax = mean + se), colour = NA, alpha = 0.5) +
  facet_wrap(~ method) +
  xlim(c(0, 100 * 108))
g = ggplot(aes(x = cumbudget, y = overall_mean, colour = method, fill = method), data = results_sum_agg) +
  geom_step() +
  geom_stepribbon(aes(ymin = overall_mean - overall_se, ymax = overall_mean + overall_se), colour = NA, alpha = 0.5) +
  xlim(c(0, 100 * 108))
#ggsave("nb101.png", plot = g, width = 15, height = 5)

results[, max_cumbudget := max(cumbudget), by = .(method, repl)]
results_na = results[cumbudget == max_cumbudget, .(niche_missing = (incumbent == 100)), by = .(method, repl, niche)]

results_na_agg = results_na[, .(mean_missing = mean(niche_missing), se_missing = sd(niche_missing) / sqrt(.N)), by = .(method, niche)]

