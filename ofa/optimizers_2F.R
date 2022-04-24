bop_wrapper = function(job, data, instance, ...) {
  reticulate::use_python("FIXME_python_path_of_your_virtualenv", required = TRUE)
  reticulate::use_virtualenv("FIXME_python_path_of_your_virtualenv", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  random = import("random", convert = FALSE)
  numpy = import("numpy", convert = FALSE)

  random$seed(job$seed)
  numpy$random$seed(job$seed)

  py_run_file("prepare_mobilenet_R.py", convert = FALSE)

  source("niches_overlapping_2F.R")
  repls = instance$repls

  fullbudget = 1L  # FIXME:
  y_var = "val_loss"
  feature_vars = c("latency", "size")
  n_enc = 128L
  nb = switch(as.character(instance$niches), small = ofa_note10_small_nb, medium = ofa_note10_medium_nb, large = ofa_note10_large_nb)
  instance = make_mobilenet_qdo(nb = nb, n_evals = 100L)

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 3L)
  surrogate$y_cols = c(y_var, feature_vars)
  surrogate$x_cols = paste0("E", 1:n_enc)
  surrogate$archive = instance$archive
  
  ejie = AcqFunctionEJIE$new(surrogate, niches = nb, worst = 100)
  ejie$domain$trafo = function(x, param_set) {
    enc = as.integer(py$arch2feature(x$arch))
    as.data.table(setNames(as.list(enc), paste0("E", 1:n_enc)))
  }
  
  acq_optimizer = AcqOptimizer$new(OptimizerNASRandom$new(arch_encoder = py$arch_encoder), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = ejie

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    design = map_dtr(1:10, function(i) data.table(arch = {
      tmp = py$arch_encoder$random_sample()
      list(py_dict(names(tmp), tmp))
    }))
    instance$eval_batch(design)
    bayesopt_bop(instance, acq_function = ejie, acq_optimizer = acq_optimizer, sampler = NASSampler$new(arch_encoder = py$arch_encoder))
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, c(instance$archive$cols_y, instance$archive$cols_g), with = FALSE]))
    instance$archive$data[, epoch := fullbudget]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

parego_wrapper = function(job, data, instance, ...) {
  reticulate::use_python("FIXME_python_path_of_your_virtualenv", required = TRUE)
  reticulate::use_virtualenv("FIXME_python_path_of_your_virtualenv", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  random = import("random", convert = FALSE)
  numpy = import("numpy", convert = FALSE)

  random$seed(job$seed)
  numpy$random$seed(job$seed)

  py_run_file("prepare_mobilenet_R.py", convert = FALSE)

  source("niches_overlapping_2F.R")
  repls = instance$repls

  fullbudget = 1L  # FIXME:
  y_var = "val_loss"
  feature_vars = c("latency", "size")
  n_enc = 128L
  nb = switch(as.character(instance$niches), small = ofa_note10_small_nb, medium = ofa_note10_medium_nb, large = ofa_note10_large_nb)
  instance = make_mobilenet_moo(n_evals = 100L)

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 1L)
  surrogate$y_cols = "y_scal"
  surrogate$x_cols = paste0("E", 1:n_enc)
  surrogate$archive = instance$archive
  
  ei = AcqFunctionEI$new(surrogate)
  ei$domain$trafo = function(x, param_set) {
    enc = as.integer(py$arch2feature(x$arch))
    as.data.table(setNames(as.list(enc), paste0("E", 1:n_enc)))
  }
  
  # setting "y_scal" is needed for OptimizerNAS to know which column to select
  acq_optimizer = AcqOptimizer$new(OptimizerNASRandom$new(arch_encoder = py$arch_encoder), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = ei

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    design = map_dtr(1:10, function(i) data.table(arch = {
      tmp = py$arch_encoder$random_sample()
      list(py_dict(names(tmp), tmp))
    }))
    instance$eval_batch(design)
    bayesopt_parego(instance, acq_function = ei, acq_optimizer = acq_optimizer, sampler = NASSampler$new(arch_encoder = py$arch_encoder))
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, instance$archive$cols_y, with = FALSE]))
    instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, feature_vars, with = FALSE])]
    instance$archive$data[, epoch := fullbudget]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

random_search_wrapper = function(job, data, instance, ...) {
  reticulate::use_python("FIXME_python_path_of_your_virtualenv", required = TRUE)
  reticulate::use_virtualenv("FIXME_python_path_of_your_virtualenv", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  random = import("random", convert = FALSE)
  numpy = import("numpy", convert = FALSE)

  random$seed(job$seed)
  numpy$random$seed(job$seed)

  py_run_file("prepare_mobilenet_R.py", convert = FALSE)

  source("niches_overlapping_2F.R")
  repls = instance$repls

  fullbudget = 1L  # FIXME:
  y_var = "val_loss"
  feature_vars = c("latency", "size")
  n_enc = 128L
  nb = switch(as.character(instance$niches), small = ofa_note10_small_nb, medium = ofa_note10_medium_nb, large = ofa_note10_large_nb)
  instance = make_mobilenet_moo(n_evals = 100L)

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    points = map_dtr(seq_len(instance$terminator$param_set$values$n_evals), function(i) {
      data.table(arch = {
        tmp = py$arch_encoder$random_sample()
        list(py_dict(names(tmp), tmp))
      })
    })
    for (i in seq_len(NROW(points))) {
      instance$eval_batch(points[i, ])
    }
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, instance$archive$cols_y, with = FALSE]))
    instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, feature_vars, with = FALSE])]
    instance$archive$data[, epoch := fullbudget]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

