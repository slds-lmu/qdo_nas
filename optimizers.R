bop_wrapper = function(job, data, instance, ...) {
  #reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
  #reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
  reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
  reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")
  repls = instance$repls

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

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 2L)
  surrogate$y_cols = c(y_var, feature_var)
  surrogate$x_cols = paste0("P", 1:n_paths)
  surrogate$archive = instance$archive
  
  ejie = AcqFunctionEJIE$new(surrogate, niches = nb, worst = 100)
  ejie$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:n_paths)))
  }
  
  acq_optimizer = AcqOptimizer$new(OptimizerNAS$new(ss = ss), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = ejie

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    design = map_dtr(1:10, function(i) data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))))
    instance$eval_batch(design)
    bayesopt_bop(instance, acq_function = ejie, acq_optimizer = acq_optimizer, sampler = NASSampler$new(ss = ss))
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, c(instance$archive$cols_y, instance$archive$cols_g), with = FALSE]))
    instance$archive$data[, epoch := fullbudget]
    instance$archive$data[, runtime_fit := runtime]
    instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

parego_wrapper = function(job, data, instance, ...) {
  #reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
  #reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
  reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
  reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")
  repls = instance$repls

  if (scenario == "nb101") {
    ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)
    fullbudget = 108L
    y_var = "val_loss"
    feature_var = "num_params"
    n_paths = 40L
    nb = switch(as.character(instance$niches), "small" = nb101_small_nb, "medium" = nb101_medium_nb, "large" = nb101_large_nb)
    instance = make_nb101_moo(ss, n_evals = 100L)
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
    instance = make_nb201_moo(ss, n_evals = 100L)
  }

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 1L)
  surrogate$y_cols = "y_scal"
  surrogate$x_cols = paste0("P", 1:n_paths)
  surrogate$archive = instance$archive
  
  ei = AcqFunctionEI$new(surrogate)
  ei$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:n_paths)))
  }
  
  # setting "y_scal" is needed for OptimizerNAS to know which column to select
  acq_optimizer = AcqOptimizer$new(OptimizerNAS$new(ss = ss, y_col = "y_scal"), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = ei

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    design = map_dtr(1:10, function(i) data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))))
    instance$eval_batch(design)
    bayesopt_parego(instance, acq_function = ei, acq_optimizer = acq_optimizer, sampler = NASSampler$new(ss = ss))
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, instance$archive$cols_y, with = FALSE]))
    instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, feature_var, with = FALSE])]
    instance$archive$data[, epoch := fullbudget]
    instance$archive$data[, runtime_fit := runtime]
    instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

smsego_wrapper = function(job, data, instance, ...) {
  #reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
  #reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
  reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
  reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")
  repls = instance$repls

  if (scenario == "nb101") {
    ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)
    fullbudget = 108L
    y_var = "val_loss"
    feature_var = "num_params"
    n_paths = 40L
    nb = switch(as.character(instance$niches), "small" = nb101_small_nb, "medium" = nb101_medium_nb, "large" = nb101_large_nb)
    instance = make_nb101_moo(ss, n_evals = 100L)
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
    instance = make_nb201_moo(ss, n_evals = 100L)
  }

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner)
  surrogate$y_cols = c(y_var, feature_var)
  surrogate$x_cols = paste0("P", 1:n_paths)
  surrogate$archive = instance$archive
  
  sms = AcqFunctionSmsEgo$new(surrogate)
  sms$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:n_paths)))
  }
  
  acq_optimizer = AcqOptimizer$new(OptimizerNAS$new(ss = ss), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = sms

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    design = map_dtr(1:10, function(i) data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))))
    instance$eval_batch(design)
    bayesopt_smsego(instance, acq_function = sms, acq_optimizer = acq_optimizer, sampler = NASSampler$new(ss = ss))
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, instance$archive$cols_y, with = FALSE]))
    instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, feature_var, with = FALSE])]
    instance$archive$data[, epoch := fullbudget]
    instance$archive$data[, runtime_fit := runtime]
    instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

random_search_wrapper = function(job, data, instance, ...) {
  #reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
  #reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
  reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
  reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")
  repls = instance$repls

  if (scenario == "nb101") {
    ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)
    fullbudget = 108L
    y_var = "val_loss"
    feature_var = "num_params"
    n_paths = 40L
    nb = switch(as.character(instance$niches), "small" = nb101_small_nb, "medium" = nb101_medium_nb, "large" = nb101_large_nb)
    instance = make_nb101_moo(ss, n_evals = 100L)
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
    instance = make_nb201_moo(ss, n_evals = 100L)
  }

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    points = map_dtr(seq_len(instance$terminator$param_set$values$n_evals), function(i) {
      data.table(arch = list(ss$get_cell()$random_cell(ss$nasbench, "adj")))
    })
    for (i in seq_len(NROW(points))) {
      instance$eval_batch(points[i, ])
    }
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, instance$archive$cols_y, with = FALSE]))
    instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, feature_var, with = FALSE])]
    instance$archive$data[, epoch := fullbudget]
    instance$archive$data[, runtime_fit := runtime]
    instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

bohb_qdo_wrapper = function(job, data, instance, ...) {
  #reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
  #reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
  reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
  reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")
  repls = instance$repls

  if (scenario == "nb101") {
    ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)
    fullbudget = 108L
    y_var = "val_loss"
    feature_var = "num_params"
    n_paths = 40L
    nb = switch(as.character(instance$niches), "small" = nb101_small_nb, "medium" = nb101_medium_nb, "large" = nb101_large_nb)
    instance = make_nb101_qdo_mf(ss, nb = nb, maxbudget = 100L * fullbudget)
    eta = 3
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
    instance = make_nb201_qdo_mf(ss, nb = nb, maxbudget = 100L * fullbudget)
    eta = 3
  }

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 2L)
  surrogate$y_cols = c(y_var, feature_var)
  surrogate$x_cols = c(paste0("P", 1:n_paths), "epoch")
  surrogate$archive = instance$archive
  
  ejie = AcqFunctionEJIE$new(surrogate, niches = nb, worst = 100)
  ejie$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:n_paths)))
  }
  
  acq_optimizer = AcqOptimizer$new(OptimizerNAS$new(ss = ss), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = ejie

  optimizer = OptimizerBOHBQDO$new(niches = nb)
  optimizer$param_set$values$eta = eta
  optimizer$param_set$values$acq_function = ejie
  optimizer$param_set$values$acq_optimizer = acq_optimizer
  optimizer$param_set$values$sampler = NASSampler$new(ss = ss)
  optimizer$param_set$values$repeats = TRUE

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    optimizer$optimize(instance)
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, c(instance$archive$cols_y, instance$archive$cols_g), with = FALSE]))
    instance$archive$data[, runtime_fit := runtime]
    instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

hb_qdo_wrapper = function(job, data, instance, ...) {
  #reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
  #reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
  reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
  reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")
  repls = instance$repls

  if (scenario == "nb101") {
    ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)
    fullbudget = 108L
    y_var = "val_loss"
    feature_var = "num_params"
    n_paths = 40L
    nb = switch(as.character(instance$niches), "small" = nb101_small_nb, "medium" = nb101_medium_nb, "large" = nb101_large_nb)
    instance = make_nb101_qdo_mf(ss, nb = nb, maxbudget = 100L * fullbudget)
    eta = 3
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
    instance = make_nb201_qdo_mf(ss, nb = nb, maxbudget = 100L * fullbudget)
    eta = 3
  }

  optimizer = OptimizerHyperbandQDO$new(niches = nb)
  optimizer$param_set$values$eta = eta
  optimizer$param_set$values$sampler = NASSampler$new(ss = ss)
  optimizer$param_set$values$repeats = TRUE

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    optimizer$optimize(instance)
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, c(instance$archive$cols_y, instance$archive$cols_g), with = FALSE]))
    instance$archive$data[, runtime_fit := runtime]
    instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

bohb_mo_wrapper = function(job, data, instance, ...) {
  #reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
  #reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
  reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
  reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")
  repls = instance$repls

  if (scenario == "nb101") {
    ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)
    fullbudget = 108L
    y_var = "val_loss"
    feature_var = "num_params"
    n_paths = 40L
    nb = switch(as.character(instance$niches), "small" = nb101_small_nb, "medium" = nb101_medium_nb, "large" = nb101_large_nb)
    instance = make_nb101_moo_mf(ss, maxbudget = 100L * fullbudget)
    eta = 3
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
    instance = make_nb201_moo_mf(ss, maxbudget = 100L * fullbudget)
    eta = 3
  }

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 1L)
  surrogate$y_cols = "y_scal"
  surrogate$x_cols = c(paste0("P", 1:n_paths), "epoch")
  surrogate$archive = instance$archive

  ei = AcqFunctionEI$new(surrogate)
  ei$domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    as.data.table(setNames(as.list(path), paste0("P", 1:n_paths)))
  }

  # setting "y_scal" is needed for OptimizerNAS to know which column to select
  acq_optimizer = AcqOptimizer$new(OptimizerNAS$new(ss = ss, y_col = "y_scal"), terminator = trm("none"))  # OptimizerNAS terminates on its own
  acq_optimizer$acq_function = ei

  optimizer = OptimizerBOHBMO$new()
  optimizer$param_set$values$eta = eta
  optimizer$param_set$values$acq_function = ei
  optimizer$param_set$values$acq_optimizer = acq_optimizer
  optimizer$param_set$values$sampler = NASSampler$new(ss = ss)
  optimizer$param_set$values$repeats = TRUE

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    optimizer$optimize(instance)
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, instance$archive$cols_y, with = FALSE]))
    instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, feature_var, with = FALSE])]
    instance$archive$data[, runtime_fit := runtime]
    instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

hb_mo_wrapper = function(job, data, instance, ...) {
  #reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
  #reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
  reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
  reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
  library(reticulate)

  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  naszilla = import("naszilla", convert = FALSE)

  scenario = as.character(instance$scenario)
  dataset = as.character(instance$instance)
  if (instance$overlapping) source("niches_overlapping.R") else source("niches.R")
  repls = instance$repls

  if (scenario == "nb101") {
    ss = naszilla$nas_benchmarks$Nasbench101(mf = TRUE)
    fullbudget = 108L
    y_var = "val_loss"
    feature_var = "num_params"
    n_paths = 40L
    nb = switch(as.character(instance$niches), "small" = nb101_small_nb, "medium" = nb101_medium_nb, "large" = nb101_large_nb)
    instance = make_nb101_moo_mf(ss, maxbudget = 100L * fullbudget)
    eta = 3
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
    instance = make_nb201_moo_mf(ss, maxbudget = 100L * fullbudget)
    eta = 3
  }

  optimizer = OptimizerHyperbandMO$new()
  optimizer$param_set$values$eta = eta
  optimizer$param_set$values$sampler = NASSampler$new(ss = ss)
  optimizer$param_set$values$repeats = TRUE

  pareto = res = data = best = vector("list", length = repls)
  for (r in seq_len(repls)) {
    set.seed(r)
    instance$archive$clear()
    optimizer$optimize(instance)
    pareto[[r]] = emoa::nondominated_points(t(instance$archive$data[, instance$archive$cols_y, with = FALSE]))
    instance$archive$data[, niche := nb$get_niche_dt(instance$archive$data[, feature_var, with = FALSE])]
    instance$archive$data[, runtime_fit := runtime]
    instance$archive$data[, runtime := c(0, diff(timestamp)) + runtime]
    data[[r]] = instance$archive$data
    best[[r]] = instance$archive$best()
    tmp = cummin_per_niche(instance$archive, nb = nb, y_var = y_var)
    tmp[, repl := r]
    res[[r]] = tmp
  }
  list(res = res, pareto = pareto, data = data, best = best)
}

