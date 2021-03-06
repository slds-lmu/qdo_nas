make_nb101_moo = function(ss, n_evals = 100L) {
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
        data.table(
          val_loss = py_to_r(cell$get_val_loss(ss$nasbench)),
          num_params = py_to_r(cell$get_num_params(ss$nasbench)),
          cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""),
          test_loss = py_to_r(cell$get_test_loss(ss$nasbench)),
          runtime = py_to_r(cell$get_runtime(ss$nasbench))
        )
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
    id = "nb101_moo",
    check_values = FALSE
  )

  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = n_evals),
    check_values = FALSE
  )

  instance
}

make_nb101_moo_mf = function(ss, maxbudget = 100L * 108L) {
  domain = ps(arch = p_uty(), epoch = p_int(lower = 4L, upper = 108L, tags = "budget"))
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
        data.table(
          val_loss = py_to_r(cell$get_val_loss(ss$nasbench, epochs = as.integer(x$epoch))),
          num_params = py_to_r(cell$get_num_params(ss$nasbench)),
          cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""),
          test_loss = py_to_r(cell$get_test_loss(ss$nasbench)),
          runtime = py_to_r(cell$get_runtime(ss$nasbench, epochs = as.integer(x$epoch)))
        ) 
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
    id = "nb101_moo_mf",
    check_values = FALSE
  )

  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("budget", budget = maxbudget),
    check_values = FALSE
  )

  instance
}

make_nb101_qdo = function(ss, nb, n_evals = 100L) {
  niches = names(nb$niches)

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
        data.table(
          val_loss = py_to_r(cell$get_val_loss(ss$nasbench)),
          num_params = py_to_r(cell$get_num_params(ss$nasbench)),
          cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""),
          test_loss = py_to_r(cell$get_test_loss(ss$nasbench)),
          runtime = py_to_r(cell$get_runtime(ss$nasbench))
        )
      }))
      tmp[, niche := nb$get_niche_dt(tmp[, "num_params"])]
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("num_params", tags = "feature"),
      ParamUty$new("niche", tags = "niche"))
    ),
    properties = "single-crit",
    id = "nb101_qdo",
    check_values = FALSE
  )

  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = n_evals),
    check_values = FALSE
  )

  instance
}

make_nb101_qdo_mf = function(ss, nb, maxbudget = 100L * 108L) {
  niches = names(nb$niches)

  domain = ps(arch = p_uty(), epoch = p_int(lower = 4L, upper = 108L, tags = "budget"))
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
        data.table(
          val_loss = py_to_r(cell$get_val_loss(ss$nasbench, epochs = as.integer(x$epoch))),
          num_params = py_to_r(cell$get_num_params(ss$nasbench)),
          cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""),
          test_loss = py_to_r(cell$get_test_loss(ss$nasbench)),
          runtime = py_to_r(cell$get_runtime(ss$nasbench, epochs = as.integer(x$epoch)))
        )
      }))
      tmp[, niche := nb$get_niche_dt(tmp[, "num_params"])]
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("num_params", tags = "feature"),
      ParamUty$new("niche", tags = "niche"))
    ),
    properties = "single-crit",
    id = "nb101_qdo_mf",
    check_values = FALSE
  )

  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    terminator = trm("budget", budget = maxbudget),
    check_values = FALSE
  )

  instance
}

make_nb201_moo = function(ss, n_evals = 100L) {
  domain = ps(arch = p_uty())
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:30)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:30)))))
  }

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(
          val_loss = py_to_r(cell$get_val_loss(ss$nasbench, dataset = ss$dataset)),
          latency = py_to_r(cell$get_latency(ss$nasbench, dataset = ss$dataset)),
          cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""),
          test_loss = py_to_r(cell$get_test_loss(ss$nasbench, dataset = ss$dataset)),
          runtime = py_to_r(cell$get_runtime(ss$nasbench, dataset = ss$dataset))
        )
      }))
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("latency", tags = "minimize"))
    ),
    properties = "multi-crit",
    id = "nb201_moo",
    check_values = FALSE
  )

  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = n_evals),
    check_values = FALSE
  )

  instance
}

make_nb201_moo_mf = function(ss, maxbudget = 100L * 200L) {
  domain = ps(arch = p_uty(), epoch = p_int(lower = 1L, upper = 200L, tags = "budget"))
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:30)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:30)))), epoch = x$epoch)
  }

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(
          val_loss = py_to_r(cell$get_val_loss(ss$nasbench, epochs = as.integer(x$epoch), dataset = ss$dataset)),
          latency = py_to_r(cell$get_latency(ss$nasbench, dataset = ss$dataset)),
          cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""),
          test_loss = py_to_r(cell$get_test_loss(ss$nasbench, dataset = ss$dataset)),
          runtime = py_to_r(cell$get_runtime(ss$nasbench, epochs = as.integer(x$epoch), dataset = ss$dataset))
        )
      }))
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("latency", tags = "minimize"))
    ),
    properties = "multi-crit",
    id = "nb201_moo_mf",
    check_values = FALSE
  )
  
  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("budget", budget = maxbudget),
    check_values = FALSE
  )

  instance
}

make_nb201_qdo = function(ss, nb, n_evals = 100L) {
  niches = names(nb$niches)

  domain = ps(arch = p_uty())
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:30)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:30)))))
  }

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(
          val_loss = py_to_r(cell$get_val_loss(ss$nasbench, dataset = ss$dataset)),
          latency = py_to_r(cell$get_latency(ss$nasbench, dataset = ss$dataset)),
          cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""),
          test_loss = py_to_r(cell$get_test_loss(ss$nasbench, dataset = ss$dataset)),
          runtime = py_to_r(cell$get_runtime(ss$nasbench, dataset = ss$dataset))
        )
      }))
      tmp[, niche := nb$get_niche_dt(tmp[, "latency"])]
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("latency", tags = "feature"),
      ParamUty$new("niche", tags = "niche"))
    ),
    properties = "single-crit",
    id = "nb201_qdo",
    check_values = FALSE
  )

  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = n_evals),
    check_values = FALSE
  )

  instance
}

make_nb201_qdo_mf = function(ss, nb, maxbudget = 100L * 200L) {
  niches = names(nb$niches)

  domain = ps(arch = p_uty(), epoch = p_int(lower = 1L, upper = 200L, tags = "budget"))
  domain$trafo = function(x, param_set) {
    cell = ss$get_cell(x$arch)
    path = as.integer(py_to_r(cell$encode("trunc_path")))
    x$path = as.data.table(setNames(as.list(path), paste0("P", 1:30)))
    x$arch = list(arch = list(x$arch), path = list(as.data.table(setNames(as.list(path), paste0("P", 1:30)))), epoch = x$epoch)
  }

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        cell = ss$get_cell(arch)
        data.table(
          val_loss = py_to_r(cell$get_val_loss(ss$nasbench, epochs = as.integer(x$epoch), dataset = ss$dataset)),
          latency = py_to_r(cell$get_latency(ss$nasbench, dataset = ss$dataset)),
          cell_hash = paste0(py_to_r(ss$get_hash(arch)), collapse = ""),
          test_loss = py_to_r(cell$get_test_loss(ss$nasbench, dataset = ss$dataset)),
          runtime = py_to_r(cell$get_runtime(ss$nasbench, epochs = as.integer(x$epoch), dataset = ss$dataset))
        )
      }))
      tmp[, niche := nb$get_niche_dt(tmp[, "latency"])]
      tmp = cbind(tmp, rbindlist(xdt$path))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new("latency", tags = "feature"),
      ParamUty$new("niche", tags = "niche"))
    ),
    properties = "single-crit",
    id = "nb201_qdo_mf",
    check_values = FALSE
  )

  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    terminator = trm("budget", budget = maxbudget),
    check_values = FALSE
  )

  instance
}

