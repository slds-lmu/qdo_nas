make_mobilenet_moo = function(feature_var, n_evals = 100L) {
  domain = ps(arch = p_uty())
  domain$trafo = function(x, param_set) {
    enc = as.integer(py$arch2feature(x$arch))
    x$arch = list(arch = list(x$arch), enc = list(as.data.table(setNames(as.list(enc), paste0("E", 1:128)))))
  }
  
  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        data.table(
          val_loss = 100 - py$predict_acc(arch),
          feature = if (feature_var == "latency") py$predict_eff(arch) else if (feature_var == "flops") py$predict_flops(arch)
        )
      }))
      colnames(tmp) = c("val_loss", feature_var)
      tmp = cbind(tmp, rbindlist(xdt$enc))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new(feature_var, tags = "minimize"))
    ),
    properties = "multi-crit",
    id = "ofa_mobilenet_moo",
    check_values = FALSE
  )
  
  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = n_evals),
    check_values = FALSE
  )

  instance
}

make_mobilenet_qdo = function(feature_var, nb, n_evals = 100L) {
  domain = ps(arch = p_uty())
  domain$trafo = function(x, param_set) {
    enc = as.integer(py$arch2feature(x$arch))
    x$arch = list(arch = list(x$arch), enc = list(as.data.table(setNames(as.list(enc), paste0("E", 1:128)))))
  }
  
  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = rbindlist(apply(xdt, MARGIN = 1L, FUN = function(x) {
        arch = x$arch
        data.table(
          val_loss = 100 - py$predict_acc(arch),
          feature = if (feature_var == "latency") py$predict_eff(arch) else if (feature_var == "flops") py$predict_flops(arch)
        )
      }))
      colnames(tmp) = c("val_loss", feature_var)
      tmp[, niche := nb$get_niche_dt(tmp[, eval(feature_var), with = FALSE])]
      tmp = cbind(tmp, rbindlist(xdt$enc))
      tmp
    },
    domain = domain,
    codomain = ParamSet$new(list(
      ParamDbl$new("val_loss", tags = "minimize"),
      ParamDbl$new(feature_var, tags = "feature"),
      ParamUty$new("niche", tags = "niche"))
    ),
    properties = "single-crit",
    id = "ofa_mobilenet_qdo",
    check_values = FALSE
  )
  
  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    terminator = trm("evals", n_evals = n_evals),
    check_values = FALSE
  )

  instance
}

