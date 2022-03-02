#' @export
bayesopt_bop_acqo = function(instance, acq_function, acq_optimizers, n_design = 4 * instance$search_space$length) {
  # FIXME: maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstanceQDO")
  assert_r6(acq_function, "AcqFunction")
  #assert_r6(acq_optimizer, "AcqOptimizer")  # FIXME: expects AcqOptimizer
  acq_optimizer = acq_optimizers[[1L]]
  archive = instance$archive

  # FIXME: maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = if(instance$search_space$has_deps) {
      generate_design_random(instance$search_space, n_design)$data
    } else {
      generate_design_lhs(instance$search_space, n_design)$data
    }
    instance$eval_batch(design)
  }

  acq_function$setup(archive)  # setup necessary to determine the domain, codomain (for opt direction) of acq function
  acq_vals = imp_vals = list()

  repeat {
    acq_function$surrogate$update(xydt = char_to_fct(archive_xyg(archive), ps = instance$search_space), y_cols = c(archive$cols_y, archive$cols_g))  # update surrogate model with new data
    acq_function$update(archive)
    xdt_y_alts = map(acq_optimizers, function(acqo) acqo$optimize(acq_function, archive = archive))
    xdt = xdt_y_alts[[1L]][, archive$cols_x, with = FALSE]
    acq_vals = append(acq_vals, list(setNames(map_dbl(xdt_y_alts, function(x) x[[acq_function$codomain$ids()]]), nm = map_chr(acq_optimizers, function(x) class(x$optimizer)[1L]))))
    imp_vals = append(imp_vals, list(setNames(map_dbl(xdt_y_alts, function(x) {
      xss_trafoed = transform_xdt_to_xss(x[, archive$cols_x, with = FALSE], instance$search_space)
      new = instance$objective$eval_many(xss_trafoed)
      best = archive$best(niche = new[[archive$cols_niche]][[1L]])
      if (NROW(best) == 0L) {
        return(NA_real_)
      }
      tmp = abs(best[[archive$cols_y]] - new[[archive$cols_y]])  # FIXME: expects minimization and a single niche
      if (new[[archive$cols_y]] <= best[[archive$cols_y]]) {
        tmp
      } else {
        - tmp
      }
    }), nm = map_chr(acq_optimizers, function(x) class(x$optimizer)[1L]))))
    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(archive)) break
  }

  return(list(archive = instance$archive, acq_vals = acq_vals, imp_vals = imp_vals))
}

