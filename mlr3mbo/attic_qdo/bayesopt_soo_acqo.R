#' @title Single Objective Bayesian Optimization Acqo
#'
#' @description
#' FIXME:
#' @template param_instance
#' @template param_acq_function
#' @param acq_optimizers (`list`)\cr
#' List of [AcqOptimizer]s.
#'   In case the `archive` inside the `instance` is empty, we generate a random initial design of `n_design` points.
#' @param n_design (`integer(1)`)\cr
#'   In case the `archive` inside the `instance` is empty, we generate a random initial design of `n_design` points.
#' @return [bbotk::Archive].
#'
#' @references
#' `r format_bib("jones_1998")`
#'
#' @export
bayesopt_soo_acqo = function(instance, acq_function, acq_optimizers, n_design = 4 * instance$search_space$length) {
  # FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstance")
  assert_r6(acq_function, "AcqFunction")
  #assert_r6(acq_optimizer, "AcqOptimizer")  # FIXME: expects AcqOptimizer_old
  acq_optimizer = acq_optimizers[[1L]]
  archive = instance$archive

  # FIXME maybe do not have this here, but have a general init helper
  if (archive$n_evals == 0) {
    design = if(instance$search_space$has_deps) {
      generate_design_random(instance$search_space, n_design)$data
    } else {
      generate_design_lhs(instance$search_space, n_design)$data
    }
    instance$eval_batch(design)
  }

  acq_function$setup(archive) # setup necessary to determine the domain, codomain (for opt direction) of acq function
  acq_vals = imp_vals = list()

  repeat {
    acq_function$surrogate$update(xydt = char_to_fct(archive_xy(archive), ps = instance$search_space), y_cols = archive$cols_y)  # update surrogate model with new data

    # NOTE: necessary because we have to determine e.g. y_best for ei.
    # There are possible other costy calculations that we just want to do once for each state.
    # We might not want to do these calculation in acq_function$fun() because this can get called several times during the optimization.
    # One more costy example would be AEI, where we ask the surrogate for the mean prediction of the points in the design.
    # Alternatively the update could be called by the AcqOptimizer (but he should not need to know about the archive, so then the archive also has to live in the AcqFunction).
    acq_function$update(archive)
    xdt_y_alts = map(acq_optimizers, function(acqo) acqo$optimize(acq_function, archive = archive))
    xdt = xdt_y_alts[[1L]][[1L]]
    acq_vals = append(acq_vals, list(setNames(map_dbl(xdt_y_alts, function(x) x[[2L]][[1L]]), nm = map_chr(acq_optimizers, function(x) class(x)[1L]))))
    imp_vals = append(imp_vals, list(setNames(map_dbl(xdt_y_alts, function(x) {
      xss_trafoed = transform_xdt_to_xss(x[[1L]], instance$search_space)
      new = instance$objective$eval_many(xss_trafoed)
      best = archive$best()
      if (NROW(best) == 0L) return (NA_real_)
      tmp = abs(best[[archive$cols_y]] - new[[archive$cols_y]])  # FIXME: expects maximization
      if (new[[archive$cols_y]] >= best[[archive$cols_y]]) {
        tmp
      } else {
        - tmp
      }
    }), nm = map_chr(acq_optimizers, function(x) class(x)[1L]))))
    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(archive)) break
  }

  return(list(archive = instance$archive, acq_vals = acq_vals, imp_vals = imp_vals))
}

