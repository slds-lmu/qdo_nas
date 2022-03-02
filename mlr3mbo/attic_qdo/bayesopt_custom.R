#' @export
bayesopt_bop_bananas_trafo = function(instance, acq_function, acq_optimizer, n_design = 4 * instance$search_space$length, trafo = NULL) {
  # FIXME: maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstanceQDO")
  assert_r6(acq_function, "AcqFunction")
  #assert_r6(acq_optimizer, "AcqOptimizer")
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
  acq_function$domain$trafo = trafo

  repeat {
    acq_function$surrogate$update(xydt = archive_bananasyg(archive), y_cols = c(archive$cols_y, archive$cols_g))  # update surrogate model with new data

    acq_function$update(archive)
    xdt = acq_optimizer$optimize(acq_function, archive)
    #xdt = acq_optimizer$optimize(acq_function, archive = archive)
    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(archive)) break
  }

  return(instance$archive)
}

#' @export
bayesopt_soo_bananas_trafo = function(instance, acq_function, acq_optimizer, n_design = 4 * instance$search_space$length, trafo = NULL) {
  # FIXME maybe do not have this here, but have a general assert helper
  assert_r6(instance, "OptimInstance")
  assert_r6(acq_function, "AcqFunction")
  #assert_r6(acq_optimizer, "AcqOptimizer")
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
  acq_function$domain$trafo = trafo

  repeat {
    acq_function$surrogate$update(xydt = archive_bananasy(archive), y_cols = archive$cols_y)  # update surrogate model with new data

    acq_function$update(archive)

    xdt = acq_optimizer$optimize(acq_function, archive)
    #xdt = acq_optimizer$optimize(acq_function, archive = archive)
    instance$eval_batch(xdt)
    if (instance$is_terminated || instance$terminator$is_terminated(archive)) break
  }

  return(instance$archive)
}

