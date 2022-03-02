#' @title Sequential Multicriteria Bayesian Optimization Via SmsEGO.
#'
#' @description
#' MBO loop function for sequential multicriteria Bayesian optimization via SmsEGO.
#' Normally used inside an [OptimizerMbo].
#'
#' @param instance ([bbotk::OptimInstanceMultiCrit])\cr
#'   The [bbotk::OptimInstanceMultiCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` \code{4 * d} is used with \code{d} being the dimensionality of the search space.
#'   Points are drawn uniformly at random.
#' @param surrogate (`NULL` | [SurrogateLearners])\cr
#'   [SurrogateLearners] to be used as a surrogate.
#'   If `NULL` \code{default_surrogate(instance)} is used.
#' @param acq_function (`NULL` | [AcqFunctionSmsEgo]).\cr
#'   [AcqFunctionSmsEgo] to be used as acquisition function.
#'   If `NULL` an [AcqFunctionSmsEgo] is used.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#'   If `NULL` \code{default_acqopt(acqfun)} is used.
#' @param sampler (`NULL` | any)\cr
#'   Any optional sampler holding a `$sample()` function.
#'
#' @note
#' * If `surrogate` is `NULL` but `acq_function` is given and contains a `$surrogate`, this
#'   [SurrogateLearners] is used.
#' * You can pass a `surrogate` that was not given the [bbotk::Archive] of the
#'   `instance` during initialization.
#'   In this case, the [bbotk::Archive] of the given `instance` is set during execution.
#' * Similarly, you can pass an `acq_function` that was not given the `surrogate` during initialization
#'   and an `acq_optimizer` that was not given the `acq_function`, i.e., delayed initialization is
#'   handled automatically.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @references
#' * `r format_bib("beume_2007")`
#' * `r format_bib("ponweiser_2008")`
#' @family Loop Function
#' @export
#' @examples
#' library(bbotk)
#' library(paradox)
#' library(mlr3learners)
#'
#' fun = function(xs) {
#'   list(y1 = xs$x^2, y2 = (xs$x - 2) ^ 2)
#' }
#' domain = ps(x = p_dbl(lower = -10, upper = 10))
#' codomain = ps(y1 = p_dbl(tags = "minimize"), y2 = p_dbl(tags = "minimize"))
#' objective = ObjectiveRFun$new(fun = fun, domain = domain, codomain = codomain)
#'
#' terminator = trm("evals", n_evals = 10)
#'
#' instance = OptimInstanceMultiCrit$new(
#'   objective = objective,
#'   terminator = terminator
#' )
#'
#' bayesopt_smsego(instance)
bayesopt_smsego = function(
    instance,
    init_design_size = NULL,
    surrogate = NULL,
    acq_function = NULL,
    acq_optimizer = NULL,
    sampler = NULL
  ) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceMultiCrit")
  assert_r6(instance$terminator, "TerminatorEvals")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(surrogate, classes = "SurrogateLearners", null.ok = TRUE)
  assert_r6(acq_function, classes = "AcqFunctionSmsEgo", null.ok = TRUE)
  assert_r6(acq_optimizer, classes = "AcqOptimizer", null.ok = TRUE)
  sampler = sampler %??% SamplerUnif$new(instance$search_space)

  surrogate = surrogate %??% acq_function$surrogate

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  k = length(archive$cols_y)  # codomain can hold non targets since #08116aa02204980f87c8c08841176ae8f664980a
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4 * d
  if (is.null(surrogate)) surrogate = default_surrogate(instance)
  if (is.null(acq_function)) acq_function = AcqFunctionSmsEgo$new()
  if (is.null(acq_optimizer)) acq_optimizer = default_acqopt(acq_function)
  surrogate$archive = archive
  #acq_function$surrogate = surrogate  # FIXME:
  acq_optimizer$acq_function = acq_function

  # initial design
  if (isTRUE(init_design_size > 0L)) {
    design = generate_design_random(domain, n = init_design_size)$data
    instance$eval_batch(design)
  }

  # scaling based on initial design
  #scaling = archive$data[, list(mean_y = map_dbl(.SD, mean), sd_y = map_dbl(.SD, stats::sd)), .SDcols = archive$cols_y]
  #for (i in seq_along(archive$cols_y)) {
  #  archive$data[, archive$cols_y[i] := (get(archive$cols_y[i]) - scaling$mean_y[i]) / scaling$sd_y[i]]
  #}
  #on.exit({
  #  for (i in seq_along(archive$cols_y)) {
  #    archive$data[, archive$cols_y[i] := (get(archive$cols_y[i]) * scaling$sd_y[i]) + scaling$mean_y[i]]
  #  }
  #})  # rescale on exit

  # loop
  repeat {
    xdt = tryCatch({
      acq_function$progress = instance$terminator$param_set$values$n_evals - archive$n_evals  # FIXME: this needs to be handled differently
      acq_function$surrogate$update()
      acq_function$update()
      acq_optimizer$optimize()
    }, mbo_error = function(mbo_error_condition) {
      lg$info("Proposing a randomly sampled point")
      sampler$sample(1L)$data
    })

    instance$eval_batch(xdt)
    #for (i in seq_along(archive$cols_y)) {
    #  archive$data[batch_nr == archive$n_batch, archive$cols_y[i] := (get(archive$cols_y[i]) - scaling$mean_y[i]) / scaling$sd_y[i]]
    #}

    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

