#' @title Acquisition Function Expected Improvement
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ei
#'
#' @templateVar id ei
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Expected Improvement.
#'
#' @references
#' `r format_bib("jones_1998")`
#' @family Acquisition Function
#' @export
AcqFunctionEI = R6Class("AcqFunctionEI",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric(1)`).
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    initialize = function(surrogate = NULL) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      super$initialize("acq_ei", surrogate = surrogate, direction = "maximize")
    },

    #' @description
    #' Updates acquisition function and sets `y_best`.
    update = function() {
      super$update()
      self$y_best = min(self$surrogate_max_to_min * self$archive$data[[self$surrogate$y_cols]])
    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      constants = list(...)[self$constants$ids()]
      if (length(constants)) {
        xdt[, names(constants) := constants]  # FIXME: check this
      }
      if (is.null(self$y_best)) {
        stop("y_best is not set. Missed to call $update()?")
      }
      p = self$surrogate$predict(xdt)
      mu = p$mean
      se = p$se
      d = self$y_best - self$surrogate_max_to_min * mu
      d_norm = d / se
      ei = d * pnorm(d_norm) + se * dnorm(d_norm)
      ei = ifelse(se < 1e-20, 0, ei)
      data.table(acq_ei = ei)
    }
  )
)

mlr_acqfunctions$add("ei", AcqFunctionEI)
