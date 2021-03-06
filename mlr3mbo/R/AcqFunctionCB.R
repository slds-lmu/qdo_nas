#' @title Acquisition Function Confidence Bound
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_cb
#'
#' @templateVar id cb
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Confidence Bound.
#'
#' @section Parameters:
#' * `"lambda"` (`numeric(1)`)\cr
#'   \eqn{\lambda} value used for the confidence bound.
#'   Defaults to `2`.
#'
#' @references
#' `r format_bib("snoek_2012")`
#'
#' @family Acquisition Function
#' @export
# FIXME: DESCRIPTION and Reference.
AcqFunctionCB = R6Class("AcqFunctionCB",
  inherit = AcqFunction,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate (`NULL` | [SurrogateLearner]).
    #' @param lambda (`numeric(1)`).
    initialize = function(surrogate = NULL, lambda = 2) {
      assert_r6(surrogate, "SurrogateLearner", null.ok = TRUE)
      assert_number(lambda, lower = 0, finite = TRUE)

      constants = ps(lambda = p_dbl(lower = 0, default = 2))
      constants$values$lambda = lambda

      super$initialize("acq_cb", constants = constants, surrogate = surrogate, direction = "same")
    }
  ),

  private = list(
    .fun = function(xdt, ...) {
      # FIXME: check that lambda is in ... and use this
      p = self$surrogate$predict(xdt)
      res = p$mean - self$surrogate_max_to_min * self$constants$values$lambda * p$se
      data.table(acq_cb = res)
    }
  )
)

mlr_acqfunctions$add("cb", AcqFunctionCB)
