#' @title Acquisition Function Independent Thompson Sampling
#'
#' @description
#' Independent Thompson Sampling
#'
#' TODO DESCRIPTION and Reference
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionITS = R6Class("AcqFunctionITS",
  inherit = AcqFunction,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      assert_r6(surrogate, "SurrogateSingleCrit")

      fun = function(xdt) {
        p = self$surrogate$predict(xdt)
        mu = p$mean
        se = p$se
        its = - self$surrogate_max_to_min * rnorm(NROW(p), mean = mu, sd = se)
        data.table(acq_its = its)
      }

      super$initialize("acq_its", surrogate = surrogate, direction = "maximize", fun = fun)
    }
  )
)
