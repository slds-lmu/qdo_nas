#' @title Acquisition Function Constant Mean
#'
#' @description
#' Constant Mean.
#'
#' TODO DESCRIPTION and Reference
#'
#' @family Acquisition Function
#'
#' @export
AcqFunctionMU = R6Class("AcqFunctionMU",
  inherit = AcqFunction,
  public = list(

    #' @field y_best (`numeric()`).
    y_best = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate [SurrogateSingleCrit].
    initialize = function(surrogate) {
      assert_r6(surrogate, "SurrogateSingleCrit")

      fun = function(xdt) {
        p = self$surrogate$predict(xdt)
        mu = p$mean
        data.table(acq_mu = mu)
      }

      super$initialize("acq_mu", surrogate = surrogate, direction = "maximize", fun = fun)
    }
  )
)
