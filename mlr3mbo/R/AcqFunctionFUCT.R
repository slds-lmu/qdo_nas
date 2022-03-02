#' @title Acquisition Function Feature Function Uncertainty
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_fuct
#'
#' @templateVar id fuct
#' @template section_dictionary_acqfunctions
#'
#' @description
#' FIXME:
#'
#' @family Acquisition Function
#' @export
AcqFunctionFUCT = R6Class("AcqFunctionFUCT",
  inherit = AcqFunction,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateLearners]).
    initialize = function(surrogate) {
      assert_r6(surrogate, "SurrogateLearners")

      super$initialize("acq_fuct", surrogate = surrogate, direction = "maximize")
    },

    #' @description
    #' Updates acquisition function.
    update = function() {
      super$update()
    }
  ),

  active = list(
    #' @field archive_data ([data.table::data.table])\cr
    #'   Points to the data of the [bbotk::Archive] of the [Surrogate].
    archive_data = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.archive)) stop("archive_data is read-only.")
      private$.archive$data
    },

    #' @field cols_x (`character()`)\cr
    #'   Points to the feature column names of the the data of the [bbotk::Archive] of the [Surrogate].
    cols_x = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.archive)) stop("cols_x is read-only.")
      private$.archive$cols_x
    },

    #' @field cols_y (`character()`)\cr
    #'   Points to the target column names of the the data of the [bbotk::Archive] of the [Surrogate].
    cols_y = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.archive)) stop("cols_y is read-only.")
      private$.archive$cols_y
    },

    #' @field cols_g (`character()`)\cr
    #'   Points to the feature function column names of the the data of the [bbotk::Archive] of the [Surrogate].
    cols_g = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.archive)) stop("cols_g is read-only.")
      private$.archive$cols_g
    },

    #' @field cols_niche (`character()`)\cr
    #'   Points to the niche column names of the the data of the [bbotk::Archive] of the [Surrogate].
    cols_niche = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.archive)) stop("cols_niche is read-only.")
      private$.archive$cols_niche
    }
  ),

  private = list(
    .fun = function(xdt) {
      p = self$surrogate$predict(xdt)
      se = p[[self$cols_g]]$se
      data.table(acq_fuct = se)
    }
  )
)

mlr_acqfunctions$add("fuct", AcqFunctionFUCT)

