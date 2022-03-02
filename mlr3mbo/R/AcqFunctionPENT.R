#' @title Acquisition Function Probability in Niche Entropy
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_pent
#'
#' @templateVar id pent
#' @template section_dictionary_acqfunctions
#'
#' @description
#' FIXME:
#'
#' @family Acquisition Function
#' @export
AcqFunctionPENT = R6Class("AcqFunctionPENT",
  inherit = AcqFunction,
  public = list(

    #' @field niches ([bbotk::Niches]).
    niches = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateLearners]).
    initialize = function(surrogate, niches) {
      assert_r6(surrogate, "SurrogateLearners")
      assert_r6(niches, "Niches")
      self$niches = niches

      super$initialize("acq_pent", surrogate = surrogate, direction = "maximize")
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

      prob_j = {
        p_g = p[self$cols_g]
        mu_g = if (is.list(p_g) & !is.data.table(p_g)) map(p_g, "mean") else setNames(list(p_g$mean), nm = self$cols_g)
        se_g = if (is.list(p_g) & !is.data.table(p_g)) map(p_g, "se") else setNames(list(p_g$se), nm = self$cols_g)

        if (test_r6(self$niches, classes = "NichesBoundaries")) {
          map(names(self$niches$niches), function(niche) {
            boundaries = self$niches$niches[[niche]]$niche_boundaries
            pj = 1
            for (id in self$cols_g) {
              pj = pj * (pnorm((mu_g[[id]] - boundaries[[id]][1L]) / se_g[[id]]) - pnorm((mu_g[[id]] - boundaries[[id]][2L]) / se_g[[id]]))
            }
            pj
          })
        }
      }

      data.table(acq_pent = - Reduce("+", pmap(list(prob_j, map(prob_j, log)), `*`)))
    }
  )
)

mlr_acqfunctions$add("pent", AcqFunctionPENT)

