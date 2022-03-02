#' @title Acquisition Function Expected Joint Improvement of Elites
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ejie
#'
#' @templateVar id ejie
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Expected Joint Improvement of Elites.
#'
#' @references
#' `r format_bib("kent_2020")`
#' @family Acquisition Function
#' @export
AcqFunctionEJIE = R6Class("AcqFunctionEJIE",
  inherit = AcqFunction,
  public = list(

    #' @field niches ([bbotk::Niches]).
    niches = NULL,

    #' @field y_bests (`list()`).
    y_bests = NULL,

    #' @field y_worst (`double(1)`).
    worst = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateLearners]).
    #' @param niches ([bbotk::Niches]).
    #' @param worst (`double(1)`)
    initialize = function(surrogate, niches, worst) {
      assert_r6(surrogate, "SurrogateLearners")
      assert_r6(niches, "Niches")
      assert_number(worst)
      self$niches = niches
      self$worst = worst

      super$initialize("acq_ejie", surrogate = surrogate, direction = "maximize")
    },

    #' @description
    #' Updates acquisition function and sets `y_bests`.
    update = function() {
      super$update()
      self$y_bests = setNames(transpose_list(self$surrogate_max_to_min[self$cols_y] * self$archive$best()[, self$cols_y, with = FALSE]), nm = self$archive$best()[[self$cols_niche]])
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
    .fun = function(xdt, ...) {
      constants = list(...)[self$constants$ids()]
      if (length(constants)) {
        xdt[, names(constants) := constants]  # FIXME: check this
      }
      if (is.null(self$y_bests)) {
        stop("y_bests is not set. Missed to call $update(archive)?")
      }

      p = self$surrogate$predict(xdt)
      mu = p[[self$cols_y]]$mean
      se = p[[self$cols_y]]$se

      ei_j = map(names(self$niches$niches), function(niche) {
        best = self$y_bests[[niche]][[self$cols_y]]

        # if no point in niche set best to worst
        if (!length(best)) {
          best = self$worst
          #best = max(self$surrogate_max_to_min[self$cols_y] * self$archive$data[[self$cols_y]]) 
        }

        d = best - self$surrogate_max_to_min[[self$cols_y]] * mu
        d_norm = d / se
        ei_j = d * pnorm(d_norm) + se * dnorm(d_norm)
      })

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
      # FIXME: sum to 1?
      ejie = Reduce("+", pmap(list(ei_j, prob_j), function(ec, pc) ec * pc))

      ejie[se < 1e-20] = 0
      data.table(acq_ejie = ejie)
    }
  )
)

mlr_acqfunctions$add("ejie", AcqFunctionEJIE)

