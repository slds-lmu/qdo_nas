#' @title Acquisition Function Thompson Sampling of Elites
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_ts
#'
#' @templateVar id ts
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Thompson Sampling of Elites
#'
#' @family Acquisition Function
#' @export
AcqFunctionTS = R6Class("AcqFunctionTS",
  inherit = AcqFunction,
  public = list(

    #' @field niches ([bbotk::Niches]).
    niches = NULL,

    #' @field y_bests (`list()`).
    y_bests = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateLearners]).
    #' @param niches ([bbotk::Niches]).
    initialize = function(surrogate, niches) {
      assert_r6(surrogate, "SurrogateLearners")
      assert_r6(niches, "Niches")
      self$niches = niches

      super$initialize("acq_ts", surrogate = surrogate, direction = "maximize")
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
    .fun = function(xdt) {
      # FIXME: this currently only works for a single feature function in cols_g
      post_y = predict(self$surrogate$model[[self$cols_y]]$model, newdata = xdt, type = "SK", cov.compute = TRUE)
      mu_y = post_y$mean
      sigma_y = post_y$cov
      sample_y = mvtnorm::rmvnorm(1L, mean = mu_y, sigma = sigma_y)[1L, ] * self$surrogate_max_to_min[[self$cols_y]]
      post_g = predict(self$surrogate$model[[self$cols_g]]$model, newdata = xdt, type = "SK", cov.compute = TRUE)
      mu_g = post_g$mean
      sigma_g = post_g$cov
      sample_g = mvtnorm::rmvnorm(1L, mean = mu_g, sigma = sigma_g)[1L, ] * self$surrogate_max_to_min[[self$cols_g]]
      sample_niche = self$niches$get_niche_dt(setNames(data.table(sample_g), nm = self$cols_g))
      tmp = setNames(cbind(sample_y, sample_niche), nm = c(self$cols_y, self$cols_niche))

      ts = map_dbl(seq_len(nrow(tmp)), function(i) {
        niches = tmp[i, ][[self$cols_niche]]
        # this also handles the case of overlapping niches
        improvement = map_dbl(niches, function(niche) {
          y_best = self$y_bests[[niche]][[self$cols_y]]
          # FIXME: could also use something like worst like in AcqFunctionEJIE
          if (is.null(y_best)) {
            y_best = max(self$surrogate_max_to_min[[self$cols_y]] * self$archive_data[[self$cols_y]])
          }
          y_best - tmp[i, ][[self$cols_y]]
        })
        sum(improvement)
      })

      data.table(acq_ts = ts)
    }
  )
)

mlr_acqfunctions$add("ts", AcqFunctionTS)

