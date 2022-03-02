#' @title Optimization Instance with budget and archive QDO
#'
#' @description
#' Abstract base class.
#'
#' @section Technical details:
#' The [Optimizer] writes the final result to the `.result` field by using
#' the `$assign_result()` method. `.result` stores a [data.table::data.table]
#' consisting of x values in the *search space*, (transformed) x values in the
#' *domain space* and y values in the *codomain space* of the [Objective]. The
#' user can access the results with active bindings (see below).
#'
#' @template param_xdt
#' @template param_search_space
#' @export
OptimInstanceQDO = R6Class("OptimInstanceQDO",
  inherit = OptimInstance,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param objective ([Objective]).
    #' @param terminator ([Terminator]).
    #' @param check_values (`logical(1)`)\cr
    #' Should x-values that are added to the archive be checked for validity?
    #' Search space that is logged into archive.
    initialize = function(objective, search_space = NULL, terminator,
      check_values = TRUE) {

      self$objective = assert_r6(objective, "Objective")

      domain_search_space = self$objective$domain$search_space()
      self$search_space = if (is.null(search_space) && domain_search_space$length == 0) {
        self$objective$domain
      } else if (is.null(search_space) && domain_search_space$length > 0) {
        domain_search_space
      } else if (!is.null(search_space) && domain_search_space$length == 0) {
        assert_param_set(search_space)
      } else {
        stop("If the domain contains TuneTokens, you cannot supply a search_space.")
      }
      self$terminator = assert_terminator(terminator, self)

      assert_flag(check_values)

      is_rfundt = inherits(self$objective, "ObjectiveRFunDt")

      self$archive = ArchiveQDO$new(search_space = self$search_space,
        codomain = objective$codomain, check_values = check_values,
          store_x_domain = !is_rfundt || self$search_space$has_trafo)

      if (!self$search_space$all_numeric) {
        private$.objective_function = objective_error
      } else {
        private$.objective_function = objective_function
      }
      self$objective_multiplicator = self$objective$codomain$maximization_to_minimization
    }
  )
)

