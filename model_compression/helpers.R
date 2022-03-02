# from https://github.com/mlr-org/miesmuschel/blob/master/R/TerminatorBudget.R
TerminatorBudget = R6Class("TerminatorBudget", inherit = Terminator,
  public = list(
    #' @description
    #' Initialize the `TerminatorBudget` object.
    initialize = function() {
      param_set = ps(budget = p_dbl(tags = "required"), aggregate = p_uty(tags = "required", custom_check = function(x) {
        if (test_function(x) && test_number(x(NULL), finite = TRUE)) return(TRUE)
        "must be a function with one argument, which when called with NULL must return a finite numeric value."
      }))
      param_set$values = list(budget = Inf, aggregate = sum)
      super$initialize(param_set = param_set, properties = c("single-crit", "multi-crit"))
      self$unit = "percent"
    },

    #' @description
    #' Is `TRUE` if when the termination criterion is matched, `FALSE` otherwise.
    #' @param archive [`Archive`][bbotk::Archive]
    #'   Archive to check.
    #' @return `logical(1)`: Whether to terminate.
    is_terminated = function(archive) {
      assert_r6(archive, "Archive")
      params = self$param_set$get_values()
      budget_id = archive$search_space$ids(tags = "budget")
      if (length(budget_id) != 1) stopf("Need exactly one budget parameter, but found %s: %s",
        length(budget_id), str_collapse(budget_id))
      params$aggregate(archive$data[[budget_id]]) >= params$budget
    }
  ),
  private = list(
    .status = function(archive) {
      params = self$param_set$get_values()
      budget_id = archive$search_space$ids(tags = "budget")
      if (length(budget_id) != 1) stopf("Need exactly one budget parameter, but found %s: %s",
        length(budget_id), str_collapse(budget_id))

      origin = params$aggregate(NULL)
      aggregated = params$aggregate(archive$data[[budget_id]])

      c(
        max_steps = if (params$budget <= origin) 0 else 100,
        # when budget <= origin, then we are terminated from the beginning, and want to avoid negative numbers / division by 0.
        current_steps = if (params$budget <= origin) 0 else floor((aggregated - origin) / (params$budget - origin) * 100)
      )
    }
  )
)
mlr_terminators$add("budget", TerminatorBudget)

