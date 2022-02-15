# Custom sampler for architectures
# FIXME: document
NASSampler = R6Class("NASSampler",
  public = list(
    ss = NULL,

    col_x = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(ss, col_x = "arch") {
      self$ss = ss
      self$col_x = col_x
    },

    sample = function(n) {
      data = map_dtr(seq_len(n), function(i) {
        setNames(data.table(x = list(self$ss$get_cell()$random_cell(self$ss$nasbench, "adj"))), nm = self$col_x)
      })
      list(data = data)
    }
  )
)

# Optimizer used as acquisition function optimizer, mimics what is done in mutation in BANANAS
# FIXME: document
OptimizerNAS = R6Class("OptimizerNAS",
  inherit = Optimizer,
  public = list(
    ss = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(ss, y_col = NULL) {
      param_set = ps()

      self$ss = ss
      self$y_col = assert_string(y_col, null.ok = TRUE)

      super$initialize(
        param_set = param_set,
        param_classes = c("ParamUty"),
        properties = c("single-crit")
      )
    },

    archive = NULL,

    y_col = NULL
  ),

  private = list(
    # FIXME: cell_hash comparison is hacky
    .optimize = function(inst) {
      archive = self$archive
      y_col = self$y_col %??% archive$cols_y  # FIXME:
      n_mutations = 100L  # FIXME: in param_set; this is overall - per candidate is ceiling(n_mutations / NROW(candidates))
      candidates = archive$best()[, c(archive$cols_x, y_col, "cell_hash"), with = FALSE]  # for QDO best per niche, otherwise best pareto
      n_mutations = ceiling(n_mutations / NROW(candidates))
      mutations = rbindlist(apply(candidates, MARGIN = 1L, FUN = function(candidate) {
        arch = candidate$arch
        cell = self$ss$get_cell(arch)
        tmp = map_dtr(seq_len(n_mutations), function(i) {
          mutation = cell$mutate(self$ss$nasbench)
          data.table(arch = list(mutation), cell_hash = paste0(py_to_r(self$ss$get_hash(mutation)), collapse = ""))
        })
        tmp = tmp[which(!duplicated(tmp$cell_hash) & tmp$cell_hash %nin% archive$data$cell_hash), ]
        tmp
      }))
      inst$eval_batch(mutations)
    }
  )
)

get_test_loss = function(arch, ss) {
  cell = ss$get_cell(arch)
  data.table(test_loss = py_to_r(cell$get_test_loss(ss$nasbench)))
}

cummin_per_niche = function(archive, nb, y_var = "val_loss", budget_var = "epoch", time_var = "runtime", worst = 100) {
  data = copy(archive$data)
  data[, iter := seq_len(.N)]
  data[, cumbudget := cumsum(data[[budget_var]])]
  data[, cumtime := cumsum(data[[time_var]])]

  if ("x_domain" %in% names(data)) {
    data = data[, x_domain := NULL]  # FIXME: cannot properly recycle NULL x_domain
  }
  data$orig = seq_len(NROW(data))
  data = data[, lapply(.SD, unlist), by = orig]
  data[, orig := NULL]

  niches_ids = map_chr(nb$niches, "id")

  res = map_dtr(unique(data$iter), function(i) {
    tmp = data[iter <= i, ]
    res = tmp[, .(incumbent = cummin(get(y_ids))), by = .(niche)]
    res = res[, .(incumbent = min(incumbent)), by = .(niche)]
    for (nid in niches_ids[niches_ids %nin% res$niche]) {
      res = rbind(res, data.table(niche = nid, incumbent = worst))
    }
    res[, iter := i]
    stopifnot(length(unique(data[iter == i]$cumbudget)) == 1L)
    res[, cumbudget := data[iter == i]$cumbudget[1L]]
    stopifnot(length(unique(data[iter == i]$cumtime)) == 1L)
    res[, cumtime := data[iter == i]$cumtime[1L]]
    res
  }, .fill = TRUE)
  res
}

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

