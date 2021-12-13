nas_sampler = function(n, col_x = "arch") {
  stopifnot(n == 1L)  # FIXME:
  # FIXME: requires ss
  list(data = setNames(data.table(x = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))), nm = col_x))
}

nas_sampler_hb = function(n, col_x = "arch") {
  data = map_dtr(seq_len(n), function(i) {
    setNames(data.table(x = list(ss$get_cell()$random_cell(ss$nasbench, "adj"))), nm = col_x)
  })
  list(data = data)
}

OptimizerNAS = R6Class("OptimizerNAS",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(y_col = NULL) {
      param_set = ps()

      self$y_col = assert_string(y_col, null.ok = TRUE)

      super$initialize(
        param_set = param_set,
        param_classes = c("ParamUty"),
        properties = c("single-crit")
      )
    },

    # FIXME: document
    archive = NULL,

    # FIXME: document
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
      #setorderv(candidates, cols = y_col, order = surrogate_mult_max_to_min(archive$codomain, y_cols = y_col))
      #candidates = candidates[seq_len(n_select), ]
      mutations = rbindlist(apply(candidates, MARGIN = 1L, FUN = function(candidate) {
        arch = candidate$arch
        cell = ss$get_cell(arch)
        tmp = map_dtr(seq_len(n_mutations), function(i) {
          mutation = cell$mutate(ss$nasbench)
          data.table(arch = list(mutation), cell_hash = paste0(py_to_r(ss$get_hash(mutation)), collapse = ""))
        })
        tmp = tmp[which(!duplicated(tmp$cell_hash) & tmp$cell_hash %nin% archive$data$cell_hash), ]
        tmp
      }))
      inst$eval_batch(mutations)
    }
  )
)

get_test_loss = function(arch) {
  cell = ss$get_cell(arch)
  data.table(test_loss = py_to_r(cell$get_test_loss(ss$nasbench)))
}

cummin_per_niche = function(archive, budget_var = "epoch") {
  # FIXME: id --> iter
  data = copy(archive$data)

  if ("x_domain" %in% names(data)) {
    data = data[, x_domain := NULL]  # FIXME: cannot properly recycle NULL x_domain
  }
  data$orig = seq_len(NROW(data))
  data = data[, lapply(.SD, unlist), by = orig]
  data[, orig := NULL]
  data[, id := seq_len(.N)]

  max_to_min = mult_max_to_min(archive$codomain)
  y_ids = c(archive$codomain$ids(tags = "minimize"), archive$codomain$ids(tags = "maximize"))

  res = map_dtr(data$id, function(i) {
    tmp = data[id <= i, ]
    res = tmp[, .(incumbent = cummin(get(y_ids))), by = .(niche)]
    res = res[, .(incumbent = min(incumbent)), by = .(niche)]
    #wide = dcast(tmp[, .(incumbent = cummin(get(y_ids))), by = .(niche)], . ~ niche, value.var = "incumbent", fun.aggregate = min)[, -1L]
    #wide[, id := i]
    #wide[, cumbudget := sum(tmp[[budget_var]])]
    #wide
    res[, id := i]
    res[, cumbudget := sum(tmp[[budget_var]])]
    res
  }, .fill = TRUE)
  res
}

