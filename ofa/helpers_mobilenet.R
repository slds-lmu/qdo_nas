# Custom sampler for architectures
# FIXME: document
NASSampler = R6Class("NASSampler",
  public = list(
    arch_encoder = NULL,

    col_x = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(arch_encoder, col_x = "arch") {
      self$arch_encoder = arch_encoder
      self$col_x = col_x
    },

    sample = function(n) {
      data = map_dtr(seq_len(n), function(i) {

        setNames(data.table(x = list({
          tmp = self$arch_encoder$random_sample()
          py_dict(names(tmp), tmp)
        })), nm = self$col_x)
      })
      list(data = data)
    }
  )
)

# Optimizer used as acquisition function optimizer, random sampling based on ofa encoding
# FIXME: document
OptimizerNASRandom = R6Class("OptimizerNASRandom",
  inherit = Optimizer,
  public = list(
    arch_encoder = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(arch_encoder, y_col = NULL) {
      param_set = ps()

      self$arch_encoder = arch_encoder
      self$y_col = assert_string(y_col, null.ok = TRUE)

      self$sampler = NASSampler$new(arch_encoder)

      super$initialize(
        param_set = param_set,
        param_classes = c("ParamUty"),
        properties = c("single-crit")
      )
    },

    archive = NULL,

    y_col = NULL,

    sampler = NULL
  ),

  private = list(
    .optimize = function(inst) {
      archive = self$archive
      y_col = self$y_col %??% archive$cols_y  # FIXME:
      candidates = self$sampler$sample(1000L)$data
      inst$eval_batch(candidates)
    }
  )
)

cummin_per_niche = function(archive, nb, y_var = "val_loss", budget_var = "epoch", worst = 100) {
  data = copy(archive$data)
  data[, iter := seq_len(.N)]
  data[, cumbudget := cumsum(data[[budget_var]])]

  if ("x_domain" %in% names(data)) {
    data = data[, x_domain := NULL]  # FIXME: cannot properly recycle NULL x_domain
  }
  data$orig = seq_len(NROW(data))
  data = data[, lapply(.SD, unlist), by = orig]
  data[, orig := NULL]

  niches_ids = map_chr(nb$niches, "id")

  res = map_dtr(unique(data$iter), function(i) {
    tmp = data[iter <= i, ]
    res = tmp[, .(incumbent = cummin(get(y_var))), by = .(niche)]
    res = res[, .(incumbent = min(incumbent)), by = .(niche)]
    for (nid in niches_ids[niches_ids %nin% res$niche]) {
      res = rbind(res, data.table(niche = nid, incumbent = worst))
    }
    res[, iter := i]
    stopifnot(length(unique(data[iter == i]$cumbudget)) == 1L)
    res[, cumbudget := data[iter == i]$cumbudget[1L]]
    res
  }, .fill = TRUE)
  res[!is.na(niche), ]
}

