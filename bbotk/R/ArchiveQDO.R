#' @title Logging object for objective function evaluations QDO
#'
#' @description
#' Container around a [data.table::data.table] which stores all performed
#' function calls of the Objective.
#'
#' @template param_codomain
#' @template param_search_space
#' @template param_xdt
#' @template param_ydt
#' @template param_store_x_domain
#' @export
ArchiveQDO = R6Class("ArchiveQDO",
  inherit = Archive,
  public = list(

    #' @description
    #' Returns the best scoring evaluation for each niche. For single-crit optimization,
    #' the solution that minimizes / maximizes the objective function.
    #' For multi-crit optimization, the Pareto set / front.
    #'
    #' @param batch (`integer()`)\cr
    #' The batch number(s) to limit the best results to. Default is
    #' all batches.
    #'
    #' @param niche (`character()`)\cr
    #' The niche id(s) to limit the best results to. Default is
    #' all niches.
    #'
    #' @return [data.table::data.table()].
    best = function(batch = NULL, niche = NULL) {
      if (self$n_batch == 0L) {
        stop("No results stored in archive")
      }

      data = self$data

      if ("x_domain" %in% names(data)) {
        data = data[, x_domain := NULL]  # FIXME: cannot properly recycle NULL x_domain
      }
      data$orig = seq_len(NROW(data))
      data = data[, lapply(.SD, unlist), by = orig]
      data[, orig := NULL]

      batch = if (is.null(batch)) {
        seq_len(self$n_batch)
      } else {
        assert_integerish(batch, lower = 1L, upper = self$n_batch, coerce = TRUE)
      }
      batch_nr = NULL # CRAN check

      niche = if (is.null(niche)) {
        unique(data[[self$cols_niche]])
      } else {
        assert_string(niche)
      }

      consider = data$batch_nr %in% batch & data[[self$cols_niche]] %in% niche
      tab = data[consider, ]

      max_to_min = mult_max_to_min(self$codomain)
      y_ids = c(self$codomain$ids(tags = "minimize"), self$codomain$ids(tags = "maximize"))
      if (length(y_ids) == 1L) {
        setorderv(tab, y_ids, order = max_to_min[[y_ids]], na.last = TRUE)
        res = unique(tab, by = self$cols_niche)
      } else {
        # FIXME:
        stop("Not implemented.")
        #ymat = t(as.matrix(tab[, self$cols_y, with = FALSE]))
        #ymat = max_to_min * ymat
        #res = tab[!is_dominated(ymat)]
      }

      return(res)
    }
  ),

  active = list(
    #' @field cols_y (`character()`).
    #' Column names of codomain parameters objective related.
    cols_y = function() c(self$codomain$ids(tags = "minimize"), self$codomain$ids(tags = "maximize")),

    #' @field cols_g (`character()`).
    #' Column names of codomain parameters feature related.
    cols_g = function() self$codomain$ids(tags = "feature"),

    #' @field cols_niche (`character()`).
    #' Column names of codomain parameters niche related.
    cols_niche = function() self$codomain$ids(tags = "niche")
  ),
)

