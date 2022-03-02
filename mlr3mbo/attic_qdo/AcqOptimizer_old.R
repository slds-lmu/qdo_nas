#' @title Acquisition Optimizer
#'
#' @description
#' Optimizer for [AcqFunction] objects.
#'
#' @export
AcqOptimizer_old = R6Class("AcqOptimizer_old",
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    #' @param archive [bbotk::Archive].
    optimize = function(acq_function, archive) {
      stop("abstract")
    }
  )
)



#' @title Acquisition Optimizer Random Search
#'
#' @description
#' `AcqOptimizerRandomSearch_old` class that implements a random search for the
#' optimization of acquisition functions.
#'
#' @export
AcqOptimizerRandomSearch_old = R6Class("AcqOptimizerRandomSearch_old",
  inherit = AcqOptimizer_old,

  public = list(

    #' @field param_set ([paradox::ParamSet]).
    param_set = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      self$param_set = ParamSet$new(list(
        ParamInt$new("iters", lower = 1L),
        ParamLgl$new("trafo"),
        ParamLgl$new("return_acqv"))
      )
      self$param_set$values = list(iters = 1000L, trafo = FALSE, return_acqv = FALSE)
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    #' @param archive [bbotk::Archive].
    optimize = function(acq_function, archive) {
      xdt = generate_design_random(acq_function$domain, self$param_set$values$iters)
      txdt = if (self$param_set$values$trafo) {
        as.data.table(do.call(rbind, xdt$transpose()))
      } else {
        xdt$data
      }
      ydt = acq_function$eval_dt(char_to_fct(txdt, ps =acq_function$codomain)) * mult_max_to_min(acq_function$codomain)  # FIXME: ps = acq_function$domain?
      best = which(ydt[[1L]] == min(ydt[[1L]]))
      if (length(best) > 1L) {
        best = sample(best, 1L)
      }
      if (self$param_set$values$return_acqv) {
        list(xdt$data[best, ], ydt[best])
      } else {
        xdt$data[best, ]
      }
    }
))




#' @title Acquisition Optimizer MIES
#'
#' @description
#' `AcqOptimizerMIES_old` class that uses [CEGO::optimMIES] for the
#' optimization of acquisition functions.
#'
#'
#' @export
AcqOptimizerMIES_old = R6Class("AcqOptimizerMIES_old",
  inherit = AcqOptimizer_old,

  public = list(

    #' @field param_set ([paradox::ParamSet]).
    param_set = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      self$param_set = ParamSet$new(list(
        ParamLgl$new("classical"),
        ParamInt$new("budget", lower = 1L),
        ParamInt$new("popsize", lower = 1L),
        ParamLgl$new("niches"))
      )
      self$param_set$values = list(classical = FALSE, budget = 1000L, popsize = 100L, niches = TRUE)
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    #' @param archive [bbotk::Archive].
    optimize = function(acq_function, archive) {
      fun = function(x) {
        mult_max_to_min(acq_function$codomain) * unlist(acq_function$eval_dt(char_to_fct(setNames(transpose(as.data.table(x)), nm = acq_function$domain$ids()), ps = acq_function$domain)))  # FIXME: ps = acq_function$domain?
      }
      # FIXME: CEGO with custom start design is buggy af
      get_sigma0 = function(ps) {
        t(as.matrix(map_dbl(ps$params, .f = function(param) {
          switch(class(param)[1L],
            "ParamDbl" = (param$upper - param$lower) * 0.1,
            "ParamInt" = (param$upper - param$lower) * (1 / 3),
            "ParamFct" = 0.1
          )
        })))
      }

      # FIXME: currently always uses best_niches + 1 random
      if (self$param_set$values$classical) {
        mies_res = CEGO::optimMIES(x = NULL, fun = fun,
          control = list(budget = self$param_set$values$budget,
            popsize = self$param_set$values$popsize, vectorized = TRUE,
            generations = Inf,
            types = acq_function$domain$storage_type,
            lower = unlist(map(acq_function$domain$params, "lower")),
            upper = unlist(map(acq_function$domain$params, "upper"))))
        setNames(transpose(as.data.table(mies_res$xbest)), nm = acq_function$domain$ids())
      } else {
        # FIXME: currently always uses best_niches + 1 random
        best_niches = if (self$param_set$values$niches) {
          acq_function$bests[, acq_function$cols_x, with = FALSE]
        } else {
          acq_function$archive_data[, acq_function$cols_x, with = FALSE]
        }
        xdt = cbind(best_niches, sigma0 = get_sigma0(acq_function$domain))
        x = as.list(transpose(xdt))

        mies_res = CEGO::optimMIES(x = x, fun = fun,
          control = list(budget = self$param_set$values$budget,
            popsize = length(x), vectorized = TRUE,
            generations = Inf,
            types = acq_function$domain$storage_type,
            lower = unlist(map(acq_function$domain$params, "lower")),
            upper = unlist(map(acq_function$domain$params, "upper"))))
        setNames(transpose(as.data.table(mies_res$xbest)), nm = acq_function$domain$ids())
      }
    }
))



#' @title Acquisition Optimizer Mutation Bananas
#'
#' @description
#' `AcqOptimizerMutateBananas` class that implements a mutation
#' algorithm for the optimization of acquisition functions.
#'
#' @export
AcqOptimizerMutateBananas = R6Class("AcqOptimizerMutateBananas",
  inherit = AcqOptimizer_old,

  public = list(

    #' @field param_set ([paradox::ParamSet]).
    param_set = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      self$param_set = ParamSet$new(list(
        ParamInt$new("n", lower = 1L),
        ParamLgl$new("trafo"),
        ParamLgl$new("consider_all"),
        ParamLgl$new("random_interleaving"),
        ParamLgl$new("return_acqv"),
        ParamFct$new("operation", levels = c("mutation", "mut_cross")))
      )
      self$param_set$values = list(n = 100L, trafo = FALSE, consider_all = TRUE, random_interleaving = FALSE, return_acqv = FALSE, operation = "mutation")
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    #' @param archive [bbotk::Archive].
    optimize = function(acq_function, archive) {
      transpose = function(data, ps, filter_na = TRUE, trafo = TRUE) {
        assert_flag(filter_na)
        assert_flag(trafo)
        xs = transpose_list(data)
        if (filter_na) {
          xs = map(xs, function(x) Filter(Negate(is_scalar_na), x))
        }
        if (ps$has_trafo && trafo) {
          xs = map(xs, function(x) ps$trafo(x, ps))
        }
        return(xs)
      }

      if (self$param_set$values$random_interleaving && ((archive$n_batch %% 2) == 1)) {
        xdt = generate_design_random(acq_function$domain, 1000L)  # 1000 randoms
        txdt = if (self$param_set$values$trafo) {
          as.data.table(do.call(rbind, xdt$transpose()))
        } else {
          xdt$data
        }
        ydt = acq_function$eval_dt(char_to_fct(txdt, ps =acq_function$domain)) * mult_max_to_min(acq_function$codomain)  # FIXME: ps = acq_function$domain?
        best = which(ydt[[1L]] == min(ydt[[1L]]))
        if (length(best) > 1L) {
          best = sample(best, 1L)
        }
        if (self$param_set$values$return_acqv) {
          return(list(xdt$data[best, ], ydt[best]))
        } else {
          return(xdt$data[best, ])
        }
      }

      all_data = archive$data[, archive$cols_x, with = FALSE]
      data = archive$best()[, archive$cols_x, with = FALSE]
      params_to_consider = if (self$param_set$values$consider_all) {
         seq_len(NCOL(data))
      } else {
        which(!bbotk::is_dominated(t(map_dtc(acq_function$surrogate$model, function(surrogate) ranger::importance(surrogate$state$model$regr.ranger$model)))))
      }
      if (!length(params_to_consider)) params_to_consider = seq_len(NCOL(data))
      xdt = if (self$param_set$values$operation == "mutation") {
        map_dtr(seq_len(self$param_set$values$n), .f = function(x) mutate(data, acq_function = acq_function, params_to_consider = params_to_consider))
      } else {
        map_dtr(seq_len(self$param_set$values$n), .f = function(x) mutate_crossover(data, acq_function, params_to_consider = params_to_consider))
      }
      xdt = unique(xdt)

      check = apply(xdt, MARGIN = 1L, FUN = function(x) NROW(merge(as.data.table(as.list(x)), all_data)) == 0L)
      xdt = xdt[check, ]

      if (NROW(xdt) == 0L) {
        return(generate_design_random(acq_function$domain, n = 1L)$data)
      }

      txdt = if (self$param_set$values$trafo) {
        as.data.table(do.call(rbind, transpose(xdt, acq_function$domain)))
      } else {
        xdt
      }

      ydt = acq_function$eval_dt(char_to_fct(txdt, ps = acq_function$domain)) * mult_max_to_min(acq_function$codomain)  # FIXME: ps = acq_function$domain?
      best = which(ydt[[1L]] == min(ydt[[1L]]))
      if (length(best) > 1L) {
        best = sample(best, 1L)
      }
      if (self$param_set$values$return_acqv) {
        list(xdt[best, ], ydt[best])
      } else {
        xdt[best, ]
      }
    }
))

mutate = function(data, acq_function, params_to_consider) {
  # uniform mutation
  # mutate params that are important in the surrogates
  n = NROW(data)
  # FIXME: we could also specify the niche here
  if (n > 1L) data = data[sample(n, size = 1L), ]  # sample if more than a single row
  ind = sample(intersect(seq_len(NCOL(data)), params_to_consider), size = 1L)
  qunif = runif(1L, min = 0, max = 1)
  data[[ind]] = acq_function$domain$params[[colnames(data)[ind]]]$qunif(qunif)

  # params that are NA need their default here
  data = setDT(imap(data, .f = function(value, name) {
    if (is.na(value)) {
      acq_function$domain$params[[name]]$default
    } else {
      value
    }
  }))

  for (i in seq_len(NROW(acq_function$domain$deps))) {
    dep = acq_function$domain$deps[i, ]

    if (any(map_lgl(dep[["cond"]], .f = function(cond) cond$test(data[[dep[["on"]]]])) == FALSE)) {
      data[[dep[["id"]]]] = switch(acq_function$domain$storage_type[[dep[["id"]]]], "integer" = NA_integer_, "double" = NA_real_, "character" = NA_character_)
    }
  }

  data
}

mutate_crossover = function(data, acq_function, params_to_consider) {
  # uniform mutation
  # mutate params that are important in the surrogates
  n = NROW(data)
  p = NCOL(data)
  rows = seq_len(n)
  cols = seq_len(p)

  mut_ind = mlr3misc::shuffle(intersect(cols, params_to_consider), n = 1L)
  qunif = runif(1L, min = 0, max = 1)
  data[[mut_ind]] = acq_function$domain$params[[colnames(data)[mut_ind]]]$qunif(qunif)

  # crossover
  # crossover params that are important in the surrogates and not mutated
  cross_inds = integer()
  while(NROW(data) > 1L) {
    pairs = split(seq_len(NROW(data)), ceiling(seq_len(NROW(data)) / 2L))
    candidates = setdiff(intersect(cols, params_to_consider), c(mut_ind, cross_inds))
    if (length(candidates) == 0L) candidates = setdiff(cols, c(mut_ind, cross_inds))
    ind = mlr3misc::shuffle(candidates, n = 1L)
    cross_inds = c(cross_inds, ind)
    for (pair in pairs) {
      if (length(pair) > 1L) {
        if (runif(1L, min = 0, max = 1) > 0.5) {
          data[pair[1L], ][[ind]] = data[pair[2L], ][[ind]]
        }
      }
    }
    data = data[map_int(pairs, 1L), ]
  }

  # params that are NA need their default here
  data = setDT(imap(data, .f = function(value, name) {
    if (is.na(value)) {
      acq_function$domain$params[[name]]$default
    } else {
      value
    }
  }))

  for (i in seq_len(NROW(acq_function$domain$deps))) {
    dep = acq_function$domain$deps[i, ]

    if (any(map_lgl(dep[["cond"]], .f = function(cond) cond$test(data[[dep[["on"]]]])) == FALSE)) {
      data[[dep[["id"]]]] = switch(acq_function$domain$storage_type[[dep[["id"]]]], "integer" = NA_integer_, "double" = NA_real_, "character" = NA_character_)
    }
  }

  data
}



#' @title Acquisition Optimizer Mies
#'
#' @description
#'
#' @export
AcqOptimizerMies = R6Class("AcqOptimizerMies",
  inherit = AcqOptimizer_old,

  public = list(

    #' @field param_set ([paradox::ParamSet]).
    param_set = NULL,

    optimizer = NULL,
    terminator = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(optimizer, terminator) {
      self$param_set = ParamSet$new(list(
        ParamLgl$new("trafo"),
        ParamLgl$new("return_acqv"))
      )
      self$param_set$values = list(trafo = FALSE, return_acqv = FALSE)

      self$optimizer = optimizer
      self$terminator = terminator
    },

    #' @description
    #' Optimize the acquisition function.
    #'
    #' @param acq_function [AcqFunction].
    #' @param archive [bbotk::Archive].
    optimize = function(acq_function, archive) {
      # FIXME: cannot handle trafo

      instance = OptimInstanceSingleCrit$new(objective = acq_function, terminator = self$terminator, check_values = FALSE) # FIXME: check values

      incumbent = archive$best()[, archive$cols_x, with = FALSE]
      instance$eval_batch(char_to_fct(incumbent, ps = instance$search_space))  # warm start

      self$optimizer$optimize(instance)
      xdt_data = instance$archive$data[, instance$archive$cols_x, with = FALSE]
      xdt_data[["_id"]] = seq_len(NROW(xdt_data))
      ids = merge(xdt_data, incumbent, keep.rownames = FALSE)[["_id"]]

      instance$archive$data[[acq_function$codomain$ids()]][ids] = 0  # do not propose incumbent

      xdt = fct_to_chr(instance$archive$best()[, instance$archive$cols_x, with = FALSE])

      if (self$param_set$values$return_acqv) {
        list(xdt, - instance$archive$best()[, instance$archive$cols_y, with = FALSE]) # FIXME: all other also return the negative
      } else {
        xdt
      }
    }
))

fct_to_chr = function(xydt) {
  xydt_ = copy(xydt)
  # Convert character params to factors
  fct_cols = names(xydt_)[map_chr(xydt_, class) == "factor"]
  if (length(fct_cols)) {
    xydt_[, (fct_cols) := imap(.SD, function(x, id) {
        as.character(x)
    }), .SDcols = fct_cols]
  }
  return(xydt_)
}

