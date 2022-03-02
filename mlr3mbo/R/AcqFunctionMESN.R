#' @title Acquisition Function Max-value Entropy Search for QDO
#'
#' @include AcqFunction.R
#' @name mlr_acqfunctions_mesn
#'
#' @templateVar id mesn
#' @template section_dictionary_acqfunctions
#'
#' @description
#' Max-value Entropy Search for QDO.
#'
#' @family Acquisition Function
#' @export
# FIXME: check that surrogate_max_to_min does what it is supposed to
# FIXME: rewrite for efficieny
# FIXME: MAP-Elites instead of Gumbel sampling for maxesn
# FIXME: full information theoretic
AcqFunctionMESN = R6Class("AcqFunctionMESN",
  inherit = AcqFunction,
  public = list(
    #' @field niches ([Niches]).
    niches = NULL,

    #' @field full_info (`logical(1)`).
    full_info = NULL,

    #' @field grid [data.table::data.table]
    grid = NULL,

    #' @field maxesn (`list()`).
    maxesn = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateLearners]).
    #' @param niches ([bbotk::Niches]).
    initialize = function(surrogate, niches, full_info = FALSE) {
      assert_r6(surrogate, "SurrogateLearners")
      assert_r6(niches, "Niches")
      assert_flag(full_info)
      self$niches = niches
      self$full_info = full_info  # FIXME: constant

      super$initialize("acq_mesn", surrogate = surrogate, direction = "maximize")   
    },   

    #' @description
    #' Updates acquisition function and sets `maxes`.
    update = function() {
      super$update()
      self$maxesn = get_maxesn(x = self$archive$data[, self$cols_x, with = FALSE], grid = self$grid, surrogate = self$surrogate, surrogate_max_to_min = self$surrogate_max_to_min, cols_y = self$cols_y, cols_g = self$cols_g, niches = self$niches)
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
      if (is.null(self$maxesn)) {
        stop("maxesn is not set. Missed to call $update()?")
      }
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
      if (self$full_info) {
        maxesn = self$maxesn
        nmax = max(map_int(maxesn, length))
        maxesn = map(maxesn, function(maxes) {
          n = length(maxes)
          c(maxes, rep(NA_real_, nmax - n))
        })
        mesn = map_dbl(seq_len(NROW(p[[self$cols_y]])), function(i) {
          mu = p[[self$cols_y]]$mean[i]
          se = p[[self$cols_y]]$se[i]
          if (se < sqrt(.Machine$double.eps)) se = sqrt(.Machine$double.eps)  #  NOTE: this prevents exploding values
          pj = as.matrix(map_dbl(prob_j, i))
          hy = ((1 / 2) * log(2 * pi * (se ^ 2))) + (1 / 2)
          tmp = map_dbl(seq_len(nmax), function(j) {
            maxes = map_dbl(maxesn, j)
            hys_fun = function(y) {
              num = dnorm((y - (- self$surrogate_max_to_min[[self$cols_y]] * mu)) / se)
              denum = pnorm((maxes - (- self$surrogate_max_to_min[[self$cols_y]] * mu)) / se)
              denum[!is.na(denum) & denum < sqrt(.Machine$double.eps)] = sqrt(.Machine$double.eps)  # NOTE: this prevents exploding values
              frac = (1 / se) * outer(num, denum, "/")
              frac[is.na(frac)] = 0
              #tmp = rowSums(matrix(pj, nrow = length(y), ncol = length(pj), byrow = TRUE) * frac, na.rm = TRUE)
              tmp = as.vector(frac %*% pj)
              tmp[tmp < .Machine$double.eps] = .Machine$double.eps   # NOTE: this prevents exploding values
              tmp = tmp * log(tmp)
              tmp
            }
            # FIXME: add truncnorm
            tnquants = map(seq_along(maxes), function(j) truncnorm::qtruncnorm(c(0.001, 0.999), a = -Inf, b = maxes[j], mean = - self$surrogate_max_to_min[[self$cols_y]] * mu, sd = se))
            lower = min(map_dbl(tnquants, 1L), na.rm = TRUE)
            upper = max(map_dbl(tnquants, 2L), na.rm = TRUE)
            hys = tryCatch(- {integrate(hys_fun, lower = lower, upper = upper)$value},
              error = function(error_condition) NA_real_
            )
            #hys_fun_j = function(y, j) {
            #  num = dnorm((y - (- self$surrogate_max_to_min[[self$cols_y]] * mu)) / se)
            #  denum = pnorm((maxes[j] - (- self$surrogate_max_to_min[[self$cols_y]] * mu)) / se)
            #  frac = (1 / se) * (num / denum)
            #  tmp = frac * log(frac)
            #  tmp[is.na(tmp)] = 0
            #  tmp
            #}
            #hys_j = map_dbl(seq_along(maxes), function(j) {
            #  tryCatch(- {integrate(hys_fun_j, lower = -Inf, upper = Inf, j = j)$value},error = function(error_condition) NA_real_)
            #})
            #sum(pj * hys_j, na.rm = TRUE) <= hys
          })
          ehys = mean(tmp, na.rm = TRUE)
          hy - ehys
        })
      } else {
        mesn = map_dbl(seq_len(NROW(p[[self$cols_y]])), function(i) {
          mu = p[[self$cols_y]]$mean[i]
          se = p[[self$cols_y]]$se[i]
          if (se < sqrt(.Machine$double.eps)) se = sqrt(.Machine$double.eps)  #  NOTE: this prevents exploding values
          sum(map_dbl(self$maxesn, function(maxes) {
            if (is.null(maxes)) return(0)  # early exit
            gamma = (maxes - (- self$surrogate_max_to_min[[self$cols_y]] * mu)) / se
            p_gamma = pnorm(gamma)
            p_gamma[p_gamma < sqrt(.Machine$double.eps)] = sqrt(.Machine$double.eps)  # NOTE: check
            mean(((gamma * dnorm(gamma)) / (2 * p_gamma)) - log(p_gamma), na.rm = TRUE)
          }) * map_dbl(prob_j, i))
        })
      }
      data.table(acq_mesn = mesn)
    }
  )
)



# FIXME: AcqFunction ParamSet with at least gridsize and nk
# NOTE: nK is mostly costly for full_info
get_maxesn = function(nK = 100L, x, grid, surrogate, surrogate_max_to_min, cols_y, cols_g, niches) {
  xgrid = rbind(grid, x)
  p = surrogate$predict(xgrid)
  mu = p[[cols_y]]$mean
  se = p[[cols_y]]$se
  se[se < sqrt(.Machine$double.eps)] = sqrt(.Machine$double.eps)  # FIXME:

  # FIXME: also used in ejie --> own function
  prob_j = {
    p_g = p[cols_g]
    mu_g = if (is.list(p_g) & !is.data.table(p_g)) map(p_g, "mean") else setNames(list(p_g$mean), nm = cols_g)
    se_g = if (is.list(p_g) & !is.data.table(p_g)) map(p_g, "se") else setNames(list(p_g$se), nm = cols_g)

    if (test_r6(niches, classes = "NichesBoundaries")) {
      map(names(niches$niches), function(niche) {
        boundaries = niches$niches[[niche]]$niche_boundaries
        pj = 1
        for (id in cols_g) {
          pj = pj * (pnorm((mu_g[[id]] - boundaries[[id]][1L]) / se_g[[id]]) - pnorm((mu_g[[id]] - boundaries[[id]][2L]) / se_g[[id]]))
        }
        pj
      })
    }
  }
  names(prob_j) = names(niches$niches)
  setDT(prob_j)
  argmax_prob_j = c(names(prob_j), NA_character_)[apply(prob_j, MARGIN = 1L, FUN = function(x) {
    tmp = which(x > 0.95)
    if (length(tmp) == 0L) length(x) + 1L else tmp
  })]

  setNames(map(names(niches$niches), function(niche) {

    ids = which(argmax_prob_j == niche)
    if (length(ids) == 0L) {
      return(NULL)  # early exit
    }

    # assuming maximization
    mu_max = max(- surrogate_max_to_min[[cols_y]] * mu[ids])

    left = mu_max
    leftprob = probf(left, mu = mu[ids], se = se[ids], surrogate_max_to_min = surrogate_max_to_min[[cols_y]])
    # FIXME:
    ntry = 0L
    while (leftprob > 0.10 && ntry < 3L) {
      left = if (left > 0.01) left / 2 else 2 * left - 0.05
      leftprob = probf(left, mu = mu[ids], se = se[ids], surrogate_max_to_min = surrogate_max_to_min[[cols_y]])
      ntry = ntry + 1L
    }

    ntry = 0L
    right = max(- surrogate_max_to_min[[cols_y]] * (mu[ids] - (surrogate_max_to_min[[cols_y]] * 5 * se[ids])))  # NOTE: check
    rightprob = probf(right, mu = mu[ids], se = se[ids], surrogate_max_to_min = surrogate_max_to_min[[cols_y]])
    while (rightprob < 0.90 && ntry < 3L) {
      right = right + right - left
      rightprob = probf(right, mu = mu[ids], se = se[ids], surrogate_max_to_min = surrogate_max_to_min[[cols_y]])
      ntry = ntry + 1L
    }
    stopifnot(left < right)

    if (leftprob > 0.10 || rightprob < 0.90) {
      return(mu_max + runif(nK, min = 0, max = 1))  # NOTE: check
    }

    mgrid = seq(from = left, to = right, length.out = 100L)

    prob = map_dbl(mgrid, function(mg) {
      prod(pnorm((mg - (- surrogate_max_to_min[[cols_y]] * mu[ids])) / se[ids]))
    })

    # FIXME: check this
    if (sum(prob > 0.05 & prob < 0.95) == 0L) {
      return(mu_max + runif(nK, min = 0, max = 1))
    }

    # Gumbel sampling
    q1 = stats::optimize(function(x) abs(probf(x, mu = mu[ids], se = se[ids], surrogate_max_to_min = surrogate_max_to_min[[cols_y]]) - 0.25), interval = range(mgrid))$minimum
    q2 = stats::optimize(function(x) abs(probf(x, mu = mu[ids], se = se[ids], surrogate_max_to_min = surrogate_max_to_min[[cols_y]]) - 0.5), interval = range(mgrid))$minimum
    q3 = stats::optimize(function(x) abs(probf(x, mu = mu[ids], se = se[ids], surrogate_max_to_min = surrogate_max_to_min[[cols_y]]) - 0.75), interval = range(mgrid))$minimum
    beta = (q1 - q3) / (log(log(4 / 3)) - log(log(4)))  # FIXME: assert beta > 0
    alpha = q2 + beta * log(log(2))

    -log(-log(runif(nK, min = 0, max = 1))) * beta + alpha
    # FIXME: maxes that are <= mu_max + eps should be replaced by mu_max + eps
  }), nm = names(niches$niches))
}

probf = function(mu_, mu, se, surrogate_max_to_min) {
  # assuming maximization
  prod(pnorm((mu_ - (- surrogate_max_to_min * mu)) / se))
}

get_maxesn_sample = function(nk = 100L, x, grid, surrogate, surrogate_max_to_min, cols_y, cols_g, niches) {
  # sample from posterior
  # FIXME: must be GP
  # FIXME: currently only works for y and g
  # FIXME: assumes that surrogate_max_to_min for g is 1
  xgrid = rbind(grid, x)

  # calculate prob in niches for xgrid and resample the promising points for each niche
  p = surrogate$predict(xgrid)
  prob_j = {
    p_g = p[cols_g]
    mu_g = if (is.list(p_g) & !is.data.table(p_g)) map(p_g, "mean") else setNames(list(p_g$mean), nm = cols_g)
    se_g = if (is.list(p_g) & !is.data.table(p_g)) map(p_g, "se") else setNames(list(p_g$se), nm = cols_g)
  
    if (test_r6(niches, classes = "NichesBoundaries")) {
      map(names(niches$niches), function(niche) {
        boundaries = niches$niches[[niche]]$niche_boundaries
        pj = 1
        for (id in cols_g) {
          pj = pj * (pnorm((mu_g[[id]] - boundaries[[id]][1L]) / se_g[[id]]) - pnorm((mu_g[[id]] - boundaries[[id]][2L]) / se_g[[id]]))
        }
        pj
      })
    }
  }
  names(prob_j) = names(niches$niches)
  setDT(prob_j)

  xgrid_to_select = unique(as.vector(apply(prob_j, 2, order)[1:(min(nrow(prob_j), 1000)), ]))
  xgrid = xgrid[xgrid_to_select, ]

  post_y = predict(surrogate$model[[cols_y]]$model, newdata = xgrid, type = "SK", cov.compute = TRUE)
  mu_y = post_y$mean
  sigma_y = post_y$cov
  # FIXME: mvtnorm
  sample_y = mvtnorm::rmvnorm(nk, mean = mu_y, sigma = sigma_y)

  post_g = predict(surrogate$model[[cols_g]]$model, newdata = xgrid, type = "SK", cov.compute = TRUE)
  mu_g = post_g$mean
  sigma_g = post_g$cov
  sample_g = mvtnorm::rmvnorm(nk, mean = mu_g, sigma = sigma_g)

  niches_ids = map_chr(niches$niches, "id")
  maxesn = replicate(length(niches_ids), expr = rep(NA_real_, length = nk), simplify = FALSE)
  names(maxesn) = niches_ids

  for (i in seq_len(nk)) {
    tmp = data.table(y = sample_y[i, ], g = sample_g[i, ])
    tmp[, niche := niches$get_niche_dt(tmp[, "g"])]

    tmp$orig = seq_len(NROW(tmp))
    tmp = tmp[, lapply(.SD, unlist), by = orig]
    tmp[, orig := NULL]

    niche = unique(tmp[["niche"]])
    consider = tmp[["niche"]] %in% niche
    tab = tmp[consider, ]
    tab[, g := NULL]
    setorderv(tab, cols_y, order = surrogate_max_to_min[[cols_y]], na.last = TRUE)
    res = unique(tab, by = "niche")
    for (niche_ in res$niche) {
      maxesn[[niche_]][i] = surrogate_max_to_min[[cols_y]] * res[niche == niche_, ]$y
    }
  }

  maxesn
}

mlr_acqfunctions$add("mesn", AcqFunctionMESN)

