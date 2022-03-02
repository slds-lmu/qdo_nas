#' @title Bayesian Optimization of Elites
#' 
#' @description
#' FIXME:
#'
#' @param instance ([bbotk::OptimInstanceQDOSingleCrit])\cr
#'   The [bbotk::OptimInstanceQDOSingleCrit] to be optimized.
#' @param init_design_size (`NULL` | `integer(1)`)\cr
#'   Size of the initial design.
#'   If `NULL` \code{4 * d} is used with \code{d} being the dimensionality of the search space.
#'   Points are drawn uniformly at random.
#' @param acq_function ([AcqFunction]).\cr
#'   [AcqFunction] to be used as acquisition function.
#' @param acq_optimizer ([AcqOptimizer])\cr
#'   [AcqOptimizer] to be used as acquisition function optimizer.
#' @param sampler (`NULL` | any)\cr
#'   Any optional sampler holding a `$sample()` function.
#'
#' @return invisible(instance)\cr
#'   The original instance is modified in-place and returned invisible.
#'
#' @family Loop Function
#' @export
bayesopt_bop = function(
  instance,
  init_design_size = NULL,
  acq_function,
  acq_optimizer,
  sampler = NULL) {

  # assertions and defaults
  assert_r6(instance, "OptimInstanceQDOSingleCrit")
  assert_int(init_design_size, lower = 1L, null.ok = TRUE)
  assert_r6(acq_function, classes = "AcqFunction")
  assert_r6(acq_optimizer, classes = "AcqOptimizer")
  sampler = sampler %??% SamplerUnif$new(instance$search_space)

  archive = instance$archive
  domain = instance$search_space
  d = domain$length
  if (is.null(init_design_size) && instance$archive$n_evals == 0L) init_design_size = 4 * d
  acq_optimizer$acq_function = acq_function

  # initial design
  if (isTRUE(init_design_size > 0L)) {
    design = generate_design_random(domain, n = init_design_size)$data
    instance$eval_batch(design)
  }

  acq_function_orig = acq_function
  # FIXME: here we could interleave with a different acqf, e.g. FUCT oder pent
  acq_function_fuct = acq_function_orig
  #acq_function_fuct = AcqFunctionFUCT$new(surrogate = acq_function_orig$surrogate)

  # loop
  repeat {
    xdt = tryCatch({
      if (instance$archive$n_evals %% 5L == 0L) {
        acq_function = acq_function_fuct
      } else {
        acq_function = acq_function_orig
      }
      acq_function$surrogate$update()
      acq_function$update()
      acq_optimizer$acq_function = acq_function
      acq_optimizer$optimize()
    }, mbo_error = function(mbo_error_condition) {
      lg$info("Proposing a randomly sampled point")
      sampler$sample(1L)$data
    })

    instance$eval_batch(xdt)
    if (instance$is_terminated) break
  }

  return(invisible(instance))
}

if (FALSE) {
  # single feature function example
  set.seed(1)
  devtools::load_all("../bbotk")
  devtools::load_all()
  library(paradox)
  library(mlr3learners)
  library(ggplot2)

  nb1 = NicheBoundaries$new("niche1", niche_boundaries = list(g = c(-Inf, 1)))
  nb2 = NicheBoundaries$new("niche2", niche_boundaries = list(g = c(1, 2)))
  nb3 = NicheBoundaries$new("niche3", niche_boundaries = list(g = c(2, 3)))
  nb4 = NicheBoundaries$new("niche4", niche_boundaries = list(g = c(3, 4)))
  nb5 = NicheBoundaries$new("niche5", niche_boundaries = list(g = c(4, 5)))
  nb6 = NicheBoundaries$new("niche6", niche_boundaries = list(g = c(5, 6)))
  nb7 = NicheBoundaries$new("niche7", niche_boundaries = list(g = c(6, 7)))
  nb8 = NicheBoundaries$new("niche8", niche_boundaries = list(g = c(7, 8)))
  nb9 = NicheBoundaries$new("niche9", niche_boundaries = list(g = c(8, 9)))
  nb10 = NicheBoundaries$new("niche10", niche_boundaries = list(g = c(9, 10)))
  nb11 = NicheBoundaries$new("niche11", niche_boundaries = list(g = c(10, 11)))
  nb12 = NicheBoundaries$new("niche12", niche_boundaries = list(g = c(11, 12)))
  nb13 = NicheBoundaries$new("niche13", niche_boundaries = list(g = c(12, 13)))
  nb14 = NicheBoundaries$new("niche14", niche_boundaries = list(g = c(13, 14)))
  nb15 = NicheBoundaries$new("niche15", niche_boundaries = list(g = c(14, 15)))
  nb16 = NicheBoundaries$new("niche16", niche_boundaries = list(g = c(15, 16)))
  nb17 = NicheBoundaries$new("niche17", niche_boundaries = list(g = c(16, 17)))
  nb18 = NicheBoundaries$new("niche18", niche_boundaries = list(g = c(17, 18)))
  nb19 = NicheBoundaries$new("niche19", niche_boundaries = list(g = c(18, 19)))
  nb20 = NicheBoundaries$new("niche20", niche_boundaries = list(g = c(19, Inf)))

  nb = NichesBoundaries$new("test",
    niches_boundaries = list(niche1 = nb1, niche2 = nb2, niche3 = nb3, niche4 = nb4, niche5 = nb5, niche6 = nb6, niche7 = nb7, niche8 = nb8, niche9 = nb9, niche10 = nb10,
                             niche11 = nb11, niche12 = nb12, niche13 = nb13, niche14 = nb14, niche15 = nb15, niche16 = nb16, niche17 = nb17, niche18 = nb18, niche19 = nb19, niche20 = nb20))

  set.seed(1)
  true_dat = data.table(x1 = 0:10, y = runif(n = 11L, min = 0, max = 20), g = runif(n = 11L, min = 0, max = 20))
  task_y = TaskRegr$new("y", backend = true_dat[, -"g"], target = "y")
  task_g = TaskRegr$new("g", backend = true_dat[, -"y"], target = "g")

  learner = lrn("regr.km", covtype = "gauss", lower = rep(0.5, 1), upper = rep(2, 1), optim.method = "BFGS", multistart = 10L, nugget.stability = 1e-8, control = list(trace = FALSE))
  true_learner_y = learner$clone(deep = TRUE)
  true_learner_y$train(task_y)
  true_learner_g = learner$clone(deep = TRUE)
  true_learner_g$train(task_g)

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = data.table(
        y = true_learner_y$predict_newdata(xdt)$response,
        g = true_learner_g$predict_newdata(xdt)$response
      )
      tmp[, niche := nb$get_niche_dt(tmp[, "g"])]
      tmp
    },
    domain = ParamSet$new(list(ParamDbl$new("x1", 0, 10))),
    codomain = ParamSet$new(list(
      ParamDbl$new("y", tags = "minimize"),
      ParamDbl$new("g", tags = "feature"),
      ParamFct$new("niche", levels = paste0("niche", 1:20), special_vals = list(NA_character_), tags = "niche"))
    ),
    properties = "single-crit",
    id = "test"
  )

  instance_all = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    terminator = trm("none")
  )
  opt("grid_search", resolution = 1001L, batch_size = 1001L)$optimize(instance_all)
  best_all = instance_all$archive$best()

  terminator = trm("evals", n_evals = 20L)

  do_loop = function(acqf, seed) {
    instance = OptimInstanceQDOSingleCrit$new(
      objective = obfun,
      terminator = terminator
    )
    set.seed(seed)
    design = generate_design_random(instance$search_space, n = 8L)$data
    instance$eval_batch(design)

    surrogate = default_surrogate(instance, learner = learner, n_learner = 2L)
    surrogate$y_cols = c("y", "g")
    surrogate$archive = instance$archive

    ejie = AcqFunctionEJIE$new(surrogate, niches = nb)
    mesn = AcqFunctionMESN$new(surrogate, niches = nb)
    mesn_full = AcqFunctionMESN$new(surrogate, niches = nb, full_info = TRUE)
    mesn$grid = generate_design_grid(mesn$domain, resolution = 101L)$data
    mesn_full$grid = mesn$grid

    acq_function = switch(acqf, ejie = ejie, mesn = mesn, mesn_full = mesn_full)

    acq_optimizer = AcqOptimizer$new(opt("grid_search", resolution = 1001L, batch_size = 1001L), terminator = trm("none"))

    bayesopt_bop(instance, acq_function = acq_function, acq_optimizer = acq_optimizer)
    map_dtr(seq_len(instance$archive$n_batch), function(b) {
      best = instance$archive$best(batch = 1:b)
      regret = - sum(map_dbl(unique(best_all$niche), function(n) {
        best_all[niche == n]$y - min(max(instance_all$archive$data$y), best[niche == n]$y)
      }))
      regret_found = - sum(map_dbl(unique(best$niche), function(n) {
        best_all[niche == n]$y - min(max(instance_all$archive$data$y), best[niche == n]$y)
      }))
      data.table(regret = regret, regret_found = regret_found, b = b, best = list(best))
    })
  }

  do_random = function(seed) {
    instance = OptimInstanceQDOSingleCrit$new(
      objective = obfun,
      terminator = terminator
    )
    set.seed(seed)
    design = generate_design_random(instance$search_space, n = 8L)$data
    instance$eval_batch(design)
    opt("random_search")$optimize(instance)

    map_dtr(seq_len(instance$archive$n_batch), function(b) {
      best = instance$archive$best(batch = 1:b)
      regret = - sum(map_dbl(unique(best_all$niche), function(n) {
        best_all[niche == n]$y - min(max(instance_all$archive$data$y), best[niche == n]$y)
      }))
      regret_found = - sum(map_dbl(unique(best$niche), function(n) {
        best_all[niche == n]$y - min(max(instance_all$archive$data$y), best[niche == n]$y)
      }))
      data.table(regret = regret, regret_found = regret_found, b = b, best = list(best))
    })
  }

  library(future.apply)
  plan(multicore, workers = 8)

  #res = future_lapply(1:8, function(i) {
  res = vector("list", length = 100L)
  for (i in 1:100) {
    set.seed(i)
    true_dat = data.table(x1 = 0:10, y = runif(n = 11L, min = 0, max = 20), g = runif(n = 11L, min = 0, max = 20))
    task_y = TaskRegr$new("y", backend = true_dat[, -"g"], target = "y")
    task_g = TaskRegr$new("g", backend = true_dat[, -"y"], target = "g")

    learner = lrn("regr.km", covtype = "gauss", lower = rep(0.5, 1), upper = rep(2, 1), optim.method = "BFGS", multistart = 10L, nugget.stability = 1e-8, control = list(trace = FALSE))
    true_learner_y = learner$clone(deep = TRUE)
    true_learner_y$train(task_y)
    true_learner_g = learner$clone(deep = TRUE)
    true_learner_g$train(task_g)

    obfun = ObjectiveRFunDt$new(
      fun = function(xdt) {
        tmp = data.table(
          y = true_learner_y$predict_newdata(xdt)$response,
          g = true_learner_g$predict_newdata(xdt)$response
        )
        tmp[, niche := nb$get_niche_dt(tmp[, "g"])]
        tmp
      },
      domain = ParamSet$new(list(ParamDbl$new("x1", 0, 10))),
      codomain = ParamSet$new(list(
        ParamDbl$new("y", tags = "minimize"),
        ParamDbl$new("g", tags = "feature"),
        ParamFct$new("niche", levels = paste0("niche", 1:20), special_vals = list(NA_character_), tags = "niche"))
      ),
      properties = "single-crit",
      id = "test"
    )

    instance_all = OptimInstanceQDOSingleCrit$new(
      objective = obfun,
      terminator = trm("none")
    )
    opt("grid_search", resolution = 1001L, batch_size = 1001L)$optimize(instance_all)
    best_all = instance_all$archive$best()

    terminator = trm("evals", n_evals = 20L)

    res[[i]] = list(i = i, ejie = do_loop("ejie", seed = i), mesn = do_loop("mesn", seed = i), mesn_full = do_loop("mesn_full", seed = i), rs = do_random(seed = i))
  }

  dat = map_dtr(res, function(x) {
    if (is.null(x)) return(NULL)
    ejie = x$ejie
    ejie[, method := "ejie"]
    ejie[, n_found := map_int(ejie$best, function(y) length(unique(y$niche)))]
    mesn = x$mesn
    mesn[, method := "mesn"]
    mesn[, n_found := map_int(mesn$best, function(y) length(unique(y$niche)))]
    mesn_full = x$mesn_full
    mesn_full[, method := "mesn_full"]
    mesn_full[, n_found := map_int(mesn_full$best, function(y) length(unique(y$niche)))]
    rs = x$rs
    rs[, method := "rs"]
    rs[, n_found := map_int(rs$best, function(y) length(unique(y$niche)))]
    tmp = rbind(ejie, mesn, mesn_full, rs)
    tmp[, i := x$i]
  })

  agg = dat[, .(mreg = mean(regret), sereg = sd(regret) / sqrt(.N), mregfound = mean(regret_found), seregfound = sd(regret_found) / sqrt(.N), mnfound = mean(n_found), senfound = sd(n_found) / sqrt(.N)), by = .(method, b)]

  g1 = ggplot(aes(x = b, y = mreg, colour = method), data = agg) +
    geom_line() +
    geom_ribbon(aes(min = mreg - sereg, max = mreg + sereg, fill = method), colour = NA, alpha = 0.25) +
    xlab("Number of function evaluation") +
    ylab("Regret") +
    theme_minimal()

  g2 = ggplot(aes(x = b, y = mregfound, colour = method), data = agg) +
    geom_line() +
    geom_ribbon(aes(min = mregfound - seregfound, max = mregfound + seregfound, fill = method), colour = NA, alpha = 0.25) +
    xlab("Number of function evaluation") +
    ylab("Regret for occpuied Niches") +
    theme_minimal()

  g3 = ggplot(aes(x = b, y = mnfound, colour = method), data = agg) +
    geom_line() +
    geom_ribbon(aes(min = mnfound - senfound, max = mnfound + senfound, fill = method), colour = NA, alpha = 0.25) +
    xlab("Number of function evaluation") +
    ylab("Number of Niches occupied") +
    theme_minimal()

  # NOTE: indeed: mes finds better solutions than ejie but occupies less niches (if n_evals is less then n_niches)
}

