if (FALSE) {
  # ROC example
  set.seed(1)
  devtools::load_all("../bbotk")
  devtools::load_all()
  library(paradox)
  library(mlr3pipelines)
  library(mlr3learners)
  library(mlr3tuning)
  library(mlr3viz)
  library(precrec)

  lgr::get_logger("mlr3")$set_threshold("warn")

  task = tsk("sonar")
  learner = lrn("classif.ranger", predict_type = "prob")
  resampling = rsmp("cv", folds = 10L)
  measure = msr("classif.auc")

  # FIXME: needs all Tuning classes later
  #obfun = ObjectiveTuning$new(task = task, learner = learner,
  #  resampling = resampling, measures = list(measure),
  #  store_benchmark_result = TRUE,
  #  store_models = TRUE, check_values = TRUE)

  domain = ParamSet$new(list(
    ParamInt$new("mtry", lower = 1L, upper = 60L),
    ParamInt$new("num.trees", lower = 1L, upper = 1000L)))

  #refinstance = TuningInstanceSingleCrit$new(task, learner, resampling, measure, domain, trm("evals", n_evals = 20))
  #tnr("random_search")$optimize(refinstance) # 5, 401, 0.9380148

  obfun = ObjectiveRFun$new(
    fun = function(xs) {
      learner$param_set$values = xs
      rr = resample(task, learner, resampling)
      rr$aggregate(measure)
    },
    domain = domain,
    codomain = ParamSet$new(list(ParamDbl$new("y", tags = "maximize"))),
    id = "y"
  )

  # FIXME: Need an ObjectiveFeature to be more liberal
  ffun = ObjectiveRFun$new(
    fun = function(xs) {
      learner$param_set$values = xs
      rr = resample(task, learner, resampling)
      mod = evalmod(as_precrec(rr), raw_curves = FALSE)
      dmod = as.data.table(mod)[type == "ROC", c("x", "y")]
      #plot(mod, "ROC")
      data.table(roc = list(gx = dmod[["x"]], gy = dmod[["y"]]))
    },
    domain = domain,
    codomain = ParamSet$new(list(ParamDbl$new("roc", tags = "minimize"))),
    id = "groc",
    check_values = FALSE
  )

  e1 = Ellipsoid2D$new("e1", center = c(0., 0.7), radii = c(0.025, 0.025))
  #e2 = Ellipsoid2D$new("e2", center = c(0.4, 0.8), radii = c(0.05, 0.05))
  e3 = Ellipsoid2D$new("e3", center = c(0.3, 1), radii = c(0.025, 0.025))
  #e4 = Ellipsoid2D$new("e4", center = c(0.4, 0.9), radii = c(0.05, 0.1))

  niches = NichesROC$new("test", ellipsoids = list(niche1 = list("e1" = e1), niche2 = list("e3" = e3)))

  surrogate = lrn("regr.ranger")

  ftfun = Feature$new("test", ffun, niches, NULL)

  terminator = trm("evals", n_evals = 1)

  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    feature = ftfun,
    terminator = terminator
  )

  acq_function = AcqFunctionEJIE$new(SurrogateSingleCritLearner$new(surrogate$clone(deep = TRUE)))
  acq_optimizer = AcqOptimizerRandomSearch$new()
  acq_optimizer$param_set$values$iters = 100
  #n_design = 4 * instance$search_space$length

  bayesop_bop(instance, acq_function, acq_optimizer)

  par(mfrow = c(1, 2))
  learner$param_set$values = list(mtry = 18, num.trees = 896)
  rr = resample(task, learner, resampling)
  plot(evalmod(as_precrec(rr)), "ROC")
  learner$param_set$values = list(mtry = 8, num.trees = 617)
  rr = resample(task, learner, resampling)
  plot(evalmod(as_precrec(rr)), "ROC")
}

if (FALSE) {
  # runtime example
  set.seed(1)
  devtools::load_all("../bbotk")
  devtools::load_all()
  library(paradox)
  library(mlr3pipelines)
  library(mlr3learners)
  library(mlr3tuning)
  library(mlr3viz)

  #lgr::get_logger("mlr3")$set_threshold("warn")

  task = tsk("sonar")
  learner = lrn("classif.ranger", predict_type = "prob")
  resampling = rsmp("cv", folds = 3L)
  measure = msr("classif.acc")

  # FIXME: needs all Tuning classes later
  #obfun = ObjectiveTuning$new(task = task, learner = learner,
  #  resampling = resampling, measures = list(measure),
  #  store_benchmark_result = TRUE,
  #  store_models = TRUE, check_values = TRUE)

  domain = ParamSet$new(list(
    ParamInt$new("mtry", lower = 1L, upper = 60L),
    ParamInt$new("num.trees", lower = 1L, upper = 10000L),
    ParamDbl$new("sample.fraction", lower = 0.1, upper = 1)))

  obfun = ObjectiveRFun$new(
    fun = function(xs) {
      learner$param_set$values = xs
      rr = resample(task, learner, resampling)
      rr$aggregate(measure)
    },
    domain = domain,
    codomain = ParamSet$new(list(ParamDbl$new("y", tags = "maximize"))),
    id = "y"
  )

  # FIXME: Need an ObjectiveFeature to be more liberal
  ffun = ObjectiveRFun$new(
    fun = function(xs) {
      learner$param_set$values = xs
      learner$train(task)
      learner$timings["train"]
    },
    domain = domain,
    codomain = ParamSet$new(list(ParamDbl$new("time", tags = "minimize"))),
    id = "gtime",
    check_values = FALSE
  )

  # FIXME: allow for quantile definiton of niche storing times in a container
  nb1 = NicheBoundaries$new("niche1", niche_boundaries = list(time = c(0, 1)))
  nb2 = NicheBoundaries$new("niche2", niche_boundaries = list(time = c(1, 10)))

  nb = NichesBoundaries$new("test", niches_boundaries = list(niche1 = nb1, niche2 = nb2))

  surrogate = lrn("regr.km")
  surrogate$param_set$values = list(covtype = "matern3_2", optim.method = "gen", jitter = 0)

  ftfun = Feature$new("test", ffun, nb, SurrogateSingleCritLearner$new(surrogate$clone(deep = TRUE)))

  terminator = trm("evals", n_evals = 50)

  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    feature = ftfun,
    terminator = terminator
  )

  acq_function = AcqFunctionEJIE$new(SurrogateSingleCritLearner$new(surrogate$clone(deep = TRUE)))
  acq_optimizer = AcqOptimizerRandomSearch$new()
  #n_design = 4 * instance$search_space$length

  bayesop_bop(instance, acq_function, acq_optimizer)

  # niche2 objective
  p1 = acq_function$surrogate$predict(data.table(mtry = 1:60, num.trees = 6017, sample.fraction = 0.7519022))
  p1$lwr = p1$mean - 1.96 * p1$se
  p1$upr = p1$mean + 1.96 * p1$se
  p1$mtry = 1:60
  ggplot(p1, aes(mtry, mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha =0.3) +
    labs(x = "mtry") +
    labs(y = "Mean classif.acc, num.trees = 3707, sample.fraction = 0.883")

  p2 = acq_function$surrogate$predict(data.table(mtry = 24, num.trees = 1:10000, sample.fraction = 0.7519022))
  p2$lwr = p2$mean - 1.96 * p2$se
  p2$upr = p2$mean + 1.96 * p2$se
  p2$num.trees = 1:10000
  ggplot(p2, aes(num.trees, mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha =0.3) +
    labs(x = "num.trees") +
    labs(y = "Mean Fitting Time, mtry = 54, sample.fraction = 0.883")

  p3 = acq_function$surrogate$predict(data.table(mtry = 24, num.trees = 9068, sample.fraction = seq(0, 1, length.out = 1000)))
  p3$lwr = p3$mean - 1.96 * p3$se
  p3$upr = p3$mean + 1.96 * p3$se
  p3$sample.fraction = seq(0, 1, length.out = 1000)
  ggplot(p3, aes(sample.fraction, mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha =0.3) +
    labs(x = "mtry") +
    labs(y = "Mean Fitting Time, mtry = 54, num.trees = 3707")

  # niche2 feature
  p1 = instance$feature$surrogate$predict(data.table(mtry = 1:60, num.trees = 9068, sample.fraction = 0.7519022))
  p1$lwr = p1$mean - 1.96 * p1$se
  p1$upr = p1$mean + 1.96 * p1$se
  p1$mtry = 1:60
  ggplot(p1, aes(mtry, mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha =0.3) +
    labs(x = "mtry") +
    labs(y = "Mean Fitting Time, num.trees = 3707, sample.fraction = 0.883")

  p2 = instance$feature$surrogate$predict(data.table(mtry = 54, num.trees = 1:10000, sample.fraction = 0.8825619))
  p2$lwr = p2$mean - 1.96 * p2$se
  p2$upr = p2$mean + 1.96 * p2$se
  p2$num.trees = 1:10000
  ggplot(p2, aes(num.trees, mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha =0.3) +
    labs(x = "num.trees") +
    labs(y = "Mean Fitting Time, mtry = 54, sample.fraction = 0.883")

  p3 = instance$feature$surrogate$predict(data.table(mtry = 54, num.trees = 3707, sample.fraction = seq(0, 1, length.out = 1000)))
  p3$lwr = p3$mean - 1.96 * p3$se
  p3$upr = p3$mean + 1.96 * p3$se
  p3$sample.fraction = seq(0, 1, length.out = 1000)
  ggplot(p3, aes(sample.fraction, mean)) +
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
    labs(x = "mtry") +
    labs(y = "Mean Fitting Time, mtry = 54, num.trees = 3707")
}

