
# mlr3mbo

A new R6 and much more modular implementation for single- and
multicriteria Bayesian optimization.

<!-- badges: start -->

[![tic](https://github.com/mlr-org/mlr3mbo/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mlr3mbo/actions)
[![CRANstatus](https://www.r-pkg.org/badges/version/mlr3mbo)](https://cran.r-project.org/package=mlr3mbo)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

## Design

`mlr3mbo` is built modular relying on the following
[R6](https://cran.r-project.org/package=R6) classes:

-   `Surrogate`: Surrogate Model
-   `AcqFunction`: Acquisition Function
-   `AcqOptimizer`: Acquisition Function Optimizer

Based on these, Bayesian optimization loops can be written, see, e.g.,
`bayesopt_ego` for sequential single objective BO.

`mlr3mbo` also provides an `OptimizerMbo` class behaving like any other
`Optimizer` from the [bbotk](https://cran.r-project.org/package=bbotk)
package.

The respective `TunerMbo` is part of the
[mlr3tuning](https://cran.r-project.org/package=mlr3tuning) package.

`mlr3mbo` uses sensible defaults for the `Surrogate`, `AcqFunction`,
`AcqOptimizer`, and even the loop function. See `?mbo_defaults` for more
details.

## API

## Robustness

## Simple Optimization Example

Minimize `y = x^2` via sequential singlecriteria BO using a GP as
surrogate and EI optimized via random search as aquisition function:

``` r
set.seed(1)
library(bbotk)
library(mlr3)
library(mlr3mbo)
library(paradox)
library(mlr3learners)

obfun = ObjectiveRFun$new(
  fun = function(xs) list(y = xs$x ^ 2),
  domain = ps(x = p_dbl(lower = -5, upper = 5)),
  codomain = ps(y = p_dbl(tags = "minimize"))
)

terminator = trm("evals", n_evals = 10)

instance = OptimInstanceSingleCrit$new(
  objective = obfun,
  terminator = terminator
)

design = generate_design_lhs(obfun$domain, 4)$data
instance$eval_batch(design)

surrogate = SurrogateLearner$new(lrn("regr.km", control = list(trace = FALSE)))
acqfun = AcqFunctionEI$new()
acqopt = AcqOptimizer$new(
  opt("random_search", batch_size = 100),
  terminator = trm("evals", n_evals = 100)
 )

optimizer = opt("mbo",
  loop_function = bayesopt_ego,
  surrogate = surrogate,
  acq_function = acqfun,
  acq_optimizer = acqopt
)
optimizer$optimize(instance)
```

    ##             x  x_domain           y
    ## 1: 0.01948605 <list[1]> 0.000379706

## Simple Tuning Example Using Defaults

``` r
set.seed(1)
library(mlr3)
library(mlr3tuning)

task = tsk("pima")

learner = lrn("classif.rpart", cp = to_tune(lower = 1e-04, upper = 1e-1, logscale = TRUE))

instance = tune(
  method = "mbo",
  task = task,
  learner = learner,
  resampling = rsmp("holdout"),
  measure = msr("classif.ce"),
  term_evals = 10
)

instance$result
```

    ##           cp learner_param_vals  x_domain classif.ce
    ## 1: -4.632569          <list[2]> <list[1]>  0.1914062
