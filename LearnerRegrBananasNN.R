library(R6)
library(mlr3)
#reticulate::use_python("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9/bin/python3.7m")
#reticulate::use_virtualenv("/home/lps/.local/share/virtualenvs/qdo_nas-zvu37In9")
reticulate::use_python("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas/bin/python3", required = TRUE)
reticulate::use_virtualenv("/dss/dsshome1/lxc0C/ru84tad2/.virtualenvs/qdo_nas", required = TRUE)
library(reticulate)
py_run_file("nn_ensemble.py")
LearnerRegrBananasNN= R6Class("LearnerRegrBananasNN", inherit = LearnerRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
      ))
      super$initialize(
        id = "regr.bananasNN",
        feature_types = c("integer", "logical", "numeric"),
        predict_types = "se",
        param_set = ps,
      )
    }
  ),

  private = list(
    .train = function(task) {
      dt = task$data(cols = task$feature_names)
      dt = as.matrix(as.data.table(lapply(dt, as.numeric)))
      yt = task$data(cols = task$target_names)[[1L]]
      nn = py$train_ensemble(np_array(dt), np_array(yt))
      nn
    },

    .predict = function(task) {
      dt = task$data(cols = task$feature_names)
      dt = as.matrix(as.data.table(lapply(dt, as.numeric)))
      preds = map_dtc(self$model, .f = function(nn) {
        nn$predict(np_array(dt))
      })
      response = rowMeans(preds)
      se = apply(preds, MARGIN = 1L, FUN = sd)
      list(response = response, se = se)
    }
  )
)

