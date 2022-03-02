#' @export
OptimizerHyperbandMO = R6Class("OptimizerHyperbandMO",
  inherit = Optimizer,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        eta = p_dbl(lower = 1.0001, default = 2),
        #sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        sampler = p_uty(),
        repeats = p_lgl(default = FALSE)
      )
      param_set$values = list(eta = 2, sampler = NULL, repeats = FALSE)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct", "ParamUty"),
        param_set = param_set,
        properties = c("dependencies", "multi-crit"),
        packages = "mlr3mbo"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      pars = self$param_set$values
      eta = pars$eta
      sampler = pars$sampler
      search_space = inst$search_space
      budget_id = search_space$ids(tags = "budget")

      # check budget
      if (length(budget_id) != 1) stopf("Exactly one parameter must be tagged with 'budget'")
      assert_choice(search_space$class[[budget_id]], c("ParamInt", "ParamDbl"))

      # sampler
      search_space_sampler = search_space$clone()$subset(setdiff(search_space$ids(), budget_id))
      if (is.null(sampler)) {
        sampler = SamplerUnif$new(search_space_sampler)
      } else {
        #assert_set_equal(sampler$param_set$ids(), search_space_sampler$ids())
      }

      # r_min is the budget of a single configuration in the first stage
      # r_max is the maximum budget of a single configuration in the last stage
      # the internal budget is rescaled to a minimum budget of 1
      # for this, the budget is divided by r_min
      # the budget is transformed to the original scale before passing it to the objective function
      r_max = search_space$upper[[budget_id]]
      r_min = search_space$lower[[budget_id]]

      # maximum budget of a single configuration in the last stage (scaled)
      # R in the original paper
      r = r_max / r_min

      # s_max + 1 is the number of brackets and the number stages of the first bracket
      s_max = floor(log(r, eta))

      # approximately the used budget of an entire bracket
      # B in the original paper
      budget = (s_max + 1) * r

      # number of configurations in first stages
      n = ceiling((budget / r) * (eta^(0:s_max)) / ((0:s_max) + 1))

      repeat({
        # original hyperband algorithm iterates over brackets
        # this implementation iterates over stages with same budget
        # the number of iterations (s_max + 1) remains the same in both implementations
        for (s in s_max:0) {
          # budget of a single configuration in the first stage (unscaled)
          rs = r_min * r * eta^(-s)
          # sample initial configurations of bracket
          xdt = sampler$sample(n[s + 1])$data
          set(xdt, j = budget_id, value = rs)
          set(xdt, j = "bracket", value = s)
          set(xdt, j = "stage", value = 0)

          # promote configurations of previous batch
          if (s != s_max) {
            archive = inst$archive
            data = archive$data[batch_nr == archive$n_batch, ]
            minimize = ifelse(archive$codomain$maximization_to_minimization == -1, TRUE, FALSE)

            # for each bracket, promote configurations of previous stage
            xdt_promoted = map_dtr(s_max:(s + 1), function(i) {

              # number of configuration to promote
              ni = floor(n[i + 1] * eta^(-(i - s)))

              # get performances of previous stage
              data_bracket = data[get("bracket") == i, ]
              y = data_bracket[, archive$cols_y, with = FALSE]

              # select best ni configurations
              if (ni < 1) return(data_bracket[integer(0), ])  # FIXME:
              row_ids = if (archive$codomain$length == 1) {
                head(order(unlist(y), decreasing = minimize), ni)
              } else {
                nds_selection(points = t(as.matrix(y)), n_select = ni, minimize = minimize)
              }
              data_bracket[row_ids, ]
            })

            # increase budget and stage
            xdt_promoted = xdt_promoted[, c(inst$archive$cols_x, "stage", "bracket"), with = FALSE]
            set(xdt_promoted, j = budget_id, value = rs)
            set(xdt_promoted, j = "stage", value = xdt_promoted[["stage"]] + 1)

            xdt = rbindlist(list(xdt, xdt_promoted), use.names = TRUE, fill = TRUE)
          }

          if (search_space$class[[budget_id]] == "ParamInt") set(xdt, j = budget_id, value = round(xdt[[budget_id]]))
          inst$eval_batch(xdt)
        }
        if (!pars$repeats) break
      })
    }
  )
)

if (FALSE) {
  set.seed(1)
  devtools::load_all()
  library(paradox)
  library(mlr3learners)

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = data.table(
        y = - sin((xdt$x2) / 2L) + cos(3L * xdt$x1) - xdt$budget,
        g = cos(xdt$x1 * 2L)
      )
      tmp
    },
    domain = ParamSet$new(list(ParamDbl$new("x1", 0, 10), ParamDbl$new("x2", 0, 10), ParamDbl$new("budget", 1e-3, 1, tags = "budget"))),
    codomain = ParamSet$new(list(
      ParamDbl$new("y", tags = "minimize"),
      ParamDbl$new("g", tags = "minimize"))
    ),
    properties = "multi-crit",
    id = "test"
  )

  instance = OptimInstanceMultiCrit$new(
    objective = obfun,
    terminator = trm("none")
  )

  optimizer = OptimizerHyperbandMO$new()
  optimizer$param_set$values$eta = 3
  optimizer$optimize(instance)
}

