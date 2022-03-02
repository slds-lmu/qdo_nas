#' @export
OptimizerBOHBQDO = R6Class("OptimizerBOHBQDO",
  inherit = Optimizer,
  public = list(

    #' @field niches ([bbotk::Niches]).
    niches = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(niches) {
      assert_r6(niches, "Niches")
      self$niches = niches
      param_set = ps(
        rho  = p_dbl(lower = 0, upper = 1, default = 0.1),  # FIXME: currently not used
        acq_function = p_uty(),
        acq_optimizer = p_uty(),
        eta = p_dbl(lower = 1.0001, default = 2),
        #sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler", null.ok = TRUE)),
        sampler = p_uty(),
        repeats = p_lgl(default = FALSE)
      )
      param_set$values = list(rho = 0.1, acq_function = NULL, acq_optimizer = NULL, eta = 2, sampler = NULL, repeats = FALSE)

      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct", "ParamUty"),
        param_set = param_set,
        properties = c("dependencies", "single-crit"),
        packages = "mlr3mbo"
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      # FIXME: assert instance qdo singlecrit
      pars = self$param_set$values
      rho = pars$rho
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

      niches = names(self$niches$niches)
      n_niches = length(niches)

      n_min = 10L  # FIXME:

      repeat({
        # original hyperband algorithm iterates over brackets
        # this implementation iterates over stages with same budget
        # the number of iterations (s_max + 1) remains the same in both implementations
        for (s in s_max:0) {
          # budget of a single configuration in the first stage (unscaled)
          rs = r_min * r * eta^(-s)

          # sample initial configurations of bracket BOHB-like
          # FIXME: runif(1L, min = 0, max = 1) < rho)
          xdt = if (NROW(inst$archive$data) == 0L) {
            sampler$sample(n[s + 1])$data
          } else {
            archive = inst$archive
            data = inst$archive$data
            n_evaluated = data[, .N, by = budget_id]

            #budget_to_fit = n_evaluated[N >= n_min + 2L, max(c(0, get(budget_id)))]
            budget_to_fit = 1  # FIXME: currently, surrogate also uses fidelity and we fit on whole archive

            if (budget_to_fit == 0) {
              sampler$sample(n[s + 1])$data
            } else {
              bo_archive = archive$clone(deep = TRUE)

              #old_space = bo_archive$search_space$clone(deep = TRUE)  # FIXME: needed due to https://github.com/mlr-org/bbotk/issues/166
              #bo_archive$search_space = old_space$subset(setdiff(old_space$ids(), budget_id))  # adjust
              #bo_archive$data = bo_archive$data[get(budget_id) == budget_to_fit]

              budget_param = bo_archive$search_space$params[[budget_id]]$clone(deep = TRUE)
              budget_ps = ParamSet$new(list(budget_param))
              if (budget_ps$class[[budget_id]] == "ParamInt") budget_ps$values[[budget_id]] = round(rs) else budget_ps$values[[budget_id]] = rs
              old_space = bo_archive$search_space$clone(deep = TRUE)  # FIXME: needed due to https://github.com/mlr-org/bbotk/issues/166
              bo_archive$search_space = old_space$subset(setdiff(old_space$ids(), budget_id))  # adjust

              acq_function = self$param_set$values$acq_function
              acq_optimizer = self$param_set$values$acq_optimizer
              xdt = do_bo_proposal(bo_archive, acq_function = acq_function, acq_optimizer = acq_optimizer, budget_id = budget_id, budget_ps = budget_ps, n = n[s + 1])
              if (NROW(xdt) < n[s + 1]) {
                to_add = n[s + 1] - NROW(xdt)
                xdt = rbind(xdt, sampler$sample(to_add)$data, fill = TRUE)
              }
              xdt
            }
          }

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
              y = data_bracket[, c(archive$cols_y, archive$cols_niche), with = FALSE]
              y[, id := seq_len(.N)]
              setorderv(y, cols = archive$cols_y, order = ifelse(minimize, yes = -1L, no = 1L))

              # select best ni configurations
              # FIXME: this needs to be extended for overlapping niches
              ni_niche = floor(ni / n_niches)
              ni_add = ni - (ni_niche * n_niches)
              row_ids = unlist(map(niches, function(niche_) {
                y[niche_ %in% niche][seq_len(min(ni_niche, NROW(y[niche_ %in% niche])))]$id
              }))
              row_ids = unique(row_ids)  # FIXME: this is brute force
              ni_add = ni - length(row_ids)              
              if (ni_add > 0) {
                # choose the best not already selected
                row_ids = c(row_ids, y[id %nin% row_ids][seq_len(ni_add)]$id)
                # FIXME: could also sample randomly new ones
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
  devtools::load_all("../bbotk")
  devtools::load_all()
  library(paradox)
  library(mlr3learners)

  nb1 = NicheBoundaries$new("niche1", niche_boundaries = list(g = c(-Inf, -0.5)))
  nb2 = NicheBoundaries$new("niche2", niche_boundaries = list(g = c(-0.5, 0)))
  nb3 = NicheBoundaries$new("niche3", niche_boundaries = list(g = c(0, 0.5)))
  nb4 = NicheBoundaries$new("niche4", niche_boundaries = list(g = c(0.5, Inf)))
  nb = NichesBoundaries$new("test", niches_boundaries = list(niche1 = nb1, niche2 = nb2, niche3 = nb3, niche4 = nb4))

  obfun = ObjectiveRFunDt$new(
    fun = function(xdt) {
      tmp = data.table(
        y = - sin((xdt$x2) / 2L) + cos(3L * xdt$x1) - xdt$budget,
        g = cos(xdt$x1 * 2L)
      )
      tmp[, niche := nb$get_niche_dt(tmp[, "g"])]
      tmp
    },
    domain = ParamSet$new(list(ParamDbl$new("x1", 0, 10), ParamDbl$new("x2", 0, 10), ParamDbl$new("budget", 1e-3, 1, tags = "budget"))),
    codomain = ParamSet$new(list(
      ParamDbl$new("y", tags = "minimize"),
      ParamDbl$new("g", tags = "feature"),
      #ParamFct$new("niche", levels = c("niche1", "niche2", "niche3", "niche4"), special_vals = list(NA_character_), tags = "niche"))
      ParamUty$new("niche", tags = "niche"))
    ),
    properties = "single-crit",
    check_values = FALSE,
    id = "test"
  )

  instance = OptimInstanceQDOSingleCrit$new(
    objective = obfun,
    terminator = trm("none")
  )

  ranger = lrn("regr.ranger")
  ranger$param_set$values$se.method = "jack"
  learner = ranger
  surrogate = default_surrogate(instance, learner = learner, n_learner = 2L)
  surrogate$archive = instance$archive
  surrogate$y_cols = c("y", "g")
  
  ejie = AcqFunctionEJIE$new(surrogate, niches = nb, worst = 2)
  acq_optimizer = AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))
  acq_optimizer$acq_function = ejie
  
  optimizer = OptimizerBOHBQDO$new(niches = nb)
  optimizer$param_set$values$eta = 3
  optimizer$param_set$values$acq_function = ejie
  optimizer$param_set$values$acq_optimizer = acq_optimizer

  optimizer$optimize(instance)
}

