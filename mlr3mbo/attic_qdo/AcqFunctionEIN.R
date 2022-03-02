#' @title Acquisition Function Expected Improvement of a Niche
#'
#' @description
#' Expected Improvement of a Niche.
#'
#' @export
AcqFunctionEIN = R6Class("AcqFunctionEIN",
  inherit = AcqFunction,
  public = list(

    #' @field niches ([Niches]).
    niches = NULL,

    #' @field niche_id (`character(1)`).
    niche_id = NULL,

    #' @field y_bests (`numeric()`).
    y_bests = NULL,

    #' @field cols_y (`character()`).
    #' #FIXME: Name
    cols_y = NULL,

    #' @field cols_g (`character()`).
    #' #FIXME: Name
    #' #FIXME: must match ids in NichesBoundaries
    cols_g = NULL,

    #' @field cols_niche (`character()`).
    #' #FIXME: Name
    #' #FIXME: must match ids in NichesBoundaries
    cols_niche = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param surrogate ([SurrogateMultiCrit]).
    #' @param niches ([Niches]).
    #' @param niche_id (`character(1)`).
    initialize = function(surrogate, niches, niche_id) {
      assert_r6(surrogate, "SurrogateMultiCrit")
      assert_r6(niches, "Niches")
      assert_choice(niche_id, names(niches$niches))
      self$niches = niches
      self$niche_id = niche_id

      fun = function(xdt) {

        if (is.null(self$y_bests)) {
          stop("y_bests is not set. Missed to call $update(archive)?")
        }

        p = self$surrogate$predict(xdt)
        mu = p[[self$cols_y]]$mean
        se = p[[self$cols_y]]$se

        best = self$y_bests[[self$niche_id]][[self$cols_y]]

        # FIXME: this does not work; consider mult_max_to_min
        if (!length(best)) best = self$surrogate_max_to_min[[self$cols_y]] * max(unlist(self$y_bests), na.rm = TRUE)
        #if (!is.finite(best)) best = 0

        d = best - self$surrogate_max_to_min[[self$cols_y]] * mu
        d_norm = d / se
        ei_j = d * pnorm(d_norm) + se * dnorm(d_norm)

        prob_j = {
        # FIXME: not to model feature function via tags?
        #if (is.null(self$feature_surrogate_predict)) {
        #  niche = self$niches$get_niche_dt(self$feature_function_eval_dt(xdt))
        #  map(transpose_list(niche), function(x) {
        #    p_j = rep(0, length(self$niches$niches))
        #    niche_match = match(x, names(self$niches$niches), nomatch = 0)
        #    p_j[niche_match] = 1
        #    p_j
        #  })
        #} else {
          # FIXME: sum to 1?
          p_g = p[self$cols_g]
          mu_g = if (is.list(p_g) & !is.data.table(p_g)) map(p_g, "mean") else setNames(list(p_g$mean), nm = self$cols_g)
          se_g = if (is.list(p_g) & !is.data.table(p_g)) map(p_g, "se") else setNames(list(p_g$se), nm = self$cols_g)

          if (test_r6(self$niches, classes = "NichesBoundaries")) {
            boundaries = self$niches$niches[[self$niche_id]]$niche_boundaries
            pj = 1
            for (id in self$cols_g) {
              pj = pj * (pnorm((mu_g[[id]] - boundaries[[id]][1L]) / se_g[[id]]) - pnorm((mu_g[[id]] - boundaries[[id]][2L]) / se_g[[id]]))
            }
            pj
          }
        }

        #ejie = Reduce("+", pmap(list(ei_j, prob_j), function(ec, pc) ec * pc))
        ein = ei_j * prob_j

        # FIXME: we have to check if a point is in a niche, if not, we do not want to propose it at all, i.e, return ejie = 0
        ein[se < 1e-20] = 0
        #ejie[se < 1e-20 | is.na(niche)] = 0
        data.table(acq_ein = ein)
      }

      super$initialize("acq_ein", surrogate = surrogate, direction = "maximize", fun = fun)
    },

    #' @description
    #' Sets up the acquisition function.
    #'
    #' @param archive [bbotk::Archive].
    setup = function(archive) {
      # FIXME: Should we allow alternative search_space as additional argument?

      # here we can change the optim direction of the codomain for the acq function
      self$codomain = generate_acq_codomain(archive$codomain, id = self$id, direction = self$direction)

      self$surrogate_max_to_min = mult_max_to_min(archive$codomain)

      self$domain = archive$search_space$clone(deep = TRUE)
      #self$domain$trafo = NULL # FIXME: is it okay to do this?

      self$cols_y = archive$cols_y
      self$cols_g = archive$cols_g
      self$cols_niche = archive$cols_niche
    },

    #' @description
    #' Updates acquisition function and sets `y_bests`.
    #'
    #' @param archive [bbotk::ArchiveQDO]
    update = function(archive) {
      super$update(archive)
      self$y_bests = setNames(transpose_list(archive$best()[, archive$cols_y, with = FALSE]), nm = archive$best()[[archive$cols_niche]])
    }
  )
)
