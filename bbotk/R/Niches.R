#' @title Niches
#'
#' @description
#' Describes a set of niches related to a feature function in QDO.
#'
#' @export
Niches = R6Class("Niches",
  public = list(

    #' @field id (`character(1)`).
    id = NULL,

    #' @field niches (`list()`).
    niches = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param niches (`list()`).
    initialize = function(id, niches) {
      self$id = assert_string(id)
      self$niches = niches
    },

    #' @description
    #' Get the niche a point belongs to based on the evaluation of the feature function.
    #'
    #' @param gval (`list()`)\cr
    #' A `list()` that contains a value of the feature function, e.g., `list(g1 = 1, g2 = 2)`.
    #'
    #' @return `character(1)` indicating the niche.
    get_niche = function(gval) {
      private$.get_niche(gval)
    },

    #' @description
    #' Get the niches multiple points belong to based on the evaluations of the feature function stored in a `list()`.
    #'
    #' @param gvals (`list()`)\cr
    #' A `list()` that contains values of the feature function for multiple points, e.g., `list(list(g1 = 1, g2 = 2), list(g1 = 2, g2 = 3))`.
    #'
    #' @return `character()` indicating the niches.
    get_niche_many = function(gvals) {
      map(gvals, private$.get_niche)
    },

    #' @description
    #' Get the niches multiple points belong to based on the evaluations of the feature function stored in a [data.table::data.table].
    #'
    #' @param gvaldt ([data.table::data.table])\cr
    #' A [data.table::data.table] containing values of the feature function for multiple points as rows.
    #'
    #' @return [data.table::data.table] containing the niches.
    get_niche_dt = function(gvaldt) {
      data.table(niche = map(transpose_list(gvaldt), private$.get_niche))
    }
  ),

  private = list(
    .get_niche = function(gval) {
      stop("Abstract.")
    }
  )
)



#' @title NichesROC
#'
#' @description
#' Describes a set of niches related to a ROC feature function in QDO.
#'
#' @export
NichesROC = R6Class("NichesROC", inherit = Niches,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param ellipsoids (`list()`).
    initialize = function(id, ellipsoids) {
      assert_list(ellipsoids, types = "list", any.missing = FALSE, min.len = 2L, names = "strict")
      map(ellipsoids, assert_list, types = "Ellipsoid2D", any.missing = FALSE, min.len = 1L, names = "strict")
      super$initialize(id = id, niches = map(ellipsoids, assert_list, types = "Ellipsoid2D", any.missing = FALSE, min.len = 1L, names = "strict"))
    }
  ),

  private = list(
    .get_niche = function(gval) {
      gval = as.data.table(gval[[1L]])
      niche = names(self$niches)[map_lgl(self$niches, function(niche) all(map_lgl(niche, function(ellipsoid) any(apply(gval, MARGIN = 1L, FUN = ellipsoid$is_inside)))))]
      #if (length(niche) > 1L) niche = sample(niche, 1L)
      if (!length(niche)) niche = NA_character_
      niche
    }
  )
)



Ellipsoid2D = R6Class("Ellipsoid2D",
  #FIXME: shape matrix, plot method car::elipse
  public = list(
    id = NULL,
    center = NULL,
    radii = NULL,
    initialize = function(id, center, radii) {
      self$id = assert_string(id, min.chars = 1L)
      self$center = assert_numeric(center, finite = TRUE, any.missing = FALSE, len = 2L)
      self$radii = assert_numeric(radii, lower = 1e-3, finite = TRUE, any.missing = FALSE, len = 2L)
    },
    is_inside = function(point) {
      assert_numeric(point, finite = TRUE, any.missing = FALSE, len = 2L)
      sum((point - self$center) ^ 2 / self$radii ^ 2) < 1
    }
  )
)



#' @title NicheBoundaries
#'
#' @description
#' Describes a niche related to a feature function in QDO based on some boundaries.
#' FIXME: assert somewhere that feature function ids match boundary ids
#'
#' @export
NicheBoundaries = R6Class("NicheBoundaries",
  public = list(

    #' @field id (`character(1)`).
    id = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param niche_boundaries (`list()`).
    initialize = function(id, niche_boundaries) {
      self$id = assert_string(id)
      private$.niche_boundaries = assert_niche_boundaries(niche_boundaries)
      private$.feature_function_ids = assert_character(names(niche_boundaries), any.missing = FALSE, min.len = 1L, unique = TRUE)
    }
  ),

  private = list(
    .feature_function_ids = NULL,
    .niche_boundaries = NULL
  ),

  active = list(
    #' @field feature_function_ids (`character()`).
    feature_function_ids = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.feature_function_ids)) {
        stop("feature_function_ids is read-only.")
      }
      private$.feature_function_ids
    },

    #' @field niche_boundaries (`list()`).
    niche_boundaries = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.niche_boundaries)) {
        stop("niche_boundaries is read-only.")
      }
      private$.niche_boundaries
    }
  )
)



#' @title NichesBoundaries
#'
#' @description
#' Describes a set of niches related to a feature function in QDO based on some boundaries.
#'
#' @export
NichesBoundaries = R6Class("NichesBoundaries", inherit = Niches,
  # FIXME: needs a simple container for the resulting boundaries?
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`).
    #' @param niches_boundaries (`list()`).
    initialize = function(id, niches_boundaries) {
      super$initialize(id = id, niches = assert_niches_boundaries(niches_boundaries))
    }
  ),

  private = list(
    .get_niche = function(gval) {

      #assert_list(gval, types = "numeric", any.missing = FALSE, len = length(self$feature_function_ids))
      #assert_names(names(gval), type = "strict", subset.of = self$feature_function_ids)
      #gval = gval[self$feature_function_ids]  # FIXME: reorder necessary?
      # FIXME: the following is way too complex
      is_in_niche = map_lgl(self$niches, function(niche) {
        all(pmap_lgl(list(niche$niche_boundaries, gval), function(interval, point) interval[1L] <= point && point < interval[2L]))
      })
      niche = names(self$niches)[is_in_niche]
      if (!length(niche)) niche = NA_character_

      niche
    }
  )
)



# Helper function to check structure of a single niche
check_niche_boundaries = function(x) {
  # FIXME: boundaries w.r.t a niche must be ok w.r.t. the domain of the feature functions
  # FIXME: boundaries must be sorted (lower, upper)
  structure_check = check_list(x, types = "numeric", any.missing = FALSE, min.len = 1L, names = "strict", null.ok = FALSE)
  if (!isTRUE(structure_check)) {
    return(structure_check)  # early exit
  }
  lengths = map_int(x, length)
  if (!all(lengths == 2L)) {
    return("All elements must be numeric intervals of length 2")  # early exit
  }
  return(TRUE)
}

# Helper function to assert structure of a single niche
assert_niche_boundaries = function(x, .var.name = vname(x), add = NULL) {
  if (missing(x)) {
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  }
  res = check_niche_boundaries(x)
  makeAssertion(x, res, .var.name, add)
}



# Helper function to check structure of niches
check_niches_boundaries = function(x) {
  structure_check = check_list(x, types = "NicheBoundaries", any.missing = FALSE, min.len = 1L, unique = TRUE, names = "strict", null.ok = FALSE)
  if (!isTRUE(structure_check)) {
    return(structure_check)
  }
  ids = map_chr(x, "id")
  ids_check = check_character(ids, unique = TRUE)
  if (!isTRUE(ids_check)) {
    return(ids_check)
  }
  names(x) = ids
  feature_function_ids = setDT(map(x, "feature_function_ids"))
  if (NROW(unique(t(feature_function_ids))) > 1L) {
    return("Niches must be defined on the same feature functions in the same order")
  }
  return(TRUE)
}

# Helper function to assert structure of niches
assert_niches_boundaries = function(x, .var.name = vname(x), add = NULL) {
  if (missing(x)) {
    stop(sprintf("argument \"%s\" is missing, with no default", .var.name))
  }
  res = check_niches_boundaries(x)
  makeAssertion(x, res, .var.name, add)
}

