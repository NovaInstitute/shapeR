#' Construct a SHACL property shape
#'
#' Creates an S3 object representing a SHACL property shape, typically
#' referenced from a node shape via \code{sh:property}. Property shapes
#' constrain values reachable via a given \code{sh:path}.
#'
#' @param id Optional identifier for the property shape. May be an IRI or a
#'   blank-node identifier. Can be \code{NULL} if the shape is anonymous.
#' @param path A scalar character string giving the value of \code{sh:path}.
#'   For simple paths this is usually an IRI. More complex paths (sequence,
#'   alternative, inverse) can be represented in a later extension.
#' @param constraints A list of \code{sh_constraint} objects representing the
#'   constraint components defined on this property shape (e.g. minCount,
#'   maxCount, datatype, class, in, etc.).
#' @param nested A list describing nested shapes and logical combinations.
#'   It should typically contain elements:
#'   \itemize{
#'     \item \code{node}: a single shape identifier referenced via \code{sh:node}
#'     \item \code{or}: character vector of alternative shape IDs from \code{sh:or}
#'     \item \code{and}: character vector of shape IDs from \code{sh:and}
#'     \item \code{xone}: character vector of shape IDs from \code{sh:xone}
#'   }
#' @param annotations A named list for messages and other annotation properties
#'   (e.g. \code{sh:message}, \code{rdfs:comment}).
#' @param severity Optional scalar character string for the default severity
#'   of this shape (e.g. \code{"sh:Violation"}, \code{"sh:Warning"}).
#' @param deactivated Logical flag indicating whether the shape is deactivated
#'   (\code{sh:deactivated}).
#' @param extras A list for additional fields not yet modelled explicitly.
#'
#' @return An object of class \code{"sh_property_shape"}.
#'
#' @examples
#' min_age <- sh_constraint(
#'   component = "sh:MinCountConstraintComponent",
#'   params    = list(minCount = 1L),
#'   scope     = "property"
#' )
#'
#' age_datatype <- sh_constraint(
#'   component = "sh:DatatypeConstraintComponent",
#'   params    = list(datatype = "xsd:integer"),
#'   scope     = "property"
#' )
#'
#' sh_property_shape(
#'   id          = "_:ageShape",
#'   path        = "ex:age",
#'   constraints = list(min_age, age_datatype)
#' )
#'
#' @export
sh_property_shape <- function(id = NULL,
                              path,
                              constraints = list(),
                              nested = list(node = NULL,
                                            or   = character(),
                                            and  = character(),
                                            xone = character()),
                              annotations = list(),
                              severity = NULL,
                              deactivated = FALSE,
                              extras = list()) {

  # id: NULL or scalar character
  if (!is.null(id) &&
      (!is.character(id) || length(id) != 1L || is.na(id))) {
    stop("`id` must be NULL or a non-NA scalar character string.", call. = FALSE)
  }

  # path: required scalar character
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a non-NA scalar character string.", call. = FALSE)
  }

  # constraints: list of sh_constraint
  if (!is.list(constraints)) {
    stop("`constraints` must be a list.", call. = FALSE)
  }
  if (length(constraints) > 0L) {
    ok <- vapply(constraints, inherits, logical(1), "sh_constraint")
    if (!all(ok)) {
      stop("All elements of `constraints` must be of class 'sh_constraint'.",
           call. = FALSE)
    }
  }

  # nested: must be a list with expected slots
  if (!is.list(nested)) {
    stop("`nested` must be a list.", call. = FALSE)
  }
  if (is.null(nested$node)) {
    node <- NULL
  } else if (is.character(nested$node) && length(nested$node) == 1L) {
    node <- nested$node
  } else {
    stop("`nested$node` must be NULL or a scalar character string.", call. = FALSE)
  }

  or   <- nested$or   %||% character()
  and  <- nested$and  %||% character()
  xone <- nested$xone %||% character()

  if (!is.character(or)   || anyNA(or))   stop("`nested$or` must be character with no NA.",   call. = FALSE)
  if (!is.character(and)  || anyNA(and))  stop("`nested$and` must be character with no NA.",  call. = FALSE)
  if (!is.character(xone) || anyNA(xone)) stop("`nested$xone` must be character with no NA.", call. = FALSE)

  # annotations
  if (!is.list(annotations)) {
    stop("`annotations` must be a list.", call. = FALSE)
  }

  # severity: NULL or scalar character
  if (!is.null(severity) &&
      (!is.character(severity) || length(severity) != 1L || is.na(severity))) {
    stop("`severity` must be NULL or a non-NA scalar character string.", call. = FALSE)
  }

  # deactivated: logical scalar
  if (!is.logical(deactivated) || length(deactivated) != 1L || is.na(deactivated)) {
    stop("`deactivated` must be a non-NA logical scalar.", call. = FALSE)
  }

  # extras
  if (!is.list(extras)) {
    stop("`extras` must be a list.", call. = FALSE)
  }

  out <- list(
    id          = id,
    path        = path,
    constraints = constraints,
    nested      = list(
      node = node,
      or   = or,
      and  = and,
      xone = xone
    ),
    annotations = annotations,
    severity    = severity,
    deactivated = deactivated,
    extras      = extras
  )

  structure(out, class = c("sh_property_shape", "sh_shape", "list"))
}

# small internal helper: "x %||% y"  (only if you don't have it already)
`%||%` <- function(x, y) if (is.null(x)) y else x
