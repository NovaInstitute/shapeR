#' Construct a SHACL constraint component
#'
#' Creates a lightweight S3 object representing a single SHACL constraint
#' component such as \code{sh:MinCountConstraintComponent},
#' \code{sh:ClassConstraintComponent}, \code{sh:InConstraintComponent}, etc.
#'
#' @param component A scalar character string giving the constraint component
#'   IRI or CURIE, e.g. \code{"sh:MinCountConstraintComponent"}.
#' @param params A named list of parameter values associated with the
#'   constraint, for example \code{list(minCount = 1)} or
#'   \code{list(in = c("ex:A", "ex:B"))}.
#' @param scope A scalar character string indicating the scope where the
#'   constraint was defined. Typically \code{"node"} for node shapes or
#'   \code{"property"} for property shapes.
#'
#' @return An object of class \code{"sh_constraint"}.
#'
#' @examples
#' sh_constraint(
#'   component = "sh:MinCountConstraintComponent",
#'   params    = list(minCount = 1),
#'   scope     = "property"
#' )
#'
#' @export
sh_constraint <- function(component,
                          params = list(),
                          scope = c("node", "property")) {

  # basic checks
  if (!is.character(component) || length(component) != 1L || is.na(component)) {
    stop("`component` must be a non-NA scalar character string.", call. = FALSE)
  }

  if (!is.list(params)) {
    stop("`params` must be a list.", call. = FALSE)
  }

  scope <- match.arg(scope)

  structure(
    list(
      component = component,
      params    = params,
      scope     = scope
    ),
    class = c("sh_constraint", "list")
  )
}
