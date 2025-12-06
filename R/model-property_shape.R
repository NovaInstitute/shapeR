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
#' @param prefixes Optional named character vector of prefix mappings used to
#'   normalise IRIs.
#' @param base_iri Optional base IRI used when resolving relative identifiers.
#' @param normalise Logical; if TRUE, CURIEs/relative IRIs are expanded using
#'   \code{prefixes} and \code{base_iri} before being stored.
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
                              extras = list(),
                              prefixes = NULL,
                              base_iri = NULL,
                              normalise = FALSE) {

  # id: NULL or scalar character
  id <- check_scalar_character(id, "id", allow_null = TRUE)

  # path: required scalar character
  path <- check_scalar_character(path, "path")

  # constraints: list of sh_constraint
  constraints <- check_list(constraints, "constraints")
  check_list_of_class(constraints, "sh_constraint", "constraints")

  # nested: must be a list with expected slots
  nested <- check_list(nested, "nested")
  node <- check_scalar_character(nested$node, "nested$node", allow_null = TRUE)

  or   <- nested$or   %||% character()
  and  <- nested$and  %||% character()
  xone <- nested$xone %||% character()

  check_character_vector(or, "nested$or")
  check_character_vector(and, "nested$and")
  check_character_vector(xone, "nested$xone")

  # annotations
  annotations <- check_list(annotations, "annotations")

  # severity: NULL or scalar character
  severity <- check_scalar_character(severity, "severity", allow_null = TRUE)

  # deactivated: logical scalar
  deactivated <- check_logical_scalar(deactivated, "deactivated")

  # extras
  extras <- check_list(extras, "extras")

  prefixes <- check_character_vector(prefixes, "prefixes", allow_null = TRUE) %||% character()

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

  if (isTRUE(normalise)) {
    out$id   <- if (!is.null(out$id)) normalise_iri(out$id, prefixes, base_iri) else NULL
    out$path <- normalise_iri(out$path, prefixes, base_iri)

    out$nested$node <- if (!is.null(out$nested$node)) normalise_iri(out$nested$node, prefixes, base_iri) else NULL
    out$nested$or   <- normalise_iri(out$nested$or, prefixes, base_iri)
    out$nested$and  <- normalise_iri(out$nested$and, prefixes, base_iri)
    out$nested$xone <- normalise_iri(out$nested$xone, prefixes, base_iri)
  }

  structure(out, class = c("sh_property_shape", "sh_shape", "list"))
}

