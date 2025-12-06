#' Construct a SHACL node shape
#'
#' Creates an S3 object representing a SHACL node shape. Node shapes define
#' constraints on focus nodes and typically reference one or more property
#' shapes via \code{sh:property}.
#'
#' @param id A scalar character string giving the node shape identifier
#'   (IRI or blank-node ID).
#' @param targets A list describing the targeting of the shape. It should
#'   typically contain zero or more of the following elements, each as a
#'   character vector:
#'   \itemize{
#'     \item \code{targetClass}: targets via \code{sh:targetClass}
#'     \item \code{targetNode}: targets via \code{sh:targetNode}
#'     \item \code{targetSubjectsOf}: via \code{sh:targetSubjectsOf}
#'     \item \code{targetObjectsOf}: via \code{sh:targetObjectsOf}
#'   }
#' @param properties A list of \code{sh_property_shape} objects owned by this
#'   node shape (typically coming from \code{sh:property} triples).
#' @param constraints A list of \code{sh_constraint} objects representing
#'   node-level constraints (e.g. \code{sh:class}, \code{sh:in}, \code{sh:or},
#'   etc. when attached directly to the node shape).
#' @param annotations A named list for messages and other annotation properties
#'   (e.g. \code{sh:message}, \code{rdfs:comment}).
#' @param severity Optional scalar character string for the default severity
#'   of this shape (e.g. \code{"sh:Violation"}, \code{"sh:Warning"}).
#' @param deactivated Logical flag indicating whether the shape is deactivated
#'   (\code{sh:deactivated}).
#' @param extras A list for additional fields not yet modelled explicitly.
#' @param prefixes Optional named character vector of prefix mappings used to
#'   normalise identifiers.
#' @param base_iri Optional base IRI used when resolving relative identifiers.
#' @param normalise Logical; if TRUE, CURIEs/relative IRIs are expanded using
#'   \code{prefixes} and \code{base_iri} before being stored.
#'
#' @return An object of class \code{"sh_node_shape"}.
#'
#' @examples
#' person_shape <- sh_node_shape(
#'   id = "ex:PersonShape",
#'   targets = list(targetClass = "ex:Person")
#' )
#'
#' @export
sh_node_shape <- function(id,
                          targets = list(
                            targetClass      = character(),
                            targetNode       = character(),
                            targetSubjectsOf = character(),
                            targetObjectsOf  = character()
                          ),
                          properties  = list(),
                          constraints = list(),
                          annotations = list(),
                          severity    = NULL,
                          deactivated = FALSE,
                          extras      = list(),
                          prefixes    = NULL,
                          base_iri    = NULL,
                          normalise   = FALSE) {

  # id: required scalar character
  id <- check_scalar_character(id, "id")

  # targets: list of character vectors
  targets <- check_list(targets, "targets")

  targetClass      <- targets$targetClass      %||% character()
  targetNode       <- targets$targetNode       %||% character()
  targetSubjectsOf <- targets$targetSubjectsOf %||% character()
  targetObjectsOf  <- targets$targetObjectsOf  %||% character()

  for (name in c("targetClass", "targetNode", "targetSubjectsOf", "targetObjectsOf")) {
    check_character_vector(get(name), paste0("targets$", name))
  }

  # properties: list of sh_property_shape
  properties <- check_list(properties, "properties")
  check_list_of_class(properties, "sh_property_shape", "properties")

  # constraints: list of sh_constraint
  constraints <- check_list(constraints, "constraints")
  check_list_of_class(constraints, "sh_constraint", "constraints")

  # annotations
  annotations <- check_list(annotations, "annotations")

  # severity
  severity <- check_scalar_character(severity, "severity", allow_null = TRUE)

  # deactivated
  deactivated <- check_logical_scalar(deactivated, "deactivated")

  # extras
  extras <- check_list(extras, "extras")

  prefixes <- check_character_vector(prefixes, "prefixes", allow_null = TRUE) %||% character()

  out <- list(
    id          = id,
    targets     = list(
      targetClass      = targetClass,
      targetNode       = targetNode,
      targetSubjectsOf = targetSubjectsOf,
      targetObjectsOf  = targetObjectsOf
    ),
    properties  = properties,
    constraints = constraints,
    annotations = annotations,
    severity    = severity,
    deactivated = deactivated,
    extras      = extras
  )

  if (isTRUE(normalise)) {
    out$id <- normalise_iri(out$id, prefixes, base_iri)

    out$targets$targetClass      <- normalise_iri(out$targets$targetClass, prefixes, base_iri)
    out$targets$targetNode       <- normalise_iri(out$targets$targetNode, prefixes, base_iri)
    out$targets$targetSubjectsOf <- normalise_iri(out$targets$targetSubjectsOf, prefixes, base_iri)
    out$targets$targetObjectsOf  <- normalise_iri(out$targets$targetObjectsOf, prefixes, base_iri)

    if (length(out$properties)) {
      out$properties <- lapply(
        out$properties,
        function(prop) {
          if (!inherits(prop, "sh_property_shape")) return(prop)
          prop$id   <- if (!is.null(prop$id)) normalise_iri(prop$id, prefixes, base_iri) else NULL
          prop$path <- normalise_iri(prop$path, prefixes, base_iri)
          prop$nested$node <- if (!is.null(prop$nested$node)) normalise_iri(prop$nested$node, prefixes, base_iri) else NULL
          prop$nested$or   <- normalise_iri(prop$nested$or, prefixes, base_iri)
          prop$nested$and  <- normalise_iri(prop$nested$and, prefixes, base_iri)
          prop$nested$xone <- normalise_iri(prop$nested$xone, prefixes, base_iri)
          prop
        }
      )
    }
  }

  structure(out, class = c("sh_node_shape", "sh_shape", "list"))
}
