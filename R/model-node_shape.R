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
  if (!is.character(id) || length(id) != 1L || is.na(id)) {
    stop("`id` must be a non-NA scalar character string.", call. = FALSE)
  }

  # targets: list of character vectors
  if (!is.list(targets)) {
    stop("`targets` must be a list.", call. = FALSE)
  }

  targetClass      <- targets$targetClass      %||% character()
  targetNode       <- targets$targetNode       %||% character()
  targetSubjectsOf <- targets$targetSubjectsOf %||% character()
  targetObjectsOf  <- targets$targetObjectsOf  %||% character()

  for (v in list(
    targetClass      = targetClass,
    targetNode       = targetNode,
    targetSubjectsOf = targetSubjectsOf,
    targetObjectsOf  = targetObjectsOf
  )) {
    if (!is.character(v) || anyNA(v)) {
      stop("All entries in `targets` must be character vectors without NA.",
           call. = FALSE)
    }
  }

  # properties: list of sh_property_shape
  if (!is.list(properties)) {
    stop("`properties` must be a list.", call. = FALSE)
  }
  if (length(properties) > 0L) {
    ok <- vapply(properties, inherits, logical(1), "sh_property_shape")
    if (!all(ok)) {
      stop("All elements of `properties` must be of class 'sh_property_shape'.",
           call. = FALSE)
    }
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

  # annotations
  if (!is.list(annotations)) {
    stop("`annotations` must be a list.", call. = FALSE)
  }

  # severity
  if (!is.null(severity) &&
      (!is.character(severity) || length(severity) != 1L || is.na(severity))) {
    stop("`severity` must be NULL or a non-NA scalar character string.",
         call. = FALSE)
  }

  # deactivated
  if (!is.logical(deactivated) || length(deactivated) != 1L || is.na(deactivated)) {
    stop("`deactivated` must be a non-NA logical scalar.", call. = FALSE)
  }

  # extras
  if (!is.list(extras)) {
    stop("`extras` must be a list.", call. = FALSE)
  }

  prefixes <- prefixes %||% character()

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
