#' Construct a SHACL shape graph
#'
#' A shape graph is a collection of SHACL shapes (node shapes, property shapes,
#' and potentially other kinds of shapes in the future) plus optional metadata.
#'
#' @param shapes A named list of shape objects, typically \code{sh_node_shape}
#'   instances. Names should be the shape identifiers (IRIs or blank-node IDs).
#' @param prefixes Optional named character vector of prefix mappings, such as
#'   \code{c(ex = "http://example.com/")}. Can be \code{NULL}.
#' @param base_iri Optional base IRI to use when resolving relative IRIs.
#' @param metadata Optional list for graph-level metadata or annotations.
#'
#' @return An object of class \code{"sh_shape_graph"}.
#'
#' @examples
#' sg <- sh_shape_graph(list())
#'
#' @export
sh_shape_graph <- function(shapes   = list(),
                           prefixes = NULL,
                           base_iri = NULL,
                           metadata = list()) {

  if (!is.list(shapes)) {
    stop("`shapes` must be a list.", call. = FALSE)
  }

  structure(
    list(
      shapes   = shapes,
      prefixes = prefixes,
      base_iri = base_iri,
      metadata = metadata
    ),
    class = c("sh_shape_graph", "list")
  )
}


print.sh_shape_graph <- function(x, ...) {
  cat("SHACL shape graph with", length(x$shapes), "shapes\n")
  invisible(x)
}