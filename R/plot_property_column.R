#' Plot tidy SHACL property constraints with visNetwork
#'
#' Convenience wrapper for visualising the `property` list-column produced by
#' [read_shacl_tidy()] as an interactive network. The function keeps the
#' shape-level context but focuses on rendering via `visNetwork`, making it easy
#' to inspect property constraints extracted from a SHACL shapes graph.
#'
#' @param shapes A tidy SHACL tibble returned by [read_shacl_tidy()]. Must
#'   include columns `id` and `property`.
#' @param shape_id Optional character vector of shape identifiers to include.
#'   When supplied, only the matching shapes and their property constraints are
#'   plotted.
#'
#' @return A `tidygraph::tbl_graph` object, returned invisibly. The associated
#'   `visNetwork` widget is printed as a side effect.
#'
#' @examples
#' \dontrun{
#' shapes <- data.frame(
#'   id = "ex:PersonShape",
#'   property = I(list(list(
#'     path = "ex:name",
#'     constraints = list(list(component = "sh:datatype", datatype = "xsd:string"))
#'   ))),
#'   stringsAsFactors = FALSE
#' )
#'
#' plot_property_column(shapes)
#' }
#' @export
plot_property_column <- function(shapes, shape_id = NULL) {

  if (!inherits(shapes, "data.frame")) {
    stop("`shapes` must be a data frame produced by `read_shacl_tidy()`.", call. = FALSE)
  }

  if (!all(c("id", "property") %in% names(shapes))) {
    stop("`shapes` must include columns `id` and `property`.", call. = FALSE)
  }

  if (!is.null(shape_id)) {
    shape_id <- vapply(shape_id, check_scalar_character, character(1), arg_name = "shape_id")
    shapes <- shapes[shapes$id %in% shape_id, , drop = FALSE]

    if (!nrow(shapes)) {
      stop("No shapes found matching the supplied `shape_id`.", call. = FALSE)
    }
  }

  plot_shacl_tidy(shapes, engine = "visNetwork")
}

