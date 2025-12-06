#' Visualise a SHACL shapes graph
#'
#' Parses a SHACL shapes graph (RDF) and produces a node–edge diagram
#' showing shapes, their property shapes, and nested shapes via \code{sh:node}.
#'
#' Property nodes are labelled with:
#' \itemize{
#'   \item \code{sh:path}
#'   \item cardinality \code{[min..max]}
#'   \item \code{sh:datatype}
#'   \item \code{sh:class}
#'   \item \code{sh:in} lists (values)
#'   \item \code{sh:or} lists (alternative shapes)
#' }
#'
#' Shape nodes are labelled with their shape ID and, if present,
#' \code{sh:targetClass}.
#'
#' @param file Either a path to a SHACL file (Turtle, RDF/XML, JSON-LD, etc.) or
#'   an object of class \code{sh_shape_graph} produced by \code{read_shacl()}.
#' @param layout Layout name passed to \code{ggraph} (e.g. "sugiyama", "fr").
#'
#' @return A \code{tidygraph::tbl_graph} object, returned invisibly.
#'   The diagram is plotted as a side effect.
#'
#' @examples
#' \dontrun{
#' visualiseSHACL("inst/extdata/shapes.ttl")
#' }
#'
#' @importFrom dplyr bind_rows distinct
#' @importFrom tibble tibble
#' @importFrom tidygraph tbl_graph
#' @importFrom ggraph ggraph geom_edge_link geom_node_label
#' @importFrom ggplot2 aes theme_void
#' @importFrom grid arrow unit
#' @importFrom stats na.omit
#' @export
visualiseSHACL <- function(file, layout = "sugiyama") {

  if (is.character(file) && length(file) == 1L) {
    shape_graph <- read_shacl(file)
  } else if (inherits(file, "sh_shape_graph")) {
    shape_graph <- file
  } else {
    stop("`file` must be a file path or a `sh_shape_graph` object.", call. = FALSE)
  }

  prefixes <- shape_graph$prefixes %||% character()

  node_shapes <- shape_graph$shapes

  if (length(node_shapes) == 0L) {
    stop("No shapes found in the supplied shape graph.", call. = FALSE)
  }

  get_param <- function(constraints, param_name) {
    for (c in constraints) {
      if (param_name %in% names(c$params)) {
        return(c$params[[param_name]])
      }
    }
    NULL
  }

  prop_rows <- list()
  edge_rows <- list()
  shape_rows <- list()

  for (shape in node_shapes) {
    target <- shape$targets$targetClass
    label <- if (length(target)) {
      paste0(shorten_iri(shape$id, prefixes), " \u2192 ", shorten_iri(target[[1L]], prefixes))
    } else {
      shorten_iri(shape$id, prefixes)
    }

    shape_rows[[length(shape_rows) + 1L]] <- tibble::tibble(
      node  = shape$id,
      label = label
    )

    if (length(shape$properties) == 0L) {
      next
    }

    for (prop in shape$properties) {
      min_count <- get_param(prop$constraints, "minCount")
      max_count <- get_param(prop$constraints, "maxCount")
      datatype  <- get_param(prop$constraints, "datatype")
      class_val <- get_param(prop$constraints, "class")
      in_vals   <- get_param(prop$constraints, "in")
      or_vals   <- prop$nested$or
      node_val  <- prop$nested$node

      prop_id <- prop$id %||% paste0(shape$id, "::", prop$path)

      card <- if (!is.null(min_count) || !is.null(max_count)) {
        paste0(
          "[",
          if (!is.null(min_count)) min_count else "0",
          "..",
          if (!is.null(max_count)) max_count else "*",
          "]"
        )
      } else {
        NA_character_
      }

      label_parts <- stats::na.omit(c(
        shorten_iri(prop$path, prefixes),
        card,
        if (!is.null(datatype)) shorten_iri(datatype, prefixes) else NA_character_,
        if (!is.null(class_val)) paste0("class: ", shorten_iri(class_val, prefixes)) else NA_character_,
        if (!is.null(in_vals) && length(in_vals)) {
          paste0("in: ", paste(shorten_iri(as.character(in_vals), prefixes), collapse = ", "))
        } else {
          NA_character_
        },
        if (!is.null(or_vals) && length(or_vals)) {
          paste0("or: ", paste(shorten_iri(as.character(or_vals), prefixes), collapse = ", "))
        } else {
          NA_character_
        },
        if (!is.null(node_val)) paste0("node: ", shorten_iri(node_val, prefixes)) else NA_character_
      ))

      prop_rows[[length(prop_rows) + 1L]] <- tibble::tibble(
        node  = prop_id,
        label = paste(label_parts, collapse = "\n")
      )

      edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
        from = shape$id,
        to   = prop_id
      )

      if (!is.null(node_val)) {
        edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
          from = prop_id,
          to   = node_val
        )
      }
    }
  }

  nodes <- dplyr::bind_rows(c(shape_rows, prop_rows)) |>
    dplyr::distinct(node, .keep_all = TRUE)

  edges <- if (length(edge_rows)) {
    dplyr::bind_rows(edge_rows)
  } else {
    tibble::tibble(from = character(), to = character())
  }

  graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

  p <- ggraph::ggraph(graph, layout = layout) +
    ggraph::geom_edge_link(
      arrow = grid::arrow(length = grid::unit(3, "mm"))
    ) +
    ggraph::geom_node_label(ggplot2::aes(label = label)) +
    ggplot2::theme_void()

  print(p)
  invisible(graph)
}
