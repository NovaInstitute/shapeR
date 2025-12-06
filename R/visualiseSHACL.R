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
#'
#' prefixes <- c(
#'   ex   = "http://example.com/ns#",
#'   base = "http://example.com/base#"
#' )
#' visualiseSHACL(
#'   read_shacl(
#'     system.file("extdata", "visualise-shacl.ttl", package = "shapeR"),
#'     base_iri = "http://example.com/base#",
#'     prefixes = prefixes,
#'     normalise_iris = TRUE
#'   )
#' )
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
    shape_graph <- read_shacl(check_scalar_character(file, "file"))
  } else if (inherits(file, "sh_shape_graph")) {
    shape_graph <- file
  } else {
    stop("`file` must be a file path or a `sh_shape_graph` object.", call. = FALSE)
  }

  layout <- check_scalar_character(layout, "layout")

  prefixes <- shape_graph$prefixes %||% character()
  base_iri <- shape_graph$base_iri %||% NULL

  format_iri <- function(x) {
    if (is.null(x)) return(NULL)
    contract_iri(x, prefixes, base_iri)
  }

  clean_id <- function(x) {
    strip_angle_brackets(as.character(x))
  }

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

    class_constraint <- get_param(shape$constraints, "class")
    in_constraint    <- get_param(shape$constraints, "in")
    or_constraint    <- get_param(shape$constraints, "or")
    and_constraint   <- get_param(shape$constraints, "and")
    xone_constraint  <- get_param(shape$constraints, "xone")

    base_label <- if (length(target)) {
      paste0(format_iri(shape$id), " \u2192 ", format_iri(target[[1L]]))
    } else {
      format_iri(shape$id)
    }

    shape_label <- paste(
      stats::na.omit(c(
        base_label,
        if (!is.null(class_constraint)) paste0("class: ", paste(format_iri(as.character(class_constraint)), collapse = ", ")) else NA_character_,
        if (!is.null(in_constraint)   && length(in_constraint))   paste0("in: ",   paste(format_iri(as.character(in_constraint)), collapse = ", ")) else NA_character_,
        if (!is.null(or_constraint)   && length(or_constraint))   paste0("or: ",   paste(format_iri(as.character(or_constraint)), collapse = ", ")) else NA_character_,
        if (!is.null(and_constraint)  && length(and_constraint))  paste0("and: ",  paste(format_iri(as.character(and_constraint)), collapse = ", ")) else NA_character_,
        if (!is.null(xone_constraint) && length(xone_constraint)) paste0("xone: ", paste(format_iri(as.character(xone_constraint)), collapse = ", ")) else NA_character_
      )),
      collapse = "\n"
    )

    shape_rows[[length(shape_rows) + 1L]] <- tibble::tibble(
      node  = shape$id,
      label = shape_label
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
      and_vals  <- prop$nested$and
      xone_vals <- prop$nested$xone
      node_val  <- prop$nested$node

      prop_id <- if (!is.null(prop$id) && !startsWith(prop$id, "_:") ) {
        prop$id
      } else {
        paste0(shape$id, "::", prop$path)
      }

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
        format_iri(prop$path),
        card,
        if (!is.null(datatype)) format_iri(datatype) else NA_character_,
        if (!is.null(class_val)) paste0("class: ", format_iri(class_val)) else NA_character_,
        if (!is.null(in_vals) && length(in_vals)) {
          paste0("in: ", paste(format_iri(as.character(in_vals)), collapse = ", "))
        } else {
          NA_character_
        },
        if (!is.null(or_vals) && length(or_vals)) {
          paste0("or: ", paste(format_iri(as.character(or_vals)), collapse = ", "))
        } else {
          NA_character_
        },
        if (!is.null(and_vals) && length(and_vals)) {
          paste0("and: ", paste(format_iri(as.character(and_vals)), collapse = ", "))
        } else {
          NA_character_
        },
        if (!is.null(xone_vals) && length(xone_vals)) {
          paste0("xone: ", paste(format_iri(as.character(xone_vals)), collapse = ", "))
        } else {
          NA_character_
        },
        if (!is.null(node_val)) paste0("node: ", format_iri(node_val)) else NA_character_
      ))

      prop_rows[[length(prop_rows) + 1L]] <- tibble::tibble(
        node  = prop_id,
        label = paste(label_parts, collapse = "\n")
      )

      edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
        from = shape$id,
        to   = prop_id,
        relation = "property"
      )

      if (!is.null(node_val)) {
        edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
          from = prop_id,
          to   = node_val,
          relation = "node"
        )
      }

      if (length(or_vals)) {
        for (target in or_vals) {
          edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
            from = prop_id,
            to   = target,
            relation = "or"
          )
        }
      }

      if (length(and_vals)) {
        for (target in and_vals) {
          edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
            from = prop_id,
            to   = target,
            relation = "and"
          )
        }
      }

      if (length(xone_vals)) {
        for (target in xone_vals) {
          edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
            from = prop_id,
            to   = target,
            relation = "xone"
          )
        }
      }
    }

    if (length(or_constraint)) {
      for (target in or_constraint) {
        edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
          from = shape$id,
          to   = target,
          relation = "or"
        )
      }
    }

    if (length(and_constraint)) {
      for (target in and_constraint) {
        edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
          from = shape$id,
          to   = target,
          relation = "and"
        )
      }
    }

    if (length(xone_constraint)) {
      for (target in xone_constraint) {
        edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
          from = shape$id,
          to   = target,
          relation = "xone"
        )
      }
  }
  }

  nodes <- dplyr::bind_rows(c(shape_rows, prop_rows)) |>
    dplyr::distinct(node, .keep_all = TRUE) |>
    dplyr::mutate(node = clean_id(node))

  edges <- if (length(edge_rows)) {
    dplyr::bind_rows(edge_rows) |>
      dplyr::mutate(
        from = clean_id(from),
        to   = clean_id(to)
      )
  } else {
    tibble::tibble(from = character(), to = character(), relation = character())
  }

  graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
  graph <- graph |>
    tidygraph::activate(edges) |>
    dplyr::mutate(
      from_label = unname(nodes$node[from]),
      to_label   = unname(nodes$node[to])
    )

  p <- ggraph::ggraph(graph, layout = layout) +
    ggraph::geom_edge_link(
      arrow = grid::arrow(length = grid::unit(3, "mm"))
    ) +
    ggraph::geom_node_label(ggplot2::aes(label = label)) +
    ggplot2::theme_void()

  print(p)
  invisible(graph)
}
