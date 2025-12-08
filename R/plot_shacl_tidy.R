#' Plot a tidy SHACL tibble
#'
#' Create a node–edge diagram from the tidy tibble produced by
#' [read_shacl_tidy()]. Shapes are connected to their property constraints, and
#' nested shapes referenced by `sh:node`, `sh:or`, `sh:and`, or `sh:xone` are
#' linked from the relevant property node. Shapes with multiple target classes
#' are supported and will display each target in the shape label.
#'
#' @param shapes A tidy SHACL tibble, typically returned by
#'   [read_shacl_tidy()].
#' @param layout Layout name passed to [ggraph::ggraph()] when `engine` is
#'   "ggraph". Ignored for `engine = "visNetwork"`.
#' @param engine Visualisation engine to use. Choose "ggraph" for static plots
#'   or "visNetwork" for an interactive widget.
#'
#' @examples
#' tidy_shapes <- read_shacl_tidy(
#'   "https://raw.githubusercontent.com/IndependentImpact/Bhash/refs/heads/main/ontology/shapes/consensus.shacl.ttl",
#'   prefer_curie = TRUE
#' )
#' tidy_shapes$property <- lapply(tidy_shapes$property, clean_property_constraints)
#' plot_shacl_tidy(tidy_shapes, engine = "visNetwork")
#' 
#' @return A `tidygraph::tbl_graph` object, returned invisibly. The plot is
#'   printed as a side effect.
#' @export
plot_shacl_tidy <- function(shapes, layout = "sugiyama", engine = c("ggraph", "visNetwork")) {

  engine <- match.arg(engine, c("ggraph", "visNetwork"))
  layout <- check_scalar_character(layout, "layout")

  if (!inherits(shapes, "data.frame")) {
    stop("`shapes` must be a data frame produced by `read_shacl_tidy()`.", call. = FALSE)
  }

  required_cols <- c("id", "property")
  if (!all(required_cols %in% names(shapes))) {
    stop("`shapes` must contain columns `id` and `property`.", call. = FALSE)
  }

  shapes$property <- lapply(shapes$property, clean_property_constraints)

  clean_id <- function(x) {
    strip_angle_brackets(as.character(x))
  }

  collapse_vals <- function(x) {
    vals <- unlist(x %||% character())
    vals[!is.na(vals) & nzchar(vals)]
  }

  format_prefixed <- function(label, vals) {
    vals <- collapse_vals(vals)
    if (!length(vals)) return(NA_character_)
    paste0(label, ": ", paste(vals, collapse = ", "))
  }

  prop_rows <- list()
  edge_rows <- list()
  shape_rows <- list()
  prop_detail_rows <- list()

  for (row_idx in seq_len(nrow(shapes))) {
    shape_id <- clean_id(shapes$id[[row_idx]])
    target_classes <- collapse_vals(shapes$targetClass[[row_idx]])

    base_shape_label <- if (length(target_classes)) {
      paste0(shape_id, " \u2192 ", paste(target_classes, collapse = ", "))
    } else {
      shape_id
    }

    shape_rows[[length(shape_rows) + 1L]] <- tibble::tibble(
      node = shape_id,
      label = base_shape_label
    )

    property_tbl <- shapes$property[[row_idx]]

    if (!inherits(property_tbl, "data.frame") || nrow(property_tbl) == 0L) {
      next
    }

    for (prop_idx in seq_len(nrow(property_tbl))) {
      prop <- property_tbl[prop_idx, , drop = FALSE]

      prop_id <- paste0(shape_id, "::", prop$path[[1L]])

      min_count <- prop$minCount[[1L]]
      max_count <- prop$maxCount[[1L]]

      card <- if (!all(is.na(c(min_count, max_count)))) {
        paste0(
          "[",
          if (!is.na(min_count)) min_count else "0",
          "..",
          if (!is.na(max_count)) max_count else "*",
          "]"
        )
      } else {
        NA_character_
      }

      label_parts <- stats::na.omit(c(
        prop$path[[1L]],
        card,
        format_prefixed("datatype", prop$datatype[[1L]]),
        format_prefixed("class", prop$class[[1L]]),
        format_prefixed("in", prop$`in`[[1L]]),
        format_prefixed("or", prop$or[[1L]]),
        format_prefixed("and", prop$and[[1L]]),
        format_prefixed("xone", prop$xone[[1L]]),
        format_prefixed("node", prop$node[[1L]])
      ))

      prop_rows[[length(prop_rows) + 1L]] <- tibble::tibble(
        node = clean_id(prop_id),
        label = paste(label_parts, collapse = "\n")
      )

      prop_detail <- clean_property_constraints(prop)

      if (nrow(prop_detail)) {
        prop_detail <- cbind(node = clean_id(prop_id), prop_detail, shape = shape_id)

        prop_detail[] <- lapply(prop_detail, function(col) {
          if (!is.list(col)) return(col)

          vapply(col, function(x) {
            vals <- collapse_vals(x)
            if (!length(vals)) return(NA_character_)
            paste(vals, collapse = ", ")
          }, character(1))
        })

        prop_detail_rows[[length(prop_detail_rows) + 1L]] <- prop_detail
      }

      edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
        from = shape_id,
        to = clean_id(prop_id),
        relation = "property"
      )

      if (!is.null(prop$node[[1L]]) && length(prop$node[[1L]]) && !is.na(prop$node[[1L]])) {
        edge_rows[[length(edge_rows) + 1L]] <- tibble::tibble(
          from = clean_id(prop_id),
          to = clean_id(prop$node[[1L]]),
          relation = "node"
        )
      }

      add_relation_edges <- function(targets, relation) {
        targets <- collapse_vals(targets)
        if (!length(targets)) return()

        for (target in targets) {
          edge_rows[[length(edge_rows) + 1L]] <<- tibble::tibble(
            from = clean_id(prop_id),
            to = clean_id(target),
            relation = relation
          )
        }
      }

      add_relation_edges(prop$or[[1L]], "or")
      add_relation_edges(prop$and[[1L]], "and")
      add_relation_edges(prop$xone[[1L]], "xone")
    }
  }

  nodes <- dplyr::bind_rows(c(shape_rows, prop_rows)) |>
    dplyr::distinct(node, .keep_all = TRUE)

  edges <- if (length(edge_rows)) {
    dplyr::bind_rows(edge_rows)
  } else {
    tibble::tibble(from = character(), to = character(), relation = character())
  }

  graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE) |>
    tidygraph::activate(edges) |>
    dplyr::mutate(
      from_label = nodes$node[from],
      to_label = nodes$node[to]
    )

  if (engine == "visNetwork") {
    if (!requireNamespace("visNetwork", quietly = TRUE)) {
      stop("The `visNetwork` package is required for `engine = 'visNetwork'`.", call. = FALSE)
    }

    prop_details <- if (length(prop_detail_rows)) {
      dplyr::bind_rows(prop_detail_rows)
    } else {
      tibble::tibble()
    }

    nodes_df <- nodes |>
      dplyr::transmute(
        id = node,
        label = label,
        title = gsub("\n", "<br>", label)
      ) |>
      dplyr::left_join(prop_details, by = c("id" = "node"))

    edges_df <- edges |>
      dplyr::mutate(
        label = relation,
        arrows = "to"
      )

    widget <- visNetwork::visNetwork(nodes_df, edges_df) |>
      visNetwork::visEdges(smooth = FALSE) |>
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)

    print(widget)
  } else {
    p <- ggraph::ggraph(graph, layout = layout) +
      ggraph::geom_edge_link(
        arrow = grid::arrow(length = grid::unit(3, "mm"))
      ) +
      ggraph::geom_node_label(ggplot2::aes(label = label)) +
      ggplot2::theme_void()

    print(p)
  }

  invisible(graph)
}

