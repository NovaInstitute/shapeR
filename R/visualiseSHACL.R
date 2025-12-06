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
#' @param file Path to the SHACL file (Turtle, RDF/XML, JSON-LD, etc.).
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
#' @importFrom rdflib rdf_parse rdf_query
#' @importFrom dplyr select filter mutate transmute group_by summarise
#' @importFrom dplyr left_join distinct if_else bind_rows rowwise ungroup
#' @importFrom stringr str_detect
#' @importFrom tibble tibble
#' @importFrom tidygraph tbl_graph
#' @importFrom ggraph ggraph geom_edge_link geom_node_label
#' @importFrom ggplot2 aes theme_void
#' @importFrom grid arrow unit
#' @importFrom stats na.omit
#' @export
visualiseSHACL <- function(file, layout = "sugiyama") {

  # ---- 1. Parse SHACL RDF ------------------------------------------------
  rdf <- rdflib::rdf_parse(file)

  triples <- rdflib::rdf_query(
    rdf,
    "SELECT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object }"
  ) |>
    dplyr::select(subject, predicate, object)

  # ---- 2. sh:property and sh:node links ----------------------------------
  prop_triples <- triples |>
    dplyr::filter(stringr::str_detect(predicate, "shacl#property"))

  if (nrow(prop_triples) == 0L) {
    stop("No sh:property triples found — is this a SHACL shapes graph?", call. = FALSE)
  }

  node_triples <- triples |>
    dplyr::filter(stringr::str_detect(predicate, "shacl#node"))

  # Shapes: anything that has sh:property OR is the target of sh:node
  shape_ids     <- unique(c(prop_triples$subject, node_triples$object))
  prop_node_ids <- unique(prop_triples$object)

  # ---- 3. Extract details about property shapes --------------------------
  prop_details <- triples |>
    dplyr::filter(subject %in% prop_node_ids) |>
    dplyr::mutate(
      pred_role = dplyr::case_when(
        stringr::str_detect(predicate, "shacl#path")     ~ "path",
        stringr::str_detect(predicate, "shacl#datatype") ~ "datatype",
        stringr::str_detect(predicate, "shacl#minCount") ~ "minCount",
        stringr::str_detect(predicate, "shacl#maxCount") ~ "maxCount",
        stringr::str_detect(predicate, "shacl#node")     ~ "node",
        stringr::str_detect(predicate, "shacl#class")    ~ "class",
        stringr::str_detect(predicate, "shacl#in")       ~ "in",
        stringr::str_detect(predicate, "shacl#or")       ~ "or",
        TRUE                                             ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(pred_role)) |>
    dplyr::group_by(subject) |>
    dplyr::summarise(
      path     = first_or_na(object[pred_role == "path"]),
      datatype = first_or_na(object[pred_role == "datatype"]),
      minCount = first_or_na(object[pred_role == "minCount"]),
      maxCount = first_or_na(object[pred_role == "maxCount"]),
      node     = first_or_na(object[pred_role == "node"]),
      class    = first_or_na(object[pred_role == "class"]),
      in_head  = first_or_na(object[pred_role == "in"]),
      or_head  = first_or_na(object[pred_role == "or"]),
      .groups  = "drop"
    )

  # ---- 4. Build property node labels -------------------------------------
  prop_labels <- prop_details |>
    dplyr::rowwise() |>
    dplyr::mutate(
      base = if (!is.na(path)) shorten_iri(path) else shorten_iri(subject),
      card = if (!is.na(minCount) | !is.na(maxCount)) {
        paste0(
          "[",
          if (!is.na(minCount)) minCount else "0",
          "..",
          if (!is.na(maxCount)) maxCount else "*",
          "]"
        )
      } else {
        NA_character_
      },
      dt        = if (!is.na(datatype)) shorten_iri(datatype) else NA_character_,
      class_str = if (!is.na(class)) {
        paste0("class: ", shorten_iri(class))
      } else {
        NA_character_
      },
      in_str = if (!is.na(in_head)) {
        list_to_label(in_head, triples, "in")
      } else {
        NA_character_
      },
      or_str = if (!is.na(or_head)) {
        list_to_label(or_head, triples, "or")
      } else {
        NA_character_
      },
      node_str = if (!is.na(node)) {
        paste0("node: ", shorten_iri(node))
      } else {
        NA_character_
      },
      label = paste(
        stats::na.omit(c(base, card, dt, class_str, in_str, or_str, node_str)),
        collapse = "\n"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(node = subject, label)

  # ---- 5. Shape node labels via sh:targetClass ---------------------------
  target_triples <- triples |>
    dplyr::filter(
      subject %in% shape_ids,
      stringr::str_detect(predicate, "shacl#targetClass")
    ) |>
    dplyr::select(node = subject, target = object)

  shape_labels <- tibble::tibble(node = shape_ids) |>
    dplyr::left_join(target_triples, by = "node") |>
    dplyr::mutate(
      label = dplyr::if_else(
        !is.na(target),
        paste0(shorten_iri(node), " \u2192 ", shorten_iri(target)),
        shorten_iri(node)
      )
    ) |>
    dplyr::select(node, label)

  # ---- 6. Combine nodes and edges ----------------------------------------
  nodes <- dplyr::bind_rows(shape_labels, prop_labels) |>
    dplyr::distinct(node, .keep_all = TRUE)

  # edges: shape -> propertyShape (sh:property)
  edges_prop <- prop_triples |>
    dplyr::transmute(from = subject, to = object)

  # edges: propertyShape -> nested shape (sh:node)
  edges_node <- node_triples |>
    dplyr::transmute(from = subject, to = object)

  edges <- dplyr::bind_rows(edges_prop, edges_node)

  graph <- tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

  # ---- 7. Plot with ggraph ------------------------------------------------
  p <- ggraph::ggraph(graph, layout = layout) +
    ggraph::geom_edge_link(
      arrow = grid::arrow(length = grid::unit(3, "mm"))
    ) +
    ggraph::geom_node_label(ggplot2::aes(label = label)) +
    ggplot2::theme_void()

  print(p)
  invisible(graph)
}
