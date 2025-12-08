#' Read SHACL shapes into a tidy tibble
#'
#' Parses a SHACL shapes graph from RDF and returns it in the tidy tibble layout
#' described in the "SHACL in the Tidyverse" vignette. Each row represents a
#' node shape; columns capture top-level SHACL terms and property constraints as
#' list-columns.
#'
#' @inheritParams read_shacl
#' @param prefer_curie Logical; when `TRUE`, identifiers are contracted to
#'   CURIEs using the supplied prefixes/base IRI when possible. When `FALSE`,
#'   identifiers are returned without angle brackets but are otherwise left
#'   unchanged.
#'
#' @return A tibble with columns `id`, `type`, `context`, `targetClass`, and
#'   `property`. The `context` column stores prefix mappings as a tibble, while
#'   `property` stores a tibble of property constraints for each node shape.
#'
#' @examples
#' \dontrun{
#' tidy_shapes <- read_shacl_tidy(
#'   "Shapes/independent-impact-shapes.ttl",
#'   prefixes = c(ex = "http://example.com/"),
#'   prefer_curie = TRUE
#' )
#' }
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
read_shacl_tidy <- function(file,
                            base_iri = NULL,
                            prefixes = NULL,
                            normalise_iris = FALSE,
                            prefer_curie = TRUE) {

  sg <- read_shacl(
    file = file,
    base_iri = base_iri,
    prefixes = prefixes,
    normalise_iris = normalise_iris
  )

  prefix_map <- sg$prefixes %||% prefixes %||% character()
  base_for_contract <- sg$base_iri %||% base_iri

  convert_chars <- function(x) {
    if (!is.character(x)) return(x)

    if (prefer_curie) {
      contract_iri(x, prefixes = prefix_map, base_iri = base_for_contract)
    } else {
      strip_angle_brackets(x)
    }
  }

  context_tbl <- tibble::tibble(
    prefix = names(prefix_map),
    iri = unname(prefix_map)
  )

  make_property_row <- function(prop) {
    constraints <- prop$constraints %||% list()

    get_param <- function(name, default = NULL) {
      vals <- lapply(constraints, function(c) c$params[[name]])
      vals <- vals[!vapply(vals, is.null, logical(1))]
      if (!length(vals)) return(default)
      vals[[1L]]
    }

    coerce_int_param <- function(val) {
      if (is.null(val) || !length(val) || all(is.na(val))) {
        return(NA_integer_)
      }

      suppressWarnings({
        as.integer(strip_angle_brackets(val)[[1L]])
      })
    }

    min_count <- coerce_int_param(get_param("minCount"))
    max_count <- coerce_int_param(get_param("maxCount"))

    list(
      path = convert_chars(prop$path),
      datatype = convert_chars(get_param("datatype", NA_character_)),
      class = convert_chars(get_param("class", NA_character_)),
      minCount = min_count,
      maxCount = max_count,
      `in` = list(convert_chars(get_param("in", character()))),
      node = convert_chars(prop$nested$node %||% NA_character_),
      or = list(convert_chars(prop$nested$or %||% character())),
      and = list(convert_chars(prop$nested$and %||% character())),
      xone = list(convert_chars(prop$nested$xone %||% character()))
    )
  }

  property_cols <- list(
    path = character(),
    datatype = character(),
    class = character(),
    minCount = integer(),
    maxCount = integer(),
    `in` = vector("list", 0L),
    node = character(),
    or = vector("list", 0L),
    and = vector("list", 0L),
    xone = vector("list", 0L)
  )

  shape_rows <- lapply(
    sg$shapes,
    function(shape) {
      property_rows <- lapply(shape$properties, make_property_row)
      property_tbl <- if (length(property_rows)) {
        dplyr::bind_rows(property_rows)
      } else {
        do.call(tibble::tibble, property_cols)
      }

      tibble::tibble(
        id = convert_chars(shape$id),
        type = "sh:NodeShape",
        context = list(context_tbl),
        targetClass = list(convert_chars(shape$targets$targetClass %||% character())),
        property = list(property_tbl)
      )
    }
  )

  dplyr::bind_rows(shape_rows)
}
