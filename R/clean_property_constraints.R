#' Clean a tidy property constraints tibble
#'
#' Removes duplicate rows, drops columns that are completely empty, and
#' simplifies nested columns that only ever contain a single value per row. The
#' function is intended for the `property` list-column returned by
#' [read_shacl_tidy()] but also works on standalone property tibbles.
#'
#' @param property A tibble describing property constraints for a shape.
#'
#' @return A cleaned tibble with duplicates removed, empty columns dropped, and
#'   one-value list columns unnested to atomic vectors.
#'
#' @examples
#' tidy_shapes <- read_shacl_tidy(
#'   "https://raw.githubusercontent.com/IndependentImpact/Bhash/refs/heads/main/ontology/shapes/consensus.shacl.ttl",
#'   prefer_curie = TRUE
#' )
#'
#' tidy_shapes$property <- lapply(tidy_shapes$property, clean_property_constraints)
#' tidy_shapes$property[[1]]
#'
#' @importFrom dplyr distinct
#' @export
clean_property_constraints <- function(property) {

  if (is.null(property)) return(property)

  if (!inherits(property, "data.frame")) {
    stop("`property` must be a data frame.", call. = FALSE)
  }

  if (!nrow(property)) return(property)

  property <- dplyr::distinct(property)

  has_data <- vapply(property, column_has_content, logical(1))
  property <- property[, has_data, drop = FALSE]

  unnest_single_value_lists(property)
}

column_has_content <- function(col) {
  if (is.list(col)) {
    any(vapply(col, list_entry_has_content, logical(1)))
  } else {
    any(!is.na(col))
  }
}

list_entry_has_content <- function(x) {
  if (is.null(x)) return(FALSE)
  if (length(x) == 0L) return(FALSE)
  !all(is.na(x))
}

unnest_single_value_lists <- function(df) {
  for (nm in names(df)) {
    col <- df[[nm]]

    if (!is.list(col) || !length(col)) {
      next
    }

    sizes <- vapply(col, function(x) if (is.null(x)) 0L else length(x), integer(1))

    if (!all(sizes <= 1L)) {
      next
    }

    prototype <- find_first_value(col)

    if (is.null(prototype)) {
      df[[nm]] <- vapply(col, function(x) NA_character_, character(1))
      next
    }

    template <- if (is.integer(prototype)) {
      integer(1)
    } else if (is.numeric(prototype)) {
      numeric(1)
    } else if (is.logical(prototype)) {
      logical(1)
    } else {
      character(1)
    }

    df[[nm]] <- vapply(col, function(x) {
      if (is.null(x) || length(x) == 0L || all(is.na(x))) {
        return(NA)
      }

      x[[1L]]
    }, template)
  }

  df
}

find_first_value <- function(col) {
  for (x in col) {
    if (!is.null(x) && length(x) && !all(is.na(x))) {
      return(x[[1L]])
    }
  }

  NULL
}
