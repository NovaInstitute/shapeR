#' IRI helpers for prefix expansion and normalisation
#'
#' Utilities to expand CURIEs or relative IRIs, resolve them against a base,
#' and optionally contract them back to compact CURIEs. These functions
#' complement [shorten_iri()].

#' @keywords internal
strip_angle_brackets <- function(x) {
  vapply(
    x,
    function(val) {
      if (is.na(val)) {
        return(val)
      }
      val_chr <- as.character(val)

      if (startsWith(val_chr, "<") && endsWith(val_chr, ">")) {
        substring(val_chr, 2L, nchar(val_chr) - 1L)
      } else {
        val_chr
      }
    },
    character(1)
  )
}

#' @keywords internal
add_angle_brackets <- function(x) {
  unname(vapply(
    x,
    function(val) {
      if (is.na(val)) {
        return(val)
      }
      val_chr <- as.character(val)

      if (!is.character(val) || startsWith(val_chr, "<") || startsWith(val_chr, "_:")) {
        return(val_chr)
      }
      if (endsWith(val_chr, ">")) {
        return(val_chr)
      }
      paste0("<", val_chr, ">")
    },
    character(1)
  ))
}

#' Identify CURIE-like identifiers
#'
#' @param x Character vector to inspect.
#' @return Logical vector marking elements that look like CURIEs.
#' @export
is_curie <- function(x) {
  if (!is.character(x)) return(rep(FALSE, length(x)))
  grepl("^[A-Za-z_][A-Za-z0-9._-]*:[^/].*", x) & !grepl("^[A-Za-z][A-Za-z0-9+.-]*://", x)
}

#' Resolve a relative IRI against a base
#'
#' @param iri Character vector of IRIs or relative references.
#' @param base_iri Base IRI to prepend when needed.
#' @return Character vector with absolute IRIs wrapped in angle brackets.
#' @export
resolve_base_iri <- function(iri, base_iri = NULL) {
  if (is.null(base_iri)) return(add_angle_brackets(iri))

  base_clean <- strip_angle_brackets(base_iri)

  unname(vapply(
    iri,
    function(x) {
      if (is.na(x)) return(x)

      cleaned <- strip_angle_brackets(x)

      if (startsWith(cleaned, "_:") || grepl("^[A-Za-z][A-Za-z0-9+.-]*://", cleaned)) {
        return(add_angle_brackets(cleaned))
      }

      paste0("<", base_clean, cleaned, ">")
    },
    character(1)
  ))
}

#' Expand CURIEs using a prefix map and base IRI
#'
#' @param iri Character vector of CURIEs, relative or absolute IRIs.
#' @param prefixes Named character vector mapping prefix labels to namespace IRIs.
#' @param base_iri Optional base IRI for resolving relative references.
#' @return Character vector of expanded IRIs.
#' @export
expand_iri <- function(iri, prefixes = character(), base_iri = NULL) {
  prefixes_clean <- strip_angle_brackets(prefixes)

  expanded <- vapply(
    iri,
    function(x) {
      if (is.na(x)) return(NA_character_)

      cleaned <- strip_angle_brackets(x)

      if (is_curie(cleaned) && cleaned != "" && length(prefixes_clean) > 0L) {
        pref <- sub(":.*$", "", cleaned)
        local <- sub("^[^:]*:", "", cleaned)
        if (pref %in% names(prefixes_clean)) {
          cleaned <- paste0(prefixes_clean[[pref]], local)
        }
      }

      cleaned
    },
    character(1)
  )

  add_angle_brackets(resolve_base_iri(expanded, base_iri))
}

#' Contract expanded IRIs using a prefix map
#'
#' @param iri Character vector of IRIs to contract.
#' @param prefixes Named character vector mapping prefix labels to namespace IRIs.
#' @param base_iri Optional base IRI used for relative resolution.
#' @return Character vector of contracted CURIEs when possible.
#' @export
contract_iri <- function(iri, prefixes = character(), base_iri = NULL) {
  cleaned <- strip_angle_brackets(iri)
  shortened <- shorten_iri(cleaned, prefixes)
  if (is.null(base_iri)) return(shortened)

  base_clean <- strip_angle_brackets(base_iri)
  unname(vapply(
    shortened,
    function(x) {
      if (is.na(x)) return(x)
      if (startsWith(x, base_clean)) {
        prefix_name <- names(which(prefixes == base_clean))[1L]
        replacement <- if (!is.na(prefix_name)) prefix_name else base_clean
        paste0(replacement, substring(x, nchar(base_clean) + 1L))
      } else {
        x
      }
    },
    character(1)
  ))
}

#' Normalise IRIs through expansion and optional contraction
#'
#' @param iri Character vector to normalise.
#' @param prefixes Named character vector mapping prefix labels to namespace IRIs.
#' @param base_iri Optional base IRI used for relative resolution.
#' @param prefer_curie Logical; if `TRUE`, attempt to contract to CURIEs after
#'   expansion.
#' @return Character vector of normalised IRIs or CURIEs.
#' @export
normalise_iri <- function(iri, prefixes = character(), base_iri = NULL, prefer_curie = FALSE) {
  expanded <- expand_iri(iri, prefixes, base_iri)
  if (prefer_curie) {
    contract_iri(expanded, prefixes, base_iri)
  } else {
    expanded
  }
}
