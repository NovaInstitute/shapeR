#' Shorten IRIs using known prefixes.
#'
#' @param iri Character vector of full IRIs.
#' @param prefixes Named character vector mapping prefix to base IRI. Optional;
#'   defaults to an empty vector meaning no shortening will be applied.
#' @return Character vector of compact IRIs (e.g., `ex:Resource`).
#' @export
shorten_iri <- function(iri, prefixes = character()) {
  if (length(prefixes) == 0L) {
    return(iri)
  }

  vapply(
    iri,
    function(x) {
      match_idx <- which(startsWith(x, unname(prefixes)))
      if (length(match_idx) == 0L) {
        return(x)
      }

      idx <- match_idx[1L]
      paste0(names(prefixes)[idx], ":", substring(x, nchar(prefixes[idx]) + 1L))
    },
    character(1)
  )
}
