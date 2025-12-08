#' Shorten IRIs using known prefixes.
#'
#' @param iri Character vector of full IRIs.
#' @param prefixes Named character vector mapping prefix to base IRI. Optional;
#'   defaults to an empty vector meaning no shortening will be applied.
#' @return Character vector of compact IRIs (e.g., `ex:Resource`).
#'
#' @examples
#' prefixes <- c(ex = "http://example.com/")
#' shorten_iri("http://example.com/Resource", prefixes)
#' shorten_iri(c("http://example.com/id/1", "http://other.com/id/1"), prefixes)
#' @export
shorten_iri <- function(iri, prefixes = character()) {
  if (length(prefixes) == 0L) {
    return(iri)
  }

  prefixes_clean <- strip_angle_brackets(unname(prefixes))

  unname(vapply(
    iri,
    function(x) {
      cleaned <- strip_angle_brackets(x)
      match_idx <- which(startsWith(cleaned, prefixes_clean))
      if (length(match_idx) == 0L) {
        return(x)
      }

      idx <- match_idx[1L]
      paste0(names(prefixes)[idx], ":", substring(cleaned, nchar(prefixes_clean[idx]) + 1L))
    },
    character(1)
  ))
}
