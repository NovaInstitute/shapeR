#' Return the first element or NA when empty.
#'
#' @param x A vector or list.
#' @return The first element of `x`, or `NA` if `x` is empty.
#' @export
first_or_na <- function(x) {
  if (length(x) == 0L) {
    return(NA)
  }

  x[[1]]
}
