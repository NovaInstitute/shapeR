#' General utilities for shapeR.
#'
#' These helpers provide small conveniences shared across constructors,
#' visualisation, and validation code.

#' Null-coalescing operator
#'
#' @param x,y Values to combine.
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Assert a scalar character input
#'
#' @param x Object to check.
#' @param arg Name of the argument for error messaging.
#' @keywords internal
assert_scalar_character <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a non-NA scalar character string.", arg),
         call. = FALSE)
  }
  invisible(x)
}
