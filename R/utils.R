#' General utilities for shapeR.
#'
#' These helpers support argument validation and small internal conveniences
#' used throughout the package.

#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

arg_label <- function(x, arg_name = NULL) {
  if (!is.null(arg_name)) return(arg_name)
  deparse(substitute(x))
}

check_list <- function(x, arg_name = NULL, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(NULL)

  if (!is.list(x)) {
    stop(sprintf("`%s` must be a list.", arg_label(x, arg_name)), call. = FALSE)
  }

  x
}

check_scalar_character <- function(x, arg_name = NULL, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(NULL)

  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(
      sprintf("`%s` must be %s non-NA scalar character string.",
              arg_label(x, arg_name),
              if (allow_null) "NULL or a" else "a"),
      call. = FALSE
    )
  }

  x
}

check_character_vector <- function(x, arg_name = NULL, allow_null = FALSE) {
  if (is.null(x) && allow_null) return(NULL)

  if (!is.character(x) || anyNA(x)) {
    stop(
      sprintf("`%s` must be %scharacter vector without NA.",
              arg_label(x, arg_name),
              if (allow_null) "NULL or a " else "a "),
      call. = FALSE
    )
  }

  x
}

check_logical_scalar <- function(x, arg_name = NULL) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a non-NA logical scalar.", arg_label(x, arg_name)),
         call. = FALSE)
  }

  x
}

check_list_of_class <- function(x, class, arg_name = NULL) {
  if (length(x) > 0L) {
    ok <- vapply(x, inherits, logical(1), class)
    if (!all(ok)) {
      stop(
        sprintf("All elements of `%s` must be of class '%s'.",
                arg_label(x, arg_name), class),
        call. = FALSE
      )
    }
  }

  x
}
