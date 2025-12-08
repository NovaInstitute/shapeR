#' Helpers for working with RDF lists.
#'
#' Functions to traverse RDF collections represented with rdf:first / rdf:rest
#' triples and return the contained values in order.
#'
#' @keywords internal
rdf_list_constants <- function() {
  rdf_ns <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  list(
    first = paste0("<", rdf_ns, "first>"),
    rest  = paste0("<", rdf_ns, "rest>"),
    nil   = paste0("<", rdf_ns, "nil>")
  )
}

#' Determine whether a node is rdf:nil
#'
#' @param node Node identifier string.
#' @keywords internal
is_rdf_nil <- function(node) {
  const <- rdf_list_constants()
  identical(node, const$nil)
}

#' Get the rdf:first value for a given list node
#'
#' @param node List node identifier.
#' @param triples Data frame of RDF triples with columns subject, predicate, object.
#' @keywords internal
rdf_list_first_value <- function(node, triples) {
  const <- rdf_list_constants()
  idx <- triples$subject == node & triples$predicate == const$first
  if (!any(idx)) {
    return(NULL)
  }
  first_vals <- triples$object[idx]
  first_vals[[1L]]
}

#' Get the rdf:rest value for a given list node
#'
#' @param node List node identifier.
#' @param triples Data frame of RDF triples with columns subject, predicate, object.
#' @keywords internal
rdf_list_rest_value <- function(node, triples) {
  const <- rdf_list_constants()
  idx <- triples$subject == node & triples$predicate == const$rest
  if (!any(idx)) {
    return(NULL)
  }
  rest_vals <- triples$object[idx]
  rest_vals[[1L]]
}

#' Expand an RDF list into an ordered vector
#'
#' @param head Identifier of the head node for the RDF list.
#' @param triples Data frame of RDF triples with columns subject, predicate, object.
#'
#' @return Character vector of list members in order. If the head does not look
#'   like an RDF list node, the head value itself is returned.
#'
#' @examples
#' triples <- data.frame(
#'   subject = c("_:b0", "_:b0", "_:b1", "_:b1"),
#'   predicate = c(
#'     "<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>",
#'     "<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>",
#'     "<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>",
#'     "<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>"
#'   ),
#'   object = c("ex:first", "_:b1", "ex:second", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>"),
#'   stringsAsFactors = FALSE
#' )
#'
#' expand_rdf_list("_:b0", triples)
#' @export
expand_rdf_list <- function(head, triples) {
  if (is.null(head) || length(head) == 0L || is.na(head)) {
    return(character())
  }

  const <- rdf_list_constants()
  current <- head[[1L]]

  # handle rdf:nil directly
  if (is_rdf_nil(current)) {
    return(character())
  }

  elements <- character()
  visited <- character()

  while (!is.null(current) && length(current) && !is_rdf_nil(current)) {
    if (current %in% visited) {
      warning("Detected cycle in RDF list starting at ", head[[1L]])
      break
    }
    visited <- c(visited, current)

    first_val <- rdf_list_first_value(current, triples)
    if (is.null(first_val)) {
      # not an RDF list structure; treat the head as an atomic value
      if (identical(current, head[[1L]])) {
        return(current)
      }
      break
    }
    elements <- c(elements, first_val)

    rest_val <- rdf_list_rest_value(current, triples)
    if (is.null(rest_val)) {
      break
    }

    current <- rest_val
  }

  elements
}
