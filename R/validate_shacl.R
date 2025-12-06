#' Validate data against SHACL shapes.
#'
#' This is a lightweight validator that supports a subset of SHACL constraint
#' components and is designed to work with the in-memory shape model produced by
#' [read_shacl()]. The validator accepts data either as an `rdflib` graph or a
#' data frame of triples and dispatches validation to functions based on each
#' constraint's `component` field.
#'
#' @param data Data graph to validate. Either an [`rdflib::rdf`] object or a
#'   data frame with columns `subject`, `predicate`, `object`, and optional
#'   `datatype`.
#' @param shapes SHACL shapes graph as returned by [sh_shape_graph()].
#'
#' @return A list with two elements: `conforms`, a logical flag, and `results`,
#'   a data frame describing individual validation outcomes (one row per
#'   violation or warning). Supported result columns are `focusNode`, `shape`,
#'   `path`, `component`, `message`, `severity`, `value`, and `scope`.
#'
#' @importFrom rdflib rdf_query
#' @export
validate_shacl <- function(data, shapes) {
  if (!inherits(shapes, "sh_shape_graph")) {
    stop("`shapes` must be a sh_shape_graph object.", call. = FALSE)
  }

  triples <- as_triples_df(data)

  results <- list()

  for (shape in shapes$shapes) {
    if (isTRUE(shape$deactivated)) next

    focus_nodes <- resolve_focus_nodes(shape, triples)
    if (!length(focus_nodes)) next

    shape_severity <- shape$severity %||% "sh:Violation"

    # validate node-level constraints
    for (constraint in shape$constraints) {
      res <- validate_node_constraint(constraint, focus_nodes, triples,
                                      shape_id = shape$id,
                                      severity = shape_severity)
      results <- c(results, res)
    }

    # validate property shapes belonging to the node shape
    for (prop in shape$properties) {
      if (isTRUE(prop$deactivated)) next

      prop_severity <- prop$severity %||% shape$severity %||% "sh:Violation"

      for (node in focus_nodes) {
        idx <- triples$subject == node & triples$predicate == prop$path
        values <- triples$object[idx]
        dtypes <- triples$datatype[idx]

        for (constraint in prop$constraints) {
          res <- validate_property_constraint(constraint, values, dtypes,
                                              focus_node = node,
                                              path = prop$path,
                                              shape_id = shape$id,
                                              severity = prop_severity,
                                              triples = triples)
          results <- c(results, res)
        }
      }
    }
  }

  results_df <- as.data.frame(do.call(rbind, results), stringsAsFactors = FALSE)
  if (!nrow(results_df)) {
    results_df <- data.frame(
      focusNode = character(),
      shape     = character(),
      path      = character(),
      component = character(),
      message   = character(),
      severity  = character(),
      value     = character(),
      scope     = character(),
      stringsAsFactors = FALSE
    )
  }

  conforms <- !any(results_df$severity == "sh:Violation")

  list(conforms = conforms, results = results_df)
}

# ---------- helpers ---------------------------------------------------------

as_triples_df <- function(data) {
  if (inherits(data, "rdf")) {
    df <- rdf_query(
      data,
      "SELECT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object }"
    )
    df <- normalize_object_column(df)
    return(df)
  }

  if (is.data.frame(data)) {
    required <- c("subject", "predicate", "object")
    if (!all(required %in% names(data))) {
      stop("Data frames must contain subject, predicate, and object columns.",
           call. = FALSE)
    }

    if (!"datatype" %in% names(data)) {
      data$datatype <- NA_character_
    }
    data$subject   <- as.character(data$subject)
    data$predicate <- as.character(data$predicate)
    data$object    <- as.character(data$object)
    data$datatype  <- as.character(data$datatype)

    return(data[, c("subject", "predicate", "object", "datatype")])
  }

  stop("`data` must be either an rdflib::rdf object or a data.frame.",
       call. = FALSE)
}

normalize_object_column <- function(df) {
  if (!"datatype" %in% names(df)) {
    df$datatype <- NA_character_
  }

  pat <- '^"(.*)"\^\^<([^>]+)>'
  matches <- regexec(pat, df$object)
  captures <- regmatches(df$object, matches)

  for (i in seq_along(captures)) {
    cap <- captures[[i]]
    if (length(cap) == 3) {
      df$object[i]   <- cap[2]
      df$datatype[i] <- cap[3]
    }
  }

  df
}

resolve_focus_nodes <- function(shape, triples) {
  rdf_type <- c("rdf:type", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")

  nodes <- character()

  if (length(shape$targets$targetClass)) {
    idx <- triples$predicate %in% rdf_type & triples$object %in% shape$targets$targetClass
    nodes <- c(nodes, triples$subject[idx])
  }

  if (length(shape$targets$targetNode)) {
    nodes <- c(nodes, shape$targets$targetNode)
  }

  if (length(shape$targets$targetSubjectsOf)) {
    idx <- triples$predicate %in% shape$targets$targetSubjectsOf
    nodes <- c(nodes, triples$subject[idx])
  }

  if (length(shape$targets$targetObjectsOf)) {
    idx <- triples$predicate %in% shape$targets$targetObjectsOf
    nodes <- c(nodes, triples$object[idx])
  }

  unique(nodes)
}

validate_node_constraint <- function(constraint, focus_nodes, triples,
                                      shape_id, severity) {
  validators <- list(
    "sh:ClassConstraintComponent" = validate_node_class,
    "sh:InConstraintComponent"    = validate_node_in
  )

  fun <- validators[[constraint$component]]
  if (is.null(fun)) return(list())

  fun(constraint, focus_nodes, triples, shape_id, severity)
}

validate_property_constraint <- function(constraint, values, datatypes,
                                          focus_node, path, shape_id,
                                          severity, triples) {
  validators <- list(
    "sh:MinCountConstraintComponent" = validate_min_count,
    "sh:MaxCountConstraintComponent" = validate_max_count,
    "sh:DatatypeConstraintComponent" = validate_datatype,
    "sh:ClassConstraintComponent"    = validate_property_class,
    "sh:InConstraintComponent"       = validate_in
  )

  fun <- validators[[constraint$component]]
  if (is.null(fun)) return(list())

  fun(constraint, values, datatypes, focus_node, path, shape_id, severity,
      triples)
}

validate_min_count <- function(constraint, values, datatypes, focus_node, path,
                               shape_id, severity, triples) {
  min_count <- constraint$params$minCount %||% 0L
  n <- length(values)

  if (n < min_count) {
    msg <- sprintf("Expected at least %s value(s) for path %s, found %s.",
                   min_count, path, n)
    return(list(new_result(focus_node, shape_id, path, constraint$component,
                           msg, severity, NA_character_, constraint$scope)))
  }

  list()
}

validate_max_count <- function(constraint, values, datatypes, focus_node, path,
                               shape_id, severity, triples) {
  max_count <- constraint$params$maxCount %||% Inf
  n <- length(values)

  if (n > max_count) {
    msg <- sprintf("Expected at most %s value(s) for path %s, found %s.",
                   max_count, path, n)
    return(list(new_result(focus_node, shape_id, path, constraint$component,
                           msg, severity, NA_character_, constraint$scope)))
  }

  list()
}

validate_datatype <- function(constraint, values, datatypes, focus_node, path,
                              shape_id, severity, triples) {
  expected <- constraint$params$datatype
  if (!length(values)) return(list())

  invalid <- is.na(datatypes) | datatypes != expected

  if (any(invalid)) {
    bad_vals <- values[invalid]
    msg <- sprintf("Values %s for path %s do not match datatype %s.",
                   paste(bad_vals, collapse = ", "), path, expected)
    return(list(new_result(focus_node, shape_id, path, constraint$component,
                           msg, severity, paste(bad_vals, collapse = ", "),
                           constraint$scope)))
  }

  list()
}

validate_property_class <- function(constraint, values, datatypes, focus_node,
                                    path, shape_id, severity, triples) {
  rdf_type <- c("rdf:type", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")
  expected <- constraint$params$class
  results <- list()

  for (i in seq_along(values)) {
    val <- values[[i]]
    idx <- triples$subject == val & triples$predicate %in% rdf_type
    has_class <- any(triples$object[idx] %in% expected)
    if (!isTRUE(has_class)) {
      msg <- sprintf("Value %s for path %s does not have required class %s.",
                     val, path, paste(expected, collapse = ", "))
      results[[length(results) + 1L]] <- new_result(
        focus_node, shape_id, path, constraint$component, msg, severity,
        val, constraint$scope
      )
    }
  }

  results
}

validate_in <- function(constraint, values, datatypes, focus_node, path,
                        shape_id, severity, triples) {
  allowed <- constraint$params$in %||% character()
  if (!length(values)) return(list())

  invalid <- !values %in% allowed

  if (any(invalid)) {
    bad_vals <- values[invalid]
    msg <- sprintf("Values %s for path %s are not in allowed set.",
                   paste(bad_vals, collapse = ", "), path)
    return(list(new_result(focus_node, shape_id, path, constraint$component,
                           msg, severity, paste(bad_vals, collapse = ", "),
                           constraint$scope)))
  }

  list()
}

validate_node_class <- function(constraint, focus_nodes, triples, shape_id,
                                severity) {
  rdf_type <- c("rdf:type", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>")
  expected <- constraint$params$class
  results <- list()

  for (node in focus_nodes) {
    idx <- triples$subject == node & triples$predicate %in% rdf_type
    has_class <- any(triples$object[idx] %in% expected)
    if (!isTRUE(has_class)) {
      msg <- sprintf("Node %s is missing required class %s.",
                     node, paste(expected, collapse = ", "))
      results[[length(results) + 1L]] <- new_result(
        node, shape_id, NA_character_, constraint$component, msg, severity,
        node, constraint$scope
      )
    }
  }

  results
}

validate_node_in <- function(constraint, focus_nodes, triples, shape_id,
                             severity) {
  allowed <- constraint$params$in %||% character()
  results <- list()

  for (node in focus_nodes) {
    if (!node %in% allowed) {
      msg <- sprintf("Node %s is not in allowed set.", node)
      results[[length(results) + 1L]] <- new_result(
        node, shape_id, NA_character_, constraint$component, msg, severity,
        node, constraint$scope
      )
    }
  }

  results
}

new_result <- function(focus_node, shape_id, path, component, message, severity,
                       value, scope) {
  data.frame(
    focusNode = focus_node,
    shape     = shape_id %||% NA_character_,
    path      = path %||% NA_character_,
    component = component,
    message   = message,
    severity  = severity,
    value     = value %||% NA_character_,
    scope     = scope,
    stringsAsFactors = FALSE
  )
}
