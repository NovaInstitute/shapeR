#' Read a SHACL shapes graph from RDF
#'
#' Parses a SHACL shapes graph serialisation (e.g. Turtle, RDF/XML, JSON-LD)
#' into an in-memory R object model consisting of \code{sh_node_shape},
#' \code{sh_property_shape}, and \code{sh_constraint} objects, wrapped in a
#' \code{sh_shape_graph}.
#'
#' @param file Path or URL to a SHACL shapes file readable by \code{rdflib}.
#' @param base_iri Optional base IRI to store in the resulting shape graph
#'   (does not affect parsing, which is delegated to \code{rdflib}).
#' @param prefixes Optional named character vector of prefixes to attach to the
#'   resulting shape graph and to use when normalising identifiers.
#' @param normalise_iris Logical; when TRUE, IRIs (Internationalized Resource
#'   Identifiers) and paths are expanded using \code{prefixes} and
#'   \code{base_iri} before being stored in objects. The name refers to IRIs and
#'   is unrelated to the \code{iris} example dataset.
#'
#' @return An object of class \code{"sh_shape_graph"}.
#'
#' @examples
#' \dontrun{
#' sg <- read_shacl("Shapes/independent-impact-shapes.ttl")
#' }
#'
#' @importFrom rdflib rdf_parse rdf_query
#' @importFrom dplyr select filter mutate group_by summarise transmute
#' @importFrom dplyr distinct left_join
#' @importFrom utils download.file
#' @export
read_shacl <- function(file, base_iri = NULL, prefixes = NULL, normalise_iris = FALSE) {

  prefixes <- prefixes %||% character()

  downloaded_file <- NULL

  if (is.character(file) && length(file) == 1L && grepl("^https?://", file)) {
    ext <- tools::file_ext(file)
    downloaded_file <- tempfile(fileext = if (nzchar(ext)) paste0(".", ext) else "")
    utils::download.file(file, downloaded_file, quiet = TRUE)
    file <- downloaded_file
  }

  if (!is.null(downloaded_file)) {
    on.exit(unlink(downloaded_file), add = TRUE)
  }

  # ------------------ constants for IRIs -----------------------------------
  sh_ns   <- "http://www.w3.org/ns/shacl#"
  rdf_ns  <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"

  iri <- function(ns, local) paste0("<", ns, local, ">")

  SH_PROPERTY          <- iri(sh_ns,  "property")
  SH_PATH              <- iri(sh_ns,  "path")
  SH_DATATYPE          <- iri(sh_ns,  "datatype")
  SH_MINCOUNT          <- iri(sh_ns,  "minCount")
  SH_MAXCOUNT          <- iri(sh_ns,  "maxCount")
  SH_NODE              <- iri(sh_ns,  "node")
  SH_CLASS             <- iri(sh_ns,  "class")
  SH_IN                <- iri(sh_ns,  "in")
  SH_OR                <- iri(sh_ns,  "or")
  SH_AND               <- iri(sh_ns,  "and")
  SH_XONE              <- iri(sh_ns,  "xone")
  SH_TARGETCLASS       <- iri(sh_ns,  "targetClass")
  SH_TARGETNODE        <- iri(sh_ns,  "targetNode")
  SH_TARGETSUBJECTSOF  <- iri(sh_ns,  "targetSubjectsOf")
  SH_TARGETOBJECTSOF   <- iri(sh_ns,  "targetObjectsOf")
  SH_DEACTIVATED       <- iri(sh_ns,  "deactivated")
  SH_SEVERITY          <- iri(sh_ns,  "severity")
  SH_MESSAGE           <- iri(sh_ns,  "message")
  SH_NODESHAPE         <- iri(sh_ns,  "NodeShape")
  SH_PROPERTYSHAPE     <- iri(sh_ns,  "PropertyShape")

  RDF_TYPE             <- iri(rdf_ns, "type")

  # ------------------ parse RDF & get triples ------------------------------
  rdf <- rdflib::rdf_parse(file)

  triples <- rdflib::rdf_query(
    rdf,
    "SELECT ?subject ?predicate ?object WHERE { ?subject ?predicate ?object }"
  ) |>
    dplyr::select(subject, predicate, object) |>
    dplyr::mutate(
      subject   = add_angle_brackets(subject),
      predicate = add_angle_brackets(predicate),
      object    = add_angle_brackets(object)
    )

  # ------------------ identify property shapes -----------------------------
  prop_links <- triples |>
    dplyr::filter(predicate == SH_PROPERTY)

  prop_ids <- unique(prop_links$object)

  # ------------------ build property shape objects -------------------------

  property_shapes <- list()

  for (pid in prop_ids) {
    st <- triples[triples$subject == pid, , drop = FALSE]

    # path (required for useful property shapes)
    path_vals <- st$object[st$predicate == SH_PATH]
    if (!length(path_vals)) {
      # You could warn here instead of stopping if you want to be permissive
      path <- NA_character_
    } else {
      path <- first_or_na(path_vals)
    }

    # severity, deactivated, annotations
    severity_vals <- st$object[st$predicate == SH_SEVERITY]
    severity <- if (length(severity_vals)) first_or_na(severity_vals) else NULL

    deact_vals <- st$object[st$predicate == SH_DEACTIVATED]
    deactivated <- FALSE
    if (length(deact_vals)) {
      # very simple literal parsing, can be improved
      deactivated <- grepl("true", tolower(first_or_na(deact_vals)), fixed = TRUE)
    }

    msg_vals <- st$object[st$predicate == SH_MESSAGE]
    annotations <- list()
    if (length(msg_vals)) {
      annotations$message <- msg_vals
    }

    # nested shape references (sh:node, sh:or, sh:and, sh:xone)
    node_vals <- st$object[st$predicate == SH_NODE]
    node_target <- if (length(node_vals)) first_or_na(node_vals) else NULL

    or_heads   <- st$object[st$predicate == SH_OR]
    and_heads  <- st$object[st$predicate == SH_AND]
    xone_heads <- st$object[st$predicate == SH_XONE]

    nested_or   <- character()
    nested_and  <- character()
    nested_xone <- character()

    if (length(or_heads)) {
      nested_or <- unique(unlist(lapply(or_heads, expand_rdf_list, triples = triples)))
    }
    if (length(and_heads)) {
      nested_and <- unique(unlist(lapply(and_heads, expand_rdf_list, triples = triples)))
    }
    if (length(xone_heads)) {
      nested_xone <- unique(unlist(lapply(xone_heads, expand_rdf_list, triples = triples)))
    }

    nested <- list(
      node = node_target,
      or   = nested_or,
      and  = nested_and,
      xone = nested_xone
    )

    # constraints on the property shape
    build_constraint <- function(pred, component, param_name, list_is_rdf_list = FALSE) {
      objs <- st$object[st$predicate == pred]
      if (!length(objs)) return(NULL)

      if (list_is_rdf_list) {
        vals <- unique(unlist(lapply(objs, expand_rdf_list, triples = triples)))
      } else {
        vals <- objs
      }

      # simple integer coercion for min/maxCount
      if (param_name %in% c("minCount", "maxCount")) {
        suppressWarnings({
          ints <- as.integer(vals)
        })
        if (!all(is.na(ints))) {
          vals <- ints
        }
      }

      param_value <- if (length(vals) == 1L) vals[[1L]] else vals
      params <- list()
      params[[param_name]] <- param_value

      sh_constraint(
        component = component,
        params    = params,
        scope     = "property"
      )
    }

    constraints <- list()

    # minCount / maxCount / datatype / class
    c_min <- build_constraint(SH_MINCOUNT,
                              "sh:MinCountConstraintComponent", "minCount")
    c_max <- build_constraint(SH_MAXCOUNT,
                              "sh:MaxCountConstraintComponent", "maxCount")
    c_dt  <- build_constraint(SH_DATATYPE,
                              "sh:DatatypeConstraintComponent", "datatype")
    c_cl  <- build_constraint(SH_CLASS,
                              "sh:ClassConstraintComponent", "class")

    # in / or / and / xone as RDF lists
    c_in  <- build_constraint(SH_IN,
                              "sh:InConstraintComponent", "in",
                              list_is_rdf_list = TRUE)
    c_or  <- build_constraint(SH_OR,
                              "sh:OrConstraintComponent", "or",
                              list_is_rdf_list = TRUE)
    c_and <- build_constraint(SH_AND,
                              "sh:AndConstraintComponent", "and",
                              list_is_rdf_list = TRUE)
    c_xo  <- build_constraint(SH_XONE,
                              "sh:XoneConstraintComponent", "xone",
                              list_is_rdf_list = TRUE)

    # node constraint
    c_node <- build_constraint(SH_NODE,
                               "sh:NodeConstraintComponent", "node",
                               list_is_rdf_list = FALSE)

    for (c in list(c_min, c_max, c_dt, c_cl, c_in, c_or, c_and, c_xo, c_node)) {
      if (!is.null(c)) constraints[[length(constraints) + 1L]] <- c
    }

    # normalise_iris is forwarded so sh_property_shape can expand CURIEs/
    # relative IRIs using the supplied prefixes/base IRI when requested
    ps_obj <- sh_property_shape(
      id          = pid,
      path        = path,
      constraints = constraints,
      nested      = nested,
      annotations = annotations,
      severity    = severity,
      deactivated = deactivated,
      extras      = list(),
      prefixes    = prefixes,
      base_iri    = base_iri,
      normalise   = normalise_iris
    )

    property_shapes[[pid]] <- ps_obj
  }

  # ------------------ identify node shapes ---------------------------------

  # explicit NodeShape types
  node_type_ids <- triples$subject[
    triples$predicate == RDF_TYPE & triples$object == SH_NODESHAPE
  ]

  # any subject that has sh:property
  node_prop_ids <- unique(prop_links$subject)

  # any shape that is the target of sh:node
  node_target_ids <- unique(triples$object[triples$predicate == SH_NODE])

  node_ids <- unique(c(node_type_ids, node_prop_ids, node_target_ids))

  # ------------------ build node shape objects -----------------------------

  node_shapes <- list()

  for (nid in node_ids) {
    st <- triples[triples$subject == nid, , drop = FALSE]

    # targets
    t_class <- st$object[st$predicate == SH_TARGETCLASS]
    t_node  <- st$object[st$predicate == SH_TARGETNODE]
    t_subj  <- st$object[st$predicate == SH_TARGETSUBJECTSOF]
    t_obj   <- st$object[st$predicate == SH_TARGETOBJECTSOF]

    targets <- list(
      targetClass      = if (length(t_class)) unique(t_class) else character(),
      targetNode       = if (length(t_node))  unique(t_node)  else character(),
      targetSubjectsOf = if (length(t_subj))  unique(t_subj)  else character(),
      targetObjectsOf  = if (length(t_obj))   unique(t_obj)   else character()
    )

    # severity, deactivated, annotations
    severity_vals <- st$object[st$predicate == SH_SEVERITY]
    severity <- if (length(severity_vals)) first_or_na(severity_vals) else NULL

    deact_vals <- st$object[st$predicate == SH_DEACTIVATED]
    deactivated <- FALSE
    if (length(deact_vals)) {
      deactivated <- grepl("true", tolower(first_or_na(deact_vals)), fixed = TRUE)
    }

    msg_vals <- st$object[st$predicate == SH_MESSAGE]
    annotations <- list()
    if (length(msg_vals)) {
      annotations$message <- msg_vals
    }

    # node-level constraints (class, in, or, and, xone, etc.) --------------
    build_constraint_node <- function(pred, component, param_name, list_is_rdf_list = FALSE) {
      objs <- st$object[st$predicate == pred]
      if (!length(objs)) return(NULL)

      if (list_is_rdf_list) {
        vals <- unique(unlist(lapply(objs, expand_rdf_list, triples = triples)))
      } else {
        vals <- objs
      }

      param_value <- if (length(vals) == 1L) vals[[1L]] else vals
      params <- list()
      params[[param_name]] <- param_value

      sh_constraint(
        component = component,
        params    = params,
        scope     = "node"
      )
    }

    n_constraints <- list()

    c_cl  <- build_constraint_node(SH_CLASS,
                                   "sh:ClassConstraintComponent", "class")
    c_in  <- build_constraint_node(SH_IN,
                                   "sh:InConstraintComponent", "in",
                                   list_is_rdf_list = TRUE)
    c_or  <- build_constraint_node(SH_OR,
                                   "sh:OrConstraintComponent", "or",
                                   list_is_rdf_list = TRUE)
    c_and <- build_constraint_node(SH_AND,
                                   "sh:AndConstraintComponent", "and",
                                   list_is_rdf_list = TRUE)
    c_xo  <- build_constraint_node(SH_XONE,
                                   "sh:XoneConstraintComponent", "xone",
                                   list_is_rdf_list = TRUE)

    for (c in list(c_cl, c_in, c_or, c_and, c_xo)) {
      if (!is.null(c)) n_constraints[[length(n_constraints) + 1L]] <- c
    }

    # properties belonging to this node shape ------------------------------
    prop_for_node_ids <- prop_links$object[prop_links$subject == nid]
    props_for_node <- list()
    if (length(prop_for_node_ids)) {
      # reuse property_shapes we already built
      for (pid in unique(prop_for_node_ids)) {
        if (pid %in% names(property_shapes)) {
          props_for_node[[length(props_for_node) + 1L]] <- property_shapes[[pid]]
        }
      }
    }

    # Forward normalise_iris so identifiers and targets are expanded/contracted
    # consistently within the node shape model
    node_obj <- sh_node_shape(
      id          = nid,
      targets     = targets,
      properties  = props_for_node,
      constraints = n_constraints,
      annotations = annotations,
      severity    = severity,
      deactivated = deactivated,
      extras      = list(),
      prefixes    = prefixes,
      base_iri    = base_iri,
      normalise   = normalise_iris
    )

    node_shapes[[nid]] <- node_obj
  }

  # ------------------ assemble shape graph ---------------------------------

  # At this stage we only put node shapes in the top-level graph. Property
  # shapes are reachable via node_shapes[[i]]$properties.
  sg <- sh_shape_graph(
    shapes   = node_shapes,
    prefixes = prefixes,
    base_iri = base_iri,
    metadata = list()
  )

  sg
}
