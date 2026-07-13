#' Materialise SKOS broader/narrower hierarchy as rdf:type triples.
#'
#' SHACL `sh:class` checks `rdf:type` directly without hierarchy traversal. This
#' function prepares an in-memory triples data frame for SHACL validation by
#' adding `rdf:type` assertions inferred from the SKOS concept hierarchy: for
#' each instance that has `rdf:type <concept>` and `<concept> skos:broader*
#' <ancestor>`, an `rdf:type <ancestor>` triple is added.
#'
#' SKOS predicates are matched by IRI substring so both angle-bracket and bare
#' IRI forms are handled. The returned data frame retains the same column
#' structure as `triples`.
#'
#' @param triples Data frame of RDF triples with at least columns `subject`,
#'   `predicate`, `object`. Typically the project description fetched from
#'   Fluree or a test fixture.
#' @param concept_triples Optional data frame of SKOS concept scheme triples
#'   (e.g. loaded from `cdm-concepts.ttl`). When supplied, `skos:broader`
#'   relationships are sourced from the union of `triples` and
#'   `concept_triples`; only `triples` is returned augmented.
#'
#' @return `triples` with additional `rdf:type` rows inferred from the SKOS
#'   hierarchy. Duplicates are removed.
#'
#' @examples
#' \dontrun{
#' project <- data.frame(
#'   subject   = c("<http://ex.org/proj>", "<http://ex.org/proj>",
#'                 "<http://ex.org/tech>"),
#'   predicate = c("<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>",
#'                 "<http://w3id.org/aiao#isPerformedWith>",
#'                 "<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>"),
#'   object    = c("<http://independentimpact.org/cdm/CDMProjectActivity>",
#'                 "<http://ex.org/tech>",
#'                 "<http://independentimpact.org/cdm/SolarPV>"),
#'   datatype  = NA_character_,
#'   stringsAsFactors = FALSE
#' )
#' cdm_scheme <- read_ttl_as_triples("cdm-concepts.ttl")   # helper not shown
#' augmented  <- materialise_skos_hierarchy(project, cdm_scheme)
#' # <http://ex.org/tech> rdf:type cdm:RenewableEnergyTechnology is now present.
#' }
#'
#' @export
materialise_skos_hierarchy <- function(triples, concept_triples = NULL) {
  search_triples <- if (!is.null(concept_triples)) {
    rbind(triples[, c("subject", "predicate", "object")],
          concept_triples[, c("subject", "predicate", "object")])
  } else {
    triples[, c("subject", "predicate", "object")]
  }

  is_broader_pred  <- function(p) grepl("skos/core#broader",  p, fixed = TRUE)
  is_narrower_pred <- function(p) grepl("skos/core#narrower", p, fixed = TRUE)
  is_type_pred     <- function(p) grepl("rdf-syntax-ns#type", p, fixed = TRUE) | p == "rdf:type"

  # Step 1: build concept hierarchy (direct broader/narrower pairs)
  broader_rows  <- search_triples[is_broader_pred(search_triples$predicate),
                                  c("subject", "object"), drop = FALSE]
  narrower_rows <- search_triples[is_narrower_pred(search_triples$predicate),
                                  c("object", "subject"), drop = FALSE]
  names(narrower_rows) <- c("subject", "object")

  concept_pairs <- unique(rbind(broader_rows, narrower_rows))
  if (!nrow(concept_pairs)) return(triples)

  ancestor_map <- .skos_transitive_closure(concept_pairs)

  # Step 2: collect existing rdf:type assertions in project triples
  type_rows <- triples[is_type_pred(triples$predicate), c("subject", "object"), drop = FALSE]
  names(type_rows) <- c("instance", "concept")
  if (!nrow(type_rows)) return(triples)

  # Step 3: join to get (instance, ancestor) pairs
  inherited <- merge(type_rows, ancestor_map, by.x = "concept", by.y = "subject")
  if (!nrow(inherited)) return(triples)

  rdf_type_iri <- "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
  new_rows <- data.frame(
    subject   = inherited$instance,
    predicate = rdf_type_iri,
    object    = inherited$object,
    stringsAsFactors = FALSE
  )
  for (col in setdiff(names(triples), c("subject", "predicate", "object"))) {
    new_rows[[col]] <- NA_character_
  }
  new_rows <- new_rows[, names(triples), drop = FALSE]

  combined <- rbind(triples, new_rows)
  combined[!duplicated(combined[c("subject", "predicate", "object")]), ]
}

.skos_transitive_closure <- function(pairs) {
  repeat {
    extended <- merge(pairs, pairs, by.x = "object", by.y = "subject")
    # merge result: object (key), subject (from pairs lhs), object.y (from pairs rhs)
    extended <- data.frame(
      subject = extended$subject,
      object  = extended$object.y,
      stringsAsFactors = FALSE
    )
    combined <- unique(rbind(pairs, extended))
    if (nrow(combined) == nrow(pairs)) break
    pairs <- combined
  }
  pairs
}
