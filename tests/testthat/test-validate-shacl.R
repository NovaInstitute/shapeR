test_that("validate_shacl handles property constraints on data frames", {
  age_min <- sh_constraint(
    component = "sh:MinCountConstraintComponent",
    params    = list(minCount = 1L),
    scope     = "property"
  )

  age_dt <- sh_constraint(
    component = "sh:DatatypeConstraintComponent",
    params    = list(datatype = "xsd:integer"),
    scope     = "property"
  )

  age_prop <- sh_property_shape(
    path        = "ex:age",
    constraints = list(age_min, age_dt)
  )

  person_shape <- sh_node_shape(
    id         = "ex:PersonShape",
    targets    = list(targetClass = "ex:Person"),
    properties = list(age_prop)
  )

  sg <- sh_shape_graph(list(person_shape))

  data <- data.frame(
    subject   = c("ex:alice",   "ex:alice",  "ex:alice", "ex:bob",    "ex:charlie"),
    predicate = c("rdf:type",   "ex:age",    "rdf:type", "ex:age",    "rdf:type"),
    object    = c("ex:Person", "23",         "ex:Person", "thirty",    "ex:Person"),
    datatype  = c(NA,           "xsd:integer", NA,          "xsd:string", NA),
    stringsAsFactors = FALSE
  )

  report <- validate_shacl(data, sg)

  expect_false(report$conforms)
  expect_equal(nrow(report$results), 2)
  expect_true(all(report$results$component %in% c(
    "sh:MinCountConstraintComponent", "sh:DatatypeConstraintComponent"
  )))
  expect_true("focusNode" %in% names(report$results))
  expect_true("severity" %in% names(report$results))
})


test_that("severity defaults and warnings affect conformance", {
  age_min <- sh_constraint(
    component = "sh:MinCountConstraintComponent",
    params    = list(minCount = 1L),
    scope     = "property"
  )

  age_prop <- sh_property_shape(
    path        = "ex:age",
    constraints = list(age_min),
    severity    = "sh:Warning"
  )

  person_shape <- sh_node_shape(
    id         = "ex:PersonShape",
    targets    = list(targetClass = "ex:Person"),
    properties = list(age_prop)
  )

  sg <- sh_shape_graph(list(person_shape))

  data <- data.frame(
    subject   = "ex:alice",
    predicate = "rdf:type",
    object    = "ex:Person",
    stringsAsFactors = FALSE
  )

  report <- validate_shacl(data, sg)

  expect_true(report$conforms)
  expect_equal(report$results$severity, "sh:Warning")
})


test_that("validate_shacl can read rdflib graphs", {
  ttl <- """
  @prefix ex: <http://example.com/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

  ex:alice rdf:type ex:Person ;
           ex:age "23"^^xsd:integer .
  """

  rdf <- rdflib::rdf_parse(ttl, format = "turtle")

  age_dt <- sh_constraint(
    component = "sh:DatatypeConstraintComponent",
    params    = list(datatype = "http://www.w3.org/2001/XMLSchema#string"),
    scope     = "property"
  )

  age_prop <- sh_property_shape(
    path        = "http://example.com/age",
    constraints = list(age_dt)
  )

  person_shape <- sh_node_shape(
    id         = "http://example.com/PersonShape",
    targets    = list(targetClass = "http://example.com/Person"),
    properties = list(age_prop)
  )

  sg <- sh_shape_graph(list(person_shape))

  report <- validate_shacl(rdf, sg)

  expect_true(nrow(report$results) >= 1)
  expect_false(report$conforms)
})
