test_that("IRI helpers expand, resolve, and contract", {
  prefixes <- c(ex = "http://example.com/")

  expect_true(is_curie("ex:Thing"))
  expect_false(is_curie("<http://example.com/Thing>"))

  expect_equal(
    expand_iri("ex:Thing", prefixes = prefixes),
    "<http://example.com/Thing>"
  )

  expect_equal(
    resolve_base_iri("relative", base_iri = "http://base.example/"),
    "<http://base.example/relative>"
  )

  expect_equal(
    normalise_iri("child", prefixes = prefixes, base_iri = "http://example.com/"),
    "<http://example.com/child>"
  )

  expect_equal(
    contract_iri("<http://example.com/Thing>", prefixes = prefixes),
    "ex:Thing"
  )
})

test_that("shape constructors can normalise IRIs", {
  prefixes <- c(ex = "http://example.com/")
  prop <- sh_property_shape(
    id = "ex:propShape",
    path = "child",
    nested = list(node = "ex:other", or = "ex:alt"),
    prefixes = prefixes,
    base_iri = "http://example.com/",
    normalise = TRUE
  )

  expect_equal(prop$id, "<http://example.com/propShape>")
  expect_equal(prop$path, "<http://example.com/child>")
  expect_equal(prop$nested$node, "<http://example.com/other>")
  expect_equal(prop$nested$or, "<http://example.com/alt>")

  node <- sh_node_shape(
    id = "ex:node",
    targets = list(targetClass = "ex:Thing"),
    properties = list(prop),
    prefixes = prefixes,
    base_iri = "http://example.com/",
    normalise = TRUE
  )

  expect_equal(node$id, "<http://example.com/node>")
  expect_equal(node$targets$targetClass, "<http://example.com/Thing>")
  expect_equal(node$properties[[1]]$path, "<http://example.com/child>")
})

test_that("read_shacl stores prefixes and base IRI", {
  skip_if_not_installed("rdflib")
  skip_if_not_installed("dplyr")

  ttl <- "@prefix ex: <http://example.com/> .\nex:Shape a ex:Class .\n"
  tf <- tempfile(fileext = ".ttl")
  writeLines(ttl, tf)
  on.exit(unlink(tf), add = TRUE)

  sg <- read_shacl(tf, base_iri = "http://example.com/", prefixes = c(ex = "http://example.com/"), normalise_iris = TRUE)

  expect_equal(sg$base_iri, "http://example.com/")
  expect_equal(sg$prefixes, c(ex = "http://example.com/"))
})
