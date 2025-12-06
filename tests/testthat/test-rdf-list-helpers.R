test_that("expand_rdf_list walks rdf:first/rest chain", {
  const <- shapeR:::rdf_list_constants()

  triples <- data.frame(
    subject   = c("_:head", "_:head", "_:b1", "_:b1"),
    predicate = c(const$first, const$rest, const$first, const$rest),
    object    = c("<http://example.com/A>", "_:b1", "<http://example.com/B>", const$nil),
    stringsAsFactors = FALSE
  )

  expect_equal(
    shapeR::expand_rdf_list("_:head", triples),
    c("<http://example.com/A>", "<http://example.com/B>")
  )
})

test_that("expand_rdf_list handles rdf:nil and non-lists", {
  const <- shapeR:::rdf_list_constants()
  triples <- data.frame(
    subject   = character(),
    predicate = character(),
    object    = character(),
    stringsAsFactors = FALSE
  )

  expect_equal(shapeR::expand_rdf_list(const$nil, triples), character())
  expect_equal(shapeR::expand_rdf_list("<http://example.com/not-a-list>", triples), "<http://example.com/not-a-list>")
})
