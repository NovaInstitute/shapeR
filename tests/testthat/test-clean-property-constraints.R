test_that("clean_property_constraints removes duplicates and unnests singletons", {
  prop <- tibble::tibble(
    path = c("ex:name", "ex:name"),
    datatype = list("xsd:string", "xsd:string"),
    single_value = list("only", "only"),
    multi_value = list(c("a", "b"), c("a", "b")),
    empty_list = list(character(0), character(0)),
    na_list = list(NA_character_, NA_character_)
  )

  cleaned <- clean_property_constraints(prop)

  expect_equal(nrow(cleaned), 1L)
  expect_true(is.character(cleaned$datatype))
  expect_identical(cleaned$single_value, "only")
  expect_true(is.list(cleaned$multi_value))
  expect_false("empty_list" %in% names(cleaned))
  expect_false("na_list" %in% names(cleaned))
})

