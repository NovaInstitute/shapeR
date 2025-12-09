test_that("plot_property_column renders visNetwork graph", {
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("visNetwork")

  ttl <- system.file("extdata", "visualise-shacl.ttl", package = "shapeR")
  tidy_shapes <- read_shacl_tidy(ttl)

  expect_s3_class(plot_property_column(tidy_shapes), "tbl_graph")

  first_id <- tidy_shapes$id[[1L]]
  expect_s3_class(plot_property_column(tidy_shapes, shape_id = first_id), "tbl_graph")
})

test_that("plot_property_column validates inputs", {
  expect_error(plot_property_column(list()), "data frame")

  bad_df <- data.frame(foo = 1)
  expect_error(plot_property_column(bad_df), "include columns `id` and `property`")
})

test_that("plot_property_column handles missing cardinalities", {
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("visNetwork")

  property_tbl <- tibble::tibble(
    path = c("ex:prop1", "ex:prop2"),
    datatype = list(character(), character()),
    class = list(character(), character()),
    minCount = list(integer(0), c(1L, 2L)),
    maxCount = list(integer(0), c(1L, 2L)),
    `in` = list(list(), list()),
    node = list(character(), character()),
    or = list(list(), list()),
    and = list(list(), list()),
    xone = list(list(), list())
  )

  shapes <- tibble::tibble(
    id = "ex:Shape",
    property = list(property_tbl)
  )

  expect_s3_class(plot_property_column(shapes), "tbl_graph")
})

test_that("plot_property_column collapses duplicated property details", {
  skip_if_not_installed("tidygraph")
  skip_if_not_installed("visNetwork")

  property_tbl <- tibble::tibble(
    path = c("ex:prop", "ex:prop"),
    datatype = list("xsd:string", "xsd:string"),
    class = list(character(), character()),
    minCount = list(1L, 1L),
    maxCount = list(1L, 2L),
    `in` = list(list(), list()),
    node = list(character(), character()),
    or = list(list(), list()),
    and = list(list(), list()),
    xone = list(list(), list())
  )

  shapes <- tibble::tibble(
    id = "ex:Shape",
    property = list(property_tbl)
  )

  expect_s3_class(plot_property_column(shapes), "tbl_graph")
})

