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

