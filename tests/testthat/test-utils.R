test_that("null-coalescing helper works", {
  expect_identical(NULL %||% 1L, 1L)
  expect_identical(letters %||% "fallback", letters)
})

test_that("scalar character validation handles NULL allowance", {
  expect_identical(check_scalar_character("id", "id"), "id")
  expect_null(check_scalar_character(NULL, "id", allow_null = TRUE))
  expect_error(check_scalar_character(letters, "id"), "scalar character")
  expect_error(check_scalar_character(NA_character_, "id"), "scalar character")
})

test_that("character vector validation detects NA and non-character", {
  expect_identical(check_character_vector(c("a", "b"), "vec"), c("a", "b"))
  expect_null(check_character_vector(NULL, "vec", allow_null = TRUE))
  expect_error(check_character_vector(c("a", NA), "vec"), "without NA")
  expect_error(check_character_vector(1:3, "vec"), "character vector")
})

test_that("list validators ensure structure and class membership", {
  expect_identical(check_list(list(), "lst"), list())
  expect_null(check_list(NULL, "lst", allow_null = TRUE))
  expect_error(check_list(1:3, "lst"), "must be a list")

  objects <- list(structure(list(), class = "demo"))
  expect_identical(check_list_of_class(objects, "demo", "objects"), objects)
  expect_error(check_list_of_class(list(list()), "demo", "objects"), "class 'demo'")
})

test_that("logical scalar validation rejects invalid inputs", {
  expect_true(check_logical_scalar(TRUE, "flag"))
  expect_error(check_logical_scalar(c(TRUE, FALSE), "flag"), "logical scalar")
  expect_error(check_logical_scalar(NA, "flag"), "logical scalar")
})
