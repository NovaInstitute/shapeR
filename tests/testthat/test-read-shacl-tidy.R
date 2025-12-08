test_that("read_shacl_tidy returns tidy tibble layout", {
  skip_if_not_installed("rdflib")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")

  ttl <- "@prefix sh: <http://www.w3.org/ns/shacl#> .\n@prefix ex: <http://example.com/> .\n@prefix schema: <http://schema.org/> .\n@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\nex:PersonShape a sh:NodeShape ;\n  sh:targetClass schema:Person ;\n  sh:property [\n    sh:path schema:name ;\n    sh:datatype xsd:string ;\n    sh:minCount 1 ;\n    sh:maxCount 1\n  ] ;\n  sh:property [\n    sh:path schema:birthDate ;\n    sh:datatype xsd:date ;\n    sh:maxCount 1\n  ] .\n\nex:BookShape a sh:NodeShape ;\n  sh:targetClass schema:Book ;\n  sh:property [\n    sh:path schema:author ;\n    sh:class schema:Person ;\n    sh:minCount 1\n  ] .\n"

  tf <- tempfile(fileext = ".ttl")
  writeLines(ttl, tf)
  on.exit(unlink(tf), add = TRUE)

  tidy_shapes <- read_shacl_tidy(
    tf,
    prefixes = c(
      ex = "http://example.com/",
      schema = "http://schema.org/",
      xsd = "http://www.w3.org/2001/XMLSchema#"
    ),
    prefer_curie = TRUE
  )

  expect_s3_class(tidy_shapes, "tbl_df")
  expect_equal(nrow(tidy_shapes), 2L)
  expect_equal(tidy_shapes$type, rep("sh:NodeShape", 2L))

  expect_setequal(tidy_shapes$id, c("ex:PersonShape", "ex:BookShape"))
  expect_true(all(vapply(tidy_shapes$context, inherits, logical(1), "data.frame")))

  person_row <- tidy_shapes[tidy_shapes$id == "ex:PersonShape", ]
  expect_equal(person_row$targetClass[[1]], "schema:Person")

  person_props <- person_row$property[[1]]
  expect_true(all(c("path", "datatype", "minCount", "maxCount") %in% names(person_props)))
  counts <- setNames(person_props$minCount, person_props$path)
  expect_equal(counts[["schema:name"]], 1L)
  expect_true(is.na(counts[["schema:birthDate"]]))

  book_row <- tidy_shapes[tidy_shapes$id == "ex:BookShape", ]
  book_props <- book_row$property[[1]]
  expect_equal(book_props$path, "schema:author")
  expect_equal(book_props$class, "schema:Person")
  expect_equal(book_props$minCount, 1L)
})
