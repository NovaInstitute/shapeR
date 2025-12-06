test_that("read_shacl expands RDF list constraints", {
  skip_if_not_installed("rdflib")
  skip_if_not_installed("dplyr")

  ttl <- "@prefix sh: <http://www.w3.org/ns/shacl#> .\n@prefix ex: <http://example.com/> .\n\nex:RootShape a sh:NodeShape ;\n  sh:targetClass ex:Thing ;\n  sh:or ( ex:AltShape ex:ThirdShape ) ;\n  sh:and ( [ sh:class ex:Foo ] ) ;\n  sh:property [\n    sh:path ex:prop ;\n    sh:in ( ex:Choice1 ex:Choice2 ) ;\n    sh:or ( ex:AltShape ex:ThirdShape ) ;\n    sh:xone ( ex:OtherShape ) ;\n    sh:node ex:AltShape\n  ] .\n\nex:AltShape a sh:NodeShape ;\n  sh:property [\n    sh:path ex:child ;\n    sh:in ( ex:Child1 ex:Child2 ex:Child3 )\n  ] .\n\nex:ThirdShape a sh:NodeShape .\nex:OtherShape a sh:NodeShape .\n"

  tf <- tempfile(fileext = ".ttl")
  writeLines(ttl, tf)

  sg <- read_shacl(tf)
  on.exit(unlink(tf), add = TRUE)

  main_id <- "<http://example.com/RootShape>"
  expect_true(main_id %in% names(sg$shapes))

  root_shape <- sg$shapes[[main_id]]
  expect_equal(root_shape$targets$targetClass, "<http://example.com/Thing>")

  # node-level list constraints
  get_component <- function(constraints, name) {
    idx <- which(vapply(constraints, function(x) x$component == name, logical(1)))
    expect_gte(length(idx), 1L)
    constraints[[idx[[1L]]]]
  }

  node_or <- get_component(root_shape$constraints, "sh:OrConstraintComponent")
  expect_equal(node_or$params$or, c("<http://example.com/AltShape>", "<http://example.com/ThirdShape>"))

  node_and <- get_component(root_shape$constraints, "sh:AndConstraintComponent")
  expect_length(node_and$params$and, 1L)

  expect_length(root_shape$properties, 1L)
  prop_shape <- root_shape$properties[[1L]]
  expect_equal(prop_shape$path, "<http://example.com/prop>")

  prop_in <- get_component(prop_shape$constraints, "sh:InConstraintComponent")
  expect_equal(prop_in$params[["in"]], c("<http://example.com/Choice1>", "<http://example.com/Choice2>"))

  expect_equal(prop_shape$nested$node, "<http://example.com/AltShape>")
  expect_equal(prop_shape$nested$or, c("<http://example.com/AltShape>", "<http://example.com/ThirdShape>"))
  expect_equal(prop_shape$nested$xone, "<http://example.com/OtherShape>")

  alt_id <- "<http://example.com/AltShape>"
  expect_true(alt_id %in% names(sg$shapes))
  alt_shape <- sg$shapes[[alt_id]]
  expect_length(alt_shape$properties, 1L)

  alt_prop <- alt_shape$properties[[1L]]
  alt_in <- get_component(alt_prop$constraints, "sh:InConstraintComponent")
  expect_equal(alt_in$params[["in"]],
               c("<http://example.com/Child1>", "<http://example.com/Child2>", "<http://example.com/Child3>"))
})
