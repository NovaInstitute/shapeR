test_that("visualiseSHACL labels nested shapes and prefixes", {
  skip_if_not_installed("ggraph")
  skip_if_not_installed("tidygraph")

  ttl <- system.file("extdata", "visualise-shacl.ttl", package = "shapeR")
  expect_true(nzchar(ttl))

  prefixes <- c(
    ex   = "http://example.com/ns#",
    base = "http://example.com/base#"
  )
  base_iri <- "http://example.com/base#"

  sg <- read_shacl(ttl, base_iri = base_iri, prefixes = prefixes, normalise_iris = TRUE)

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)
  pdf(tmp)
  on.exit(dev.off(), add = TRUE)

  graph <- visualiseSHACL(sg, layout = "kk")

  node_tbl <- graph |> tidygraph::activate(nodes) |> tibble::as_tibble()
  edge_tbl <- graph |> tidygraph::activate(edges) |> tibble::as_tibble()

  prop_node_label <- node_tbl$label[grepl("::", node_tbl$node)]
  expect_true(any(grepl("and:", prop_node_label)))
  expect_true(any(grepl("xone:", prop_node_label)))

  shape_label <- node_tbl$label[node_tbl$node == "http://example.com/base#PersonShape"]
  expect_true(any(grepl("base:PersonShape", shape_label, fixed = TRUE)))
  expect_true(any(grepl("or: ex:AltShape, ex:TrustedShape", shape_label, fixed = TRUE)))

  formatted_edges <- edge_tbl
  formatted_edges$to   <- contract_iri(formatted_edges$to, prefixes, base_iri)
  formatted_edges$from <- contract_iri(formatted_edges$from, prefixes, base_iri)

  relation_counts <- sort(table(formatted_edges$relation))
  expect_equal(unname(relation_counts), c(and = 1, node = 1, or = 4, property = 1, xone = 2))

  expect_true(any(formatted_edges$relation == "and" & formatted_edges$to == "ex:TrustedShape"))
  expect_true(any(formatted_edges$relation == "xone" & formatted_edges$to == "ex:FallbackShape"))
  expect_true(any(formatted_edges$relation == "node" & formatted_edges$to == "ex:FriendShape"))
})
