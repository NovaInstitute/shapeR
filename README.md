# shapeR

shapeR is an R package that offers utility functions for working with SHACL shapes graphs. It provides helpers for parsing SHACL definitions from RDF, manipulating SHACL model objects, and preparing them for downstream validation or visualisation workflows.

## Features

- Parse SHACL shape graphs from RDF serialisations such as Turtle, RDF/XML, or JSON-LD via `read_shacl()`.
- Represent node and property shapes using lightweight model objects (`sh_node_shape`, `sh_property_shape`, `sh_constraint`, and `sh_shape_graph`).
- Shorten IRIs with a simple prefix map helper (`shorten_iri()`), and expand SHACL list structures via `expand_rdf_list()` and related utilities.
- Placeholder entry points for validation (`validate_shacl()`) and visualisation (`visualiseSHACL()`) that can be extended in future iterations.

## Installation

The package is not yet on CRAN. You can install it from a local checkout using the development tools ecosystem:

```r
# install.packages("devtools")
devtools::install()
```

If you prefer to avoid attaching the package, you can use `devtools::load_all()` while developing:

```r
devtools::load_all()
```

## Usage

Parsing a SHACL shapes graph from a Turtle file and inspecting the resulting object model:

```r
library(shapeR)

# Read and parse a SHACL shapes graph
shapes <- read_shacl("path/to/shapes.ttl", base_iri = "http://example.com/")

# Display a summary of the graph
print(shapes)

# Access individual shapes
names(shapes$shapes)
```

Working with IRIs and RDF lists exposed in the shapes graph:

```r
# Shorten an IRI using custom prefixes
shorten_iri(
  iri = "http://www.w3.org/ns/shacl#nodeShape",
  prefixes = c(sh = "http://www.w3.org/ns/shacl#")
)

# Expand RDF collection heads into a character vector
expand_rdf_list(head = "_:b0", triples = some_triples_dataframe)
```

The package currently provides scaffolding for validation and visualisation. Calling `validate_shacl()` or `visualiseSHACL()` will raise a clear error until implementations are added, making it explicit where future contributions are needed.

## Development

- Run tests with `devtools::test()`.
- Generate documentation from roxygen comments with `devtools::document()`.
- Pull requests and issues are welcome for extending validation and visualisation capabilities.
