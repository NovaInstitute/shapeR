# SHACL in the Tidyverse

This vignette sketches a tidyverse-friendly approach for working with SHACL shapes. A shape is represented as a tibble where **each row is a shape** and **columns capture top-level SHACL terms**:

- `id`: a stable identifier (URI or blank node label)
- `type`: always `sh:NodeShape`
- `context`: prefix mappings needed to interpret compact IRIs
- `targetClass`: the class IRIs targeted by the shape
- `property`: one or more property constraints, expressed as a nested tibble

Nested columns allow each row to carry the full structure of a shape while still benefiting from tidy workflows (filtering, mapping, unnesting, etc.).

## Column design

| Column | Type | Notes |
| --- | --- | --- |
| `id` | character | Unnested for readability and joins. |
| `type` | character | Constant `"sh:NodeShape"`; also unnested. |
| `context` | list-column of named lists or tibbles | Prefix/namespace bindings; can be shared across shapes or merged. |
| `targetClass` | list-column of character vectors | Supports multiple classes per shape. |
| `property` | list-column of tibbles | Each tibble holds property constraints, one row per property path. |

## Minimal example

```r
library(tibble)

shapes <- tibble::tibble(
  id = c("ex:PersonShape", "ex:BookShape"),
  type = "sh:NodeShape",
  context = list(
    tibble::tibble(prefix = c("ex", "schema"), iri = c("http://example.com/", "http://schema.org/")),
    tibble::tibble(prefix = c("ex", "schema"), iri = c("http://example.com/", "http://schema.org/"))
  ),
  targetClass = list(
    c("schema:Person"),
    c("schema:Book")
  ),
  property = list(
    tibble::tibble(
      path = c("schema:name", "schema:birthDate"),
      datatype = c("xsd:string", "xsd:date"),
      minCount = c(1L, 0L),
      maxCount = c(1L, 1L)
    ),
    tibble::tibble(
      path = c("schema:name", "schema:author"),
      datatype = c("xsd:string", NA),
      class = c(NA, "schema:Person"),
      minCount = c(1L, 1L)
    )
  )
)
```

In this layout, `id` and `type` remain plain columns for easy joins and human inspection. The other columns are list-columns so they can hold multiple values per shape without creating duplicate rows.

## Typical operations

### Filter shapes by target class

```r
library(dplyr)

person_shapes <- shapes |> 
  filter(purrr::map_lgl(targetClass, ~ "schema:Person" %in% .x))
```

### Add or modify property constraints

```r
add_required_email <- function(shape_tbl) {
  shape_tbl |> mutate(
    property = purrr::map(property, ~ tibble::add_row(.x,
      path = "schema:email",
      datatype = "xsd:string",
      minCount = 1L,
      maxCount = 1L
    ))
  )
}
```

### Unnest for exporting

```r
properties_long <- shapes |> 
  select(id, property) |> 
  tidyr::unnest(property)
```

This approach lets you keep shapes compact for manipulation while expanding them when generating SHACL documents or reports.

## Alternative layouts

- **Fully unnested rows**: Unnest `property` so each row is a property constraint. This simplifies constraint-level filtering but duplicates the shape metadata.
- **Wide property columns**: Store each constraint as a nested list-column rather than a tibble; this can be friendlier for JSON export but less convenient for tidy operations.
- **Context per data frame**: Hold `context` outside the shape tibble (e.g., as an attribute) when all shapes share the same prefixes.

Choose the layout that best balances readability, deduplication, and the downstream tasks (validation, export, visualization) you plan to perform.
