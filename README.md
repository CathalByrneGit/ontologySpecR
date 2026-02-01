# ontologySpecR

Core type system and interchange format for building ontology-based R package ecosystems.

## Overview

`ontologySpecR` defines canonical S3 classes and a JSON Schema for ontology primitives:

- **Object types** — schema definitions of real-world entities
- **Link types** — typed relationships between object types
- **Interfaces** — polymorphic "shape contracts" for object type conformance
- **Action types** — parameterized operations with effects and policy hooks
- **Queries** — reusable, parameterized query definitions

These primitives are packaged into **bundles** — versioned, distributable ontology specifications that can be serialized to/from JSON and validated against the built-in JSON Schema.

## Design goals

- **Bundle-first**: one JSON file can describe a complete, deployable ontology package
- **Backend-agnostic**: object data binding is expressed generically (DBI, Arrow, DuckDB, etc.)
- **Composable**: references between entities use stable IDs
- **Extensible**: `extensions` fields exist throughout for non-core concepts
- **Interoperable**: shared IR so downstream packages (objectTypesR, objectSetsR, vertexR, etc.) can agree on structure without tight coupling

## Installation

```r
# Install from GitHub
# devtools::install_github("CathalByrneGit/ontologySpecR")
```

## Quick start

```r
library(ontologySpecR)

# Define an object type
airport <- object_type(
  id = "Airport",
  properties = list(
    property_def("airport_id", "string", nullable = FALSE),
    property_def("name", "string"),
    property_def("latitude", "number"),
    property_def("longitude", "number")
  ),
  primary_key = "airport_id",
  display_name = "Airport",
  implements = "GeoLocated",
  source_kind = "table",
  source_table = "airports"
)

# Define an interface
geo_located <- interface_type(
  id = "GeoLocated",
  required_properties = list(
    property_requirement("latitude", "number"),
    property_requirement("longitude", "number")
  )
)

# Define a link type
route_origin <- link_type(
  id = "RouteOrigin",
  from = "FlightRoute",
  to = "Airport",
  cardinality = "many-to-one",
  join_from_keys = "origin_id",
  join_to_keys = "airport_id"
)

# Package into a bundle
b <- bundle(
  bundle_id = "aviation-demo",
  bundle_version = "0.1.0",
  objects = list(airport),
  interfaces = list(geo_located),
  links = list(route_origin)
)

# Serialize to JSON
json <- bundle_to_json(b)

# Validate against schema
validate_bundle(b)  # TRUE

# Write / read from disk
write_bundle(b, "my-ontology.json")
b2 <- read_bundle("my-ontology.json")
```

## Ecosystem

`ontologySpecR` is the shared contract for a modular R package ecosystem inspired by Palantir Foundry's ontology layer:

| Package | Role |
|---|---|
| **ontologySpecR** | Core type system + interchange (this package) |
| ontologyR | Governance: versioned definitions, audits, drift detection |
| objectTypesR | Object type schema + backend source mapping |
| linkTypesR | Relationship schema + constraint checking |
| interfacesR | Polymorphism / conformance testing |
| objectSetsR | ObjectSet algebra + traversal (lazy queries) |
| actionTypesR | Action definitions + submission lifecycle |
| vertexR | System graphs: monitoring, simulation, optimization |
| objectExplorerR | Interactive exploration UI (Shiny) |

## JSON Schema

The bundle schema is shipped with the package at `inst/schemas/ontologySpecR.bundle.schema.json` and follows JSON Schema Draft 2020-12.

## License

MIT
