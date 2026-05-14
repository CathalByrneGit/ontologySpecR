# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

make_pgq_bundle <- function() {
  bundle(
    bundle_id = "aviation-pgq", bundle_version = "1.0.0",
    objects = list(
      object_type("Airport",
        list(property_def("airport_id", "string", nullable = FALSE),
             property_def("name",       "string")),
        "airport_id",
        source_kind  = "table",
        source_table = "airports"
      ),
      object_type("Airline",
        list(property_def("airline_id", "string", nullable = FALSE),
             property_def("iata_code",  "string")),
        "airline_id",
        source_kind  = "table",
        source_table = "airlines"
      )
    ),
    links = list(
      link_type(
        "FlightRoute", "Airport", "Airport",
        source_table    = "routes",
        source_from_col = "origin_id",
        source_to_col   = "dest_id"
      )
    )
  )
}

make_filtered_bundle <- function() {
  bundle(
    bundle_id = "corp-pgq", bundle_version = "1.0.0",
    objects = list(
      object_type("Person",
        list(property_def("person_id", "string", nullable = FALSE)),
        "person_id",
        source_kind  = "table",
        source_table = "persons"
      ),
      object_type("Company",
        list(property_def("company_id", "string", nullable = FALSE)),
        "company_id",
        source_kind  = "table",
        source_table = "companies"
      )
    ),
    links = list(
      link_type(
        "WorksFor", "Person", "Company",
        source_table      = "relationships",
        source_from_col   = "person_id",
        source_to_col     = "company_id",
        source_filter_col = "rel_type",
        source_filter_val = c("employee", "contractor")
      )
    )
  )
}

# ---------------------------------------------------------------------------
# bundle_to_pgq_ddl: basic structure
# ---------------------------------------------------------------------------

test_that("bundle_to_pgq_ddl returns a character string", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_type(sql, "character")
  expect_length(sql, 1L)
})

test_that("bundle_to_pgq_ddl starts with CREATE OR REPLACE PROPERTY GRAPH", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_match(sql, "^CREATE OR REPLACE PROPERTY GRAPH")
})

test_that("bundle_to_pgq_ddl with replace=FALSE uses CREATE PROPERTY GRAPH", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle(), replace = FALSE)
  expect_match(sql, "^CREATE PROPERTY GRAPH ")
  expect_false(grepl("OR REPLACE", sql))
})

test_that("bundle_to_pgq_ddl uses bundleId as graph name by default", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_match(sql, "aviation_pgq")
})

test_that("bundle_to_pgq_ddl accepts explicit graph_name", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle(), graph_name = "my_graph")
  expect_match(sql, "my_graph")
})

test_that("bundle_to_pgq_ddl rejects invalid SQL identifier as graph_name", {
  expect_error(
    bundle_to_pgq_ddl(make_pgq_bundle(), graph_name = "123bad"),
    "valid SQL identifier"
  )
})

test_that("bundle_to_pgq_ddl ends with semicolon", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_match(sql, ";$")
})

# ---------------------------------------------------------------------------
# VERTEX TABLES
# ---------------------------------------------------------------------------

test_that("bundle_to_pgq_ddl includes VERTEX TABLES block", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_match(sql, "VERTEX TABLES")
})

test_that("bundle_to_pgq_ddl generates correct vertex table entries", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_match(sql, "airports LABEL Airport")
  expect_match(sql, "airlines LABEL Airline")
})

test_that("bundle_to_pgq_ddl warns and skips object types without source_table", {
  b <- bundle(
    bundle_id = "NoPGQ", bundle_version = "1.0.0",
    objects = list(
      object_type("Airport",
        list(property_def("airport_id", "string")), "airport_id",
        source_kind  = "table",
        source_table = "airports"
      ),
      object_type("Ghost",
        list(property_def("ghost_id", "string")), "ghost_id"
        # no source_table
      )
    ),
    links = list()
  )
  expect_warning(
    sql <- bundle_to_pgq_ddl(b),
    "no source\\$table"
  )
  expect_match(sql, "airports LABEL Airport")
  expect_false(grepl("Ghost", sql))
})

test_that("bundle_to_pgq_ddl aborts when no vertex tables available", {
  b <- bundle(
    bundle_id = "Empty", bundle_version = "1.0.0",
    objects = list(
      object_type("Ghost", list(property_def("ghost_id", "string")), "ghost_id")
    ),
    links = list()
  )
  expect_error(
    suppressWarnings(bundle_to_pgq_ddl(b)),
    "No object types with source\\$table"
  )
})

test_that("bundle_to_pgq_ddl filters vertex tables by vertex_tables arg", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle(), vertex_tables = "Airport")
  expect_match(sql, "airports LABEL Airport")
  expect_false(grepl("airlines", sql))
})

# ---------------------------------------------------------------------------
# EDGE TABLES
# ---------------------------------------------------------------------------

test_that("bundle_to_pgq_ddl includes EDGE TABLES block when links have source", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_match(sql, "EDGE TABLES")
})

test_that("bundle_to_pgq_ddl generates SOURCE KEY and DESTINATION KEY", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_match(sql, "SOURCE KEY \\(origin_id\\) REFERENCES airports \\(airport_id\\)")
  expect_match(sql, "DESTINATION KEY \\(dest_id\\) REFERENCES airports \\(airport_id\\)")
})

test_that("bundle_to_pgq_ddl generates LABEL for edge", {
  sql <- bundle_to_pgq_ddl(make_pgq_bundle())
  expect_match(sql, "LABEL FlightRoute")
})

test_that("bundle_to_pgq_ddl generates WHERE clause for filtered edges", {
  sql <- bundle_to_pgq_ddl(make_filtered_bundle())
  expect_match(sql, "WHERE rel_type IN")
  expect_match(sql, "'employee'")
  expect_match(sql, "'contractor'")
})

test_that("bundle_to_pgq_ddl SQL-escapes single quotes in filter values", {
  b <- bundle(
    bundle_id = "corp-pgq", bundle_version = "1.0.0",
    objects = list(
      object_type("Person",
        list(property_def("person_id", "string")), "person_id",
        source_kind = "table", source_table = "persons"),
      object_type("Company",
        list(property_def("company_id", "string")), "company_id",
        source_kind = "table", source_table = "companies")
    ),
    links = list(
      link_type("WorksFor", "Person", "Company",
                source_table      = "relationships",
                source_from_col   = "person_id",
                source_to_col     = "company_id",
                source_filter_col = "rel_type",
                source_filter_val = c("it's complicated", "normal"))
    )
  )
  sql <- bundle_to_pgq_ddl(b)
  expect_match(sql, "it''s complicated")
})

test_that("bundle_to_pgq_ddl warns and generates vertex-only graph when no links have source", {
  b <- bundle(
    bundle_id = "NoEdges", bundle_version = "1.0.0",
    objects = list(
      object_type("Airport",
        list(property_def("airport_id", "string")), "airport_id",
        source_kind = "table", source_table = "airports")
    ),
    links = list(
      link_type("SomeLink", "Airport", "Airport")
      # no source fields
    )
  )
  expect_warning(
    sql <- bundle_to_pgq_ddl(b),
    "vertex-only"
  )
  expect_false(grepl("EDGE TABLES", sql))
})

test_that("bundle_to_pgq_ddl filters edge tables by edge_tables arg", {
  b <- bundle(
    bundle_id = "multi-pgq", bundle_version = "1.0.0",
    objects = list(
      object_type("Airport",
        list(property_def("airport_id", "string")), "airport_id",
        source_kind = "table", source_table = "airports")
    ),
    links = list(
      link_type("RouteA", "Airport", "Airport",
                source_table = "routes", source_from_col = "from_id", source_to_col = "to_id"),
      link_type("RouteB", "Airport", "Airport",
                source_table = "routes2", source_from_col = "from_id", source_to_col = "to_id")
    )
  )
  sql <- bundle_to_pgq_ddl(b, edge_tables = "RouteA")
  expect_match(sql, "LABEL RouteA")
  expect_false(grepl("RouteB", sql))
})

test_that("bundle_to_pgq_ddl rejects non-bundle input", {
  expect_error(bundle_to_pgq_ddl(list()), "ontology_bundle")
})

# ---------------------------------------------------------------------------
# PK column resolution
# ---------------------------------------------------------------------------

test_that("bundle_to_pgq_ddl uses source$column for PK when set", {
  b <- bundle(
    bundle_id = "pk-test", bundle_version = "1.0.0",
    objects = list(
      object_type("Widget",
        list(
          property_def("widget_id", "string", nullable = FALSE,
                       source_column = "wid"),
          property_def("name", "string")
        ),
        "widget_id",
        source_kind = "table", source_table = "widgets"
      ),
      object_type("Factory",
        list(property_def("factory_id", "string", nullable = FALSE)),
        "factory_id",
        source_kind = "table", source_table = "factories"
      )
    ),
    links = list(
      link_type("MadeAt", "Widget", "Factory",
                source_table = "production", source_from_col = "w_id", source_to_col = "f_id")
    )
  )
  sql <- bundle_to_pgq_ddl(b)
  # PK for Widget should use mapped column name "wid"
  expect_match(sql, "REFERENCES widgets \\(wid\\)")
  # PK for Factory falls back to property id "factory_id"
  expect_match(sql, "REFERENCES factories \\(factory_id\\)")
})
