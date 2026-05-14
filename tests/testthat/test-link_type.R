test_that("link_type creates valid object", {
  lt <- link_type("RouteOrigin", from = "FlightRoute", to = "Airport",
                  cardinality = "many-to-one")
  expect_s3_class(lt, "ontology_link_type")
  expect_true(is_link_type(lt))
  expect_equal(lt$from, "FlightRoute")
  expect_equal(lt$to, "Airport")
  expect_equal(lt$cardinality, "many-to-one")
  expect_true(lt$directed)
})

test_that("link_type with join keys", {
  lt <- link_type("TestLink", "ObjA", "ObjB",
                  join_from_keys = c("a_id"),
                  join_to_keys = c("b_id"))
  expect_equal(lt$join$fromKeys, list("a_id"))
  expect_equal(lt$join$toKeys, list("b_id"))
})

test_that("link_type rejects invalid cardinality", {
  expect_error(
    link_type("Bad", "A", "B", cardinality = "invalid"),
    "must be one of"
  )
})

test_that("as_list.ontology_link_type works", {
  lt <- link_type("Test", "FromObj", "ToObj")
  l <- as_list(lt)
  expect_equal(l$id, "Test")
  expect_equal(l$from, "FromObj")
  expect_equal(l$to, "ToObj")
})

test_that("print.ontology_link_type runs without error", {
  lt <- link_type("Test", "A", "B", directed = FALSE)
  expect_output(print(lt), "<->")
})

# ---------------------------------------------------------------------------
# DuckPGQ source fields
# ---------------------------------------------------------------------------

test_that("link_type accepts all source fields", {
  lt <- link_type(
    "WorksFor", "Person", "Company",
    source_table     = "employment",
    source_from_col  = "person_id",
    source_to_col    = "company_id",
    source_filter_col = "rel_type",
    source_filter_val = c("employee", "contractor")
  )
  expect_equal(lt$source$table,     "employment")
  expect_equal(lt$source$fromCol,   "person_id")
  expect_equal(lt$source$toCol,     "company_id")
  expect_equal(lt$source$filterCol, "rel_type")
  expect_equal(lt$source$filterVal, list("employee", "contractor"))
})

test_that("link_type accepts source_table without filter", {
  lt <- link_type("RouteEdge", "Airport", "Airport",
                  source_table    = "routes",
                  source_from_col = "origin_id",
                  source_to_col   = "dest_id")
  expect_equal(lt$source$table,   "routes")
  expect_null(lt$source$filterCol)
  expect_null(lt$source$filterVal)
})

test_that("link_type rejects partial source trio (table only)", {
  expect_error(
    link_type("Bad", "A", "B", source_table = "tbl"),
    "must all be set together"
  )
})

test_that("link_type rejects partial source trio (table + fromCol only)", {
  expect_error(
    link_type("Bad", "A", "B",
              source_table = "tbl", source_from_col = "fk"),
    "must all be set together"
  )
})

test_that("link_type rejects filter_col without filter_val", {
  expect_error(
    link_type("Bad", "A", "B",
              source_table      = "tbl",
              source_from_col   = "fk1",
              source_to_col     = "fk2",
              source_filter_col = "dtype"),
    "both be set or both NULL"
  )
})

test_that("link_type rejects filter_val without filter_col", {
  expect_error(
    link_type("Bad", "A", "B",
              source_table      = "tbl",
              source_from_col   = "fk1",
              source_to_col     = "fk2",
              source_filter_val = c("x")),
    "both be set or both NULL"
  )
})

test_that("link_type rejects empty filter_val", {
  expect_error(
    link_type("Bad", "A", "B",
              source_table      = "tbl",
              source_from_col   = "fk1",
              source_to_col     = "fk2",
              source_filter_col = "dtype",
              source_filter_val = character(0)),
    "non-empty character vector"
  )
})

test_that("link_type source fields roundtrip through bundle JSON", {
  lt <- link_type(
    "WorksFor", "Person", "Company",
    source_table      = "employment",
    source_from_col   = "person_id",
    source_to_col     = "company_id",
    source_filter_col = "rel_type",
    source_filter_val = c("employee", "contractor")
  )
  b <- bundle(
    bundle_id = "RoundtripTest", bundle_version = "1.0.0",
    objects = list(
      object_type("Person",  list(property_def("person_id",  "string")), "person_id"),
      object_type("Company", list(property_def("company_id", "string")), "company_id")
    ),
    links = list(lt)
  )
  b2 <- bundle_from_json(bundle_to_json(b))
  lt2 <- b2$links[[1]]
  expect_equal(lt2$source$table,     "employment")
  expect_equal(lt2$source$fromCol,   "person_id")
  expect_equal(lt2$source$toCol,     "company_id")
  expect_equal(lt2$source$filterCol, "rel_type")
  expect_equal(unlist(lt2$source$filterVal), c("employee", "contractor"))
})
