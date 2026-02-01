test_that("object_type creates valid object", {
  ot <- object_type(
    id = "Airport",
    properties = list(
      property_def("airport_id", "string", nullable = FALSE),
      property_def("name", "string")
    ),
    primary_key = "airport_id",
    display_name = "Airport"
  )
  expect_s3_class(ot, "ontology_object_type")
  expect_true(is_object_type(ot))
  expect_equal(ot$id, "Airport")
  expect_length(ot$properties, 2)
  expect_s3_class(ot$primaryKey, "ontology_primary_key_def")
})

test_that("object_type accepts primary_key_def directly", {
  pk <- primary_key_def("id_col")
  ot <- object_type("Test", list(property_def("id_col", "string")), pk)
  expect_s3_class(ot$primaryKey, "ontology_primary_key_def")
})

test_that("object_type with source binding", {
  ot <- object_type(
    "Test",
    list(property_def("test_id", "string")),
    "test_id",
    source_kind = "table",
    source_table = "test_table",
    source_schema = "public"
  )
  expect_equal(ot$source$kind, "table")
  expect_equal(ot$source$table, "test_table")
})

test_that("object_type with implements", {
  ot <- object_type(
    "TestObj",
    list(property_def("obj_id", "string")),
    "obj_id",
    implements = c("GeoLocated", "Named")
  )
  expect_equal(ot$implements, list("GeoLocated", "Named"))
})

test_that("as_list.ontology_object_type works", {
  ot <- object_type("X", list(property_def("xid", "string")), "xid")
  l <- as_list(ot)
  expect_type(l, "list")
  expect_equal(l$id, "X")
  expect_length(l$properties, 1)
  expect_equal(l$primaryKey$properties, list("xid"))
})

test_that("print.ontology_object_type runs without error", {
  ot <- object_type("X", list(property_def("xid", "string")), "xid")
  expect_output(print(ot), "ObjectType")
})
