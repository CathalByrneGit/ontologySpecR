test_that("bundle creates valid object", {
  b <- bundle(
    bundle_id = "test-bundle",
    bundle_version = "1.0.0",
    objects = list(
      object_type("Obj1",
        list(property_def("obj_id", "string")),
        "obj_id"
      )
    )
  )
  expect_s3_class(b, "ontology_bundle")
  expect_true(is_bundle(b))
  expect_equal(b$bundleId, "test-bundle")
  expect_equal(b$bundleVersion, "1.0.0")
  expect_equal(b$specVersion, "0.1.0")
  expect_length(b$objects, 1)
})

test_that("bundle with metadata", {
  b <- bundle(
    "with-meta", "0.1.0",
    metadata = list(
      name = "Test",
      description = "A test bundle",
      authors = list("Author One"),
      tags = list("test")
    )
  )
  expect_equal(b$metadata$name, "Test")
})

test_that("print.ontology_bundle runs without error", {
  b <- bundle("test-pkg", "0.1.0")
  expect_output(print(b), "OntologyBundle")
  expect_output(print(b), "test-pkg")
  expect_output(print(b), "concepts")
})

test_that("as_list.ontology_bundle round-trips through JSON", {
  b <- bundle(
    bundle_id = "roundtrip",
    bundle_version = "1.0.0",
    objects = list(
      object_type("Airport",
        list(
          property_def("airport_id", "string", nullable = FALSE),
          property_def("name", "string")
        ),
        "airport_id",
        implements = "GeoLocated",
        source_kind = "table",
        source_table = "airports"
      )
    ),
    links = list(
      link_type("RouteOrigin", "FlightRoute", "Airport",
                cardinality = "many-to-one",
                join_from_keys = "origin_id",
                join_to_keys = "airport_id")
    ),
    interfaces = list(
      interface_type("GeoLocated",
        required_properties = list(
          property_requirement("latitude", "number"),
          property_requirement("longitude", "number")
        )
      )
    ),
    actions = list(
      action_type("UpdateStatus", "Airport",
        parameters = list(parameter_def("status", "string", required = TRUE)),
        effects = list(effect_def("update", "Airport")),
        impl_kind = "r",
        impl_entrypoint = "update_status"
      )
    ),
    queries = list(
      query_def("NearbyAirports", "objectSet",
        returns_object_type_id = "Airport",
        parameters = list(parameter_def("lat", "number", required = TRUE)),
        def_kind = "sql",
        def_body = "SELECT * FROM airports"
      )
    )
  )

  json <- bundle_to_json(b)
  b2 <- bundle_from_json(json)

  expect_s3_class(b2, "ontology_bundle")
  expect_equal(b2$bundleId, "roundtrip")
  expect_length(b2$objects, 1)
  expect_length(b2$links, 1)
  expect_length(b2$interfaces, 1)
  expect_length(b2$actions, 1)
  expect_length(b2$queries, 1)

  # Deep equality checks
  expect_equal(b2$objects[[1]]$id, "Airport")
  expect_s3_class(b2$objects[[1]], "ontology_object_type")
  expect_s3_class(b2$links[[1]], "ontology_link_type")
  expect_s3_class(b2$interfaces[[1]], "ontology_interface_type")
  expect_s3_class(b2$actions[[1]], "ontology_action_type")
  expect_s3_class(b2$queries[[1]], "ontology_query_def")
})
