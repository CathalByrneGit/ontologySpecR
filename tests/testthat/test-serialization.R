test_that("bundle_to_json produces valid JSON string", {
  b <- bundle("json-test", "0.1.0")
  json <- bundle_to_json(b)
  expect_type(json, "character")
  # Should be parseable
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$bundleId, "json-test")
})

test_that("bundle_to_json rejects non-bundle", {
  expect_error(bundle_to_json(list()), "must be an ontology_bundle")
})

test_that("write_bundle and read_bundle round-trip", {
  b <- bundle(
    "file-test", "1.0.0",
    objects = list(
      object_type("Widget",
        list(property_def("widget_id", "integer", nullable = FALSE)),
        "widget_id"
      )
    )
  )

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp))

  write_bundle(b, tmp)
  expect_true(file.exists(tmp))

  b2 <- read_bundle(tmp)
  expect_s3_class(b2, "ontology_bundle")
  expect_equal(b2$bundleId, "file-test")
  expect_equal(b2$objects[[1]]$id, "Widget")
})

test_that("bundle_from_json handles empty arrays", {
  json <- '{
    "specVersion": "0.1.0",
    "bundleId": "empty-test",
    "bundleVersion": "0.1.0",
    "objects": [],
    "links": [],
    "interfaces": [],
    "actions": [],
    "queries": []
  }'
  b <- bundle_from_json(json)
  expect_s3_class(b, "ontology_bundle")
  expect_length(b$objects, 0)
  expect_length(b$links, 0)
})
