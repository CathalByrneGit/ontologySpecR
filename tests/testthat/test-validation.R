test_that("validate_bundle returns TRUE for valid bundle", {
  b <- bundle(
    "valid-test", "1.0.0",
    objects = list(
      object_type("Thing",
        list(property_def("thing_id", "string", nullable = FALSE)),
        "thing_id"
      )
    )
  )
  result <- validate_bundle(b)
  expect_true(result)
})

test_that("validate_bundle(error = FALSE) returns character errors for invalid JSON", {
  bad_json <- '{"bundleId": "no-version"}'
  result <- validate_bundle(bad_json, error = FALSE)
  expect_type(result, "character")
  expect_gt(length(result), 0L)
})

test_that("validate_bundle(error = TRUE) throws on invalid JSON", {
  bad_json <- '{"bundleId": "no-version"}'
  expect_error(validate_bundle(bad_json, error = TRUE), "validation failed")
})

test_that("validate_bundle returns empty character on success with error = FALSE", {
  b <- bundle(
    "valid-test2", "1.0.0",
    objects = list(
      object_type("Thing",
        list(property_def("thing_id", "string", nullable = FALSE)),
        "thing_id"
      )
    )
  )
  result <- validate_bundle(b, error = FALSE)
  expect_type(result, "character")
  expect_length(result, 0L)
})

test_that("validate_bundle works with verbose = TRUE on valid input", {
  b <- bundle("verbose-test", "1.0.0")
  expect_message(validate_bundle(b, verbose = TRUE), "valid")
})

test_that("validate_bundle rejects non-bundle non-string input", {
  expect_error(validate_bundle(42), "must be an ontology_bundle")
})

test_that("validate_bundle validates the example bundle file", {
  example_path <- system.file("examples", "aviation-demo.json",
                              package = "ontologySpecR")
  skip_if(example_path == "", message = "Example file not found (not installed)")
  json <- paste(readLines(example_path, warn = FALSE), collapse = "\n")
  result <- validate_bundle(json)
  expect_true(result)
})

test_that("validate_bundle accepts a bundle with concepts and templates", {
  b <- bundle(
    "concept-valid", "1.0.0",
    concepts = list(
      concept_def("busy_airport", "Airport", "operations", 1L, "passengers > 50000")
    ),
    templates = list(
      concept_template_def("utilisation_threshold", "Airport",
                           "utilisation_rate > {{threshold}}")
    )
  )
  result <- validate_bundle(b)
  expect_true(result)
})
