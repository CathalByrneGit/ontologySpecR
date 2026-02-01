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

test_that("validate_bundle returns errors for invalid JSON", {
  bad_json <- '{"bundleId": "no-version"}'
  result <- validate_bundle(bad_json)
  expect_false(isTRUE(result))
  expect_type(result, "character")
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
