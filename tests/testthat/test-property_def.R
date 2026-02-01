test_that("property_def creates valid object", {
  p <- property_def("airport_code", "string", nullable = FALSE,
                    display_name = "Airport Code")
  expect_s3_class(p, "ontology_property_def")
  expect_true(is_property_def(p))
  expect_equal(p$id, "airport_code")
  expect_equal(p$type, "string")
  expect_false(p$nullable)
  expect_equal(p$display$name, "Airport Code")
})

test_that("property_def rejects invalid type", {
  expect_error(property_def("x", "badtype"), "must be one of")
})

test_that("property_def rejects empty id", {
  expect_error(property_def("", "string"), "non-empty")
})

test_that("property_def with source mapping", {
  p <- property_def("full_name", "string",
                    source_column = "first_name",
                    source_expression = "CONCAT(first_name, ' ', last_name)")
  expect_equal(p$source$column, "first_name")
  expect_true(!is.null(p$source$expression))
})

test_that("as_list.ontology_property_def works", {
  p <- property_def("test_prop", "integer", nullable = TRUE)
  l <- as_list(p)
  expect_type(l, "list")
  expect_equal(l$id, "test_prop")
  expect_equal(l$type, "integer")
})

test_that("print.ontology_property_def runs without error", {
  p <- property_def("test_prop", "string")
  expect_output(print(p), "PropertyDef")
})
