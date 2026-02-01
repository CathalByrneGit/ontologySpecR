test_that("primary_key_def creates valid object", {
  pk <- primary_key_def("id_col")
  expect_s3_class(pk, "ontology_primary_key_def")
  expect_true(is_primary_key_def(pk))
  expect_equal(pk$properties, "id_col")
  expect_equal(pk$strategy, "natural")
})

test_that("primary_key_def supports composite keys", {
  pk <- primary_key_def(c("region", "code"), strategy = "natural")
  expect_length(pk$properties, 2)
})

test_that("primary_key_def rejects empty properties", {
  expect_error(primary_key_def(character(0)), "non-empty")
})

test_that("primary_key_def rejects invalid strategy", {
  expect_error(primary_key_def("id", strategy = "auto"), "must be one of")
})

test_that("as_list.ontology_primary_key_def works", {
  pk <- primary_key_def(c("a", "b"))
  l <- as_list(pk)
  expect_type(l, "list")
  expect_equal(l$properties, list("a", "b"))
})
