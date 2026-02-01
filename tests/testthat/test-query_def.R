test_that("query_def creates valid object", {
  qd <- query_def(
    "NearbyAirports",
    returns_kind = "objectSet",
    returns_object_type_id = "Airport",
    parameters = list(
      parameter_def("lat", "number", required = TRUE),
      parameter_def("lon", "number", required = TRUE)
    ),
    def_kind = "sql",
    def_body = "SELECT * FROM airports WHERE ..."
  )
  expect_s3_class(qd, "ontology_query_def")
  expect_true(is_query_def(qd))
  expect_equal(qd$id, "NearbyAirports")
  expect_equal(qd$returns$kind, "objectSet")
  expect_equal(qd$returns$objectTypeId, "Airport")
  expect_length(qd$parameters, 2)
  expect_equal(qd$definition$kind, "sql")
})

test_that("query_def rejects invalid returns_kind", {
  expect_error(
    query_def("Bad", returns_kind = "dataframe"),
    "must be one of"
  )
})

test_that("query_def with no parameters or definition", {
  qd <- query_def("AllItems", returns_kind = "table")
  expect_s3_class(qd, "ontology_query_def")
  expect_null(qd$parameters)
  expect_null(qd$definition)
})

test_that("as_list.ontology_query_def works", {
  qd <- query_def("Q1", "scalar", def_kind = "r", def_body = "nrow(df)")
  l <- as_list(qd)
  expect_equal(l$id, "Q1")
  expect_equal(l$returns$kind, "scalar")
  expect_equal(l$definition$kind, "r")
})

test_that("print.ontology_query_def runs without error", {
  qd <- query_def("Q1", "objectSet", returns_object_type_id = "Airport")
  expect_output(print(qd), "objectSet<Airport>")
})
