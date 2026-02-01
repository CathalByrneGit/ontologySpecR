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
