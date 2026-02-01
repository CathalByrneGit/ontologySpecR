test_that("action_type creates valid object", {
  at <- action_type(
    "UpdateStatus",
    targets = "Airport",
    parameters = list(
      parameter_def("new_status", "string", required = TRUE)
    ),
    effects = list(effect_def("update", "Airport")),
    impl_kind = "r",
    impl_entrypoint = "update_status"
  )
  expect_s3_class(at, "ontology_action_type")
  expect_true(is_action_type(at))
  expect_equal(at$id, "UpdateStatus")
  expect_equal(at$targets, list("Airport"))
  expect_length(at$parameters, 1)
  expect_length(at$effects, 1)
  expect_equal(at$implementation$kind, "r")
})

test_that("parameter_def creates valid object", {
  pd <- parameter_def("threshold", "number", required = TRUE,
                      display_name = "Threshold")
  expect_s3_class(pd, "ontology_parameter_def")
  expect_true(is_parameter_def(pd))
  expect_true(pd$required)
})

test_that("effect_def creates valid object", {
  ed <- effect_def("create", "Order", notes = "Creates a new order.")
  expect_s3_class(ed, "ontology_effect_def")
  expect_true(is_effect_def(ed))
  expect_equal(ed$kind, "create")
  expect_equal(ed$objectTypeId, "Order")
})

test_that("action_type rejects empty targets", {
  expect_error(action_type("Bad", targets = character(0)), "non-empty")
})

test_that("as_list.ontology_action_type works", {
  at <- action_type("DoSomething", targets = "MyObj")
  l <- as_list(at)
  expect_equal(l$id, "DoSomething")
  expect_equal(l$targets, list("MyObj"))
})
