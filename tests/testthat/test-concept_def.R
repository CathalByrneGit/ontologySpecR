test_that("concept_def creates valid object", {
  cd <- concept_def(
    id = "busy_airport",
    object_type_id = "Airport",
    scope = "operations",
    version = 1L,
    sql_expr = "daily_passengers > 50000"
  )
  expect_s3_class(cd, "ontology_concept_def")
  expect_true(is_concept_def(cd))
  expect_equal(cd$id, "busy_airport")
  expect_equal(cd$objectTypeId, "Airport")
  expect_equal(cd$scope, "operations")
  expect_equal(cd$version, 1L)
  expect_equal(cd$sqlExpr, "daily_passengers > 50000")
  expect_equal(cd$status, "draft")
})

test_that("concept_def accepts all optional fields", {
  cd <- concept_def(
    id = "ready_for_discharge",
    object_type_id = "Patient",
    scope = "clinical",
    version = 2L,
    sql_expr = "los_days < 5",
    status = "active",
    rationale = "Based on WHO guideline",
    source_standard = "WHO",
    template_id = "length_of_stay_template",
    parameter_values = list(threshold = 5),
    display_name = "Ready for Discharge",
    display_description = "Patient is ready for discharge."
  )
  expect_equal(cd$status, "active")
  expect_equal(cd$rationale, "Based on WHO guideline")
  expect_equal(cd$sourceStandard, "WHO")
  expect_equal(cd$templateId, "length_of_stay_template")
  expect_equal(cd$parameterValues$threshold, 5)
  expect_equal(cd$display$name, "Ready for Discharge")
})

test_that("concept_def validates id", {
  expect_error(
    concept_def("", "Airport", "ops", 1L, "x > 0"),
    "`id` must be a non-empty character string"
  )
  expect_error(
    concept_def(123, "Airport", "ops", 1L, "x > 0"),
    "`id` must be a non-empty character string"
  )
})

test_that("concept_def validates object_type_id", {
  expect_error(
    concept_def("ccc", "", "ops", 1L, "x > 0"),
    "`object_type_id`"
  )
})

test_that("concept_def validates version", {
  expect_error(
    concept_def("busy_airport", "Airport", "ops", 0L, "x > 0"),
    "`version` must be a positive integer"
  )
  expect_error(
    concept_def("busy_airport", "Airport", "ops", -1L, "x > 0"),
    "`version` must be a positive integer"
  )
  expect_error(
    concept_def("busy_airport", "Airport", "ops", 1.5, "x > 0"),
    "`version` must be a positive integer"
  )
})

test_that("concept_def validates status enum", {
  expect_error(
    concept_def("busy_airport", "Airport", "ops", 1L, "x > 0", status = "unknown"),
    "`status` must be one of"
  )
})

test_that("concept_def validates sql_expr", {
  expect_error(
    concept_def("busy_airport", "Airport", "ops", 1L, ""),
    "`sql_expr` must be a non-empty character string"
  )
})

test_that("print.ontology_concept_def runs without error", {
  cd <- concept_def("busy_airport", "Airport", "ops", 1L, "passengers > 1000")
  expect_output(print(cd), "ConceptDef")
  expect_output(print(cd), "busy_airport")
})

test_that("as_list.ontology_concept_def serializes correctly", {
  cd <- concept_def(
    id = "busy_airport",
    object_type_id = "Airport",
    scope = "operations",
    version = 1L,
    sql_expr = "daily_passengers > 50000",
    status = "active"
  )
  lst <- as_list(cd)
  expect_equal(lst$id, "busy_airport")
  expect_equal(lst$objectTypeId, "Airport")
  expect_equal(lst$sqlExpr, "daily_passengers > 50000")
  expect_equal(lst$status, "active")
  expect_null(lst$rationale)
})

# ---------------------------------------------------------------------------
# concept_template_def
# ---------------------------------------------------------------------------

test_that("concept_template_def creates valid object", {
  tmpl <- concept_template_def(
    id = "utilisation_threshold",
    object_type_id = "Airport",
    base_sql_expr = "utilisation_rate > {{threshold}}",
    parameters = list(
      parameter_def("threshold", "number", required = FALSE)
    )
  )
  expect_s3_class(tmpl, "ontology_concept_template_def")
  expect_true(is_concept_template_def(tmpl))
  expect_equal(tmpl$id, "utilisation_threshold")
  expect_equal(tmpl$objectTypeId, "Airport")
  expect_equal(tmpl$baseSqlExpr, "utilisation_rate > {{threshold}}")
  expect_length(tmpl$parameters, 1L)
})

test_that("concept_template_def accepts optional fields", {
  tmpl <- concept_template_def(
    id = "utilisation_threshold",
    object_type_id = "Airport",
    base_sql_expr = "utilisation_rate > {{threshold}}",
    source_standard = "ILO",
    display_name = "Utilisation Threshold"
  )
  expect_equal(tmpl$sourceStandard, "ILO")
  expect_equal(tmpl$display$name, "Utilisation Threshold")
})

test_that("concept_template_def validates id", {
  expect_error(
    concept_template_def("", "Airport", "rate > {{x}}"),
    "`id`"
  )
})

test_that("concept_template_def validates parameters must be list", {
  expect_error(
    concept_template_def("tmpl", "Airport", "rate > {{x}}", parameters = "bad"),
    "`parameters` must be a list"
  )
})

test_that("print.ontology_concept_template_def runs without error", {
  tmpl <- concept_template_def("utilisation_threshold", "Airport",
                               "utilisation_rate > {{threshold}}")
  expect_output(print(tmpl), "ConceptTemplateDef")
  expect_output(print(tmpl), "utilisation_threshold")
})

test_that("as_list.ontology_concept_template_def serializes parameters", {
  tmpl <- concept_template_def(
    id = "utilisation_threshold",
    object_type_id = "Airport",
    base_sql_expr = "utilisation_rate > {{threshold}}",
    parameters = list(parameter_def("threshold", "number"))
  )
  lst <- as_list(tmpl)
  expect_equal(lst$id, "utilisation_threshold")
  expect_equal(lst$baseSqlExpr, "utilisation_rate > {{threshold}}")
  expect_length(lst$parameters, 1L)
  expect_equal(lst$parameters[[1]]$id, "threshold")
})

# ---------------------------------------------------------------------------
# Bundle roundtrip with concepts and templates
# ---------------------------------------------------------------------------

test_that("bundle carries concept_def and concept_template_def through JSON roundtrip", {
  cd <- concept_def(
    id = "busy_airport",
    object_type_id = "Airport",
    scope = "operations",
    version = 1L,
    sql_expr = "daily_passengers > 50000"
  )
  tmpl <- concept_template_def(
    id = "utilisation_threshold",
    object_type_id = "Airport",
    base_sql_expr = "utilisation_rate > {{threshold}}",
    parameters = list(parameter_def("threshold", "number"))
  )

  b <- bundle(
    bundle_id = "concept-roundtrip",
    bundle_version = "1.0.0",
    objects = list(
      object_type("Airport",
        list(property_def("airport_id", "string", nullable = FALSE)),
        "airport_id"
      )
    ),
    concepts = list(cd),
    templates = list(tmpl)
  )

  json <- bundle_to_json(b)
  b2 <- bundle_from_json(json)

  expect_s3_class(b2, "ontology_bundle")
  expect_length(b2$concepts, 1L)
  expect_length(b2$templates, 1L)
  expect_s3_class(b2$concepts[[1]], "ontology_concept_def")
  expect_s3_class(b2$templates[[1]], "ontology_concept_template_def")
  expect_equal(b2$concepts[[1]]$id, "busy_airport")
  expect_equal(b2$concepts[[1]]$scope, "operations")
  expect_equal(b2$templates[[1]]$id, "utilisation_threshold")
  expect_equal(b2$templates[[1]]$baseSqlExpr, "utilisation_rate > {{threshold}}")
})
