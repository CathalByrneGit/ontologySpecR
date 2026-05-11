test_that("interface_type creates valid object", {
  iface <- interface_type(
    "GeoLocated",
    display_name = "Geo-Located",
    required_properties = list(
      property_requirement("latitude", "number"),
      property_requirement("longitude", "number")
    )
  )
  expect_s3_class(iface, "ontology_interface_type")
  expect_true(is_interface_type(iface))
  expect_equal(iface$id, "GeoLocated")
  expect_length(iface$requiredProperties, 2)
})

test_that("property_requirement creates valid object", {
  pr <- property_requirement("lat", "number", nullable = FALSE)
  expect_s3_class(pr, "ontology_property_requirement")
  expect_true(is_property_requirement(pr))
  expect_equal(pr$id, "lat")
  expect_false(pr$nullable)
})

test_that("link_requirement creates valid object", {
  lr <- link_requirement("HasParent", min_count = 1L, max_count = 2L)
  expect_s3_class(lr, "ontology_link_requirement")
  expect_true(is_link_requirement(lr))
  expect_equal(lr$linkTypeId, "HasParent")
  expect_equal(lr$minCount, 1L)
  expect_equal(lr$maxCount, 2L)
})

test_that("interface_type with required actions", {
  iface <- interface_type("Auditable", required_actions = c("AuditLog", "Review"))
  expect_equal(iface$requiredActions, list("AuditLog", "Review"))
})

test_that("as_list.ontology_interface_type works", {
  iface <- interface_type(
    "Named",
    required_properties = list(property_requirement("name", "string"))
  )
  l <- as_list(iface)
  expect_equal(l$id, "Named")
  expect_length(l$requiredProperties, 1)
  expect_equal(l$requiredProperties[[1]]$id, "name")
})

# ---------------------------------------------------------------------------
# check_implements()
# ---------------------------------------------------------------------------

make_airport <- function(...) {
  object_type(
    "Airport",
    properties = list(
      property_def("airport_id", "string", nullable = FALSE),
      property_def("name",       "string"),
      property_def("latitude",   "number"),
      property_def("longitude",  "number")
    ),
    primary_key = "airport_id",
    ...
  )
}

make_geo_iface <- function() {
  interface_type(
    "GeoLocated",
    required_properties = list(
      property_requirement("latitude",  "number"),
      property_requirement("longitude", "number")
    )
  )
}

test_that("check_implements returns TRUE for compliant object type", {
  result <- check_implements(make_airport(), make_geo_iface())
  expect_true(result)
})

test_that("check_implements error=FALSE returns empty vector when compliant", {
  result <- check_implements(make_airport(), make_geo_iface(), error = FALSE)
  expect_type(result, "logical")
  expect_true(result)
})

test_that("check_implements detects missing required property", {
  ot <- object_type(
    "Sparse",
    properties = list(property_def("sparse_id", "string")),
    primary_key = "sparse_id"
  )
  result <- check_implements(ot, make_geo_iface(), error = FALSE)
  expect_type(result, "character")
  expect_true(any(grepl("missing required property 'latitude'", result)))
  expect_true(any(grepl("missing required property 'longitude'", result)))
})

test_that("check_implements detects property type mismatch", {
  ot <- object_type(
    "Bad",
    properties = list(
      property_def("bad_id",   "string"),
      property_def("latitude",  "string"),   # wrong type
      property_def("longitude", "number")
    ),
    primary_key = "bad_id"
  )
  result <- check_implements(ot, make_geo_iface(), error = FALSE)
  expect_true(any(grepl("type 'string' but interface requires 'number'", result)))
})

test_that("check_implements detects nullable mismatch", {
  strict_iface <- interface_type(
    "StrictGeo",
    required_properties = list(
      property_requirement("latitude", "number", nullable = FALSE)
    )
  )
  ot <- object_type(
    "Loose",
    properties = list(
      property_def("loose_id",  "string"),
      property_def("latitude",  "number", nullable = TRUE)   # nullable, but iface says must not be
    ),
    primary_key = "loose_id"
  )
  result <- check_implements(ot, strict_iface, error = FALSE)
  expect_true(any(grepl("nullable", result)))
})

test_that("check_implements aborts with error=TRUE on violation", {
  ot <- object_type(
    "Empty",
    properties = list(property_def("empty_id", "string")),
    primary_key = "empty_id"
  )
  expect_error(check_implements(ot, make_geo_iface(), error = TRUE))
})

test_that("check_implements checks required links when bundle provided", {
  iface <- interface_type(
    "Connectable",
    required_links = list(link_requirement("ConnectsTo"))
  )
  ot <- make_airport()

  # No bundle: link check is skipped → passes
  expect_true(check_implements(ot, iface, bundle = NULL))

  # Bundle with the required link: passes
  b_ok <- bundle(
    "test", "1.0.0",
    objects = list(ot),
    links   = list(link_type("ConnectsTo", "Airport", "Airport")),
    interfaces = list(iface)
  )
  expect_true(check_implements(ot, iface, bundle = b_ok))

  # Bundle missing the link: violation
  b_bad <- bundle("test", "1.0.0", objects = list(ot), interfaces = list(iface))
  result <- check_implements(ot, iface, bundle = b_bad, error = FALSE)
  expect_true(any(grepl("missing required link type 'ConnectsTo'", result)))
})

test_that("check_implements checks required actions when bundle provided", {
  iface <- interface_type("Operable", required_actions = "Activate")
  ot    <- make_airport()

  # No bundle → skipped
  expect_true(check_implements(ot, iface, bundle = NULL))

  # Bundle with the action: passes
  b_ok <- bundle(
    "test", "1.0.0",
    objects    = list(ot),
    interfaces = list(iface),
    actions    = list(action_type("Activate", "Airport"))
  )
  expect_true(check_implements(ot, iface, bundle = b_ok))

  # Bundle without the action: violation
  b_bad <- bundle("test", "1.0.0", objects = list(ot), interfaces = list(iface))
  result <- check_implements(ot, iface, bundle = b_bad, error = FALSE)
  expect_true(any(grepl("missing required action type 'Activate'", result)))
})

test_that("check_implements rejects non-object_type input", {
  expect_error(check_implements("not-an-ot", make_geo_iface()), "ontology_object_type")
})

test_that("check_implements rejects non-interface_type input", {
  expect_error(check_implements(make_airport(), "not-an-iface"), "ontology_interface_type")
})
