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
