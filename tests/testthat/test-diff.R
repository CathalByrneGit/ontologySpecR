# Shared fixtures -----------------------------------------------------------

make_base <- function() {
  bundle(
    bundle_id = "diff-base", bundle_version = "1.0.0",
    objects = list(
      object_type("Airport",
        list(property_def("airport_id", "string", nullable = FALSE),
             property_def("name",       "string")),
        "airport_id"
      ),
      object_type("Airline",
        list(property_def("airline_id", "string", nullable = FALSE)),
        "airline_id"
      )
    ),
    links = list(
      link_type("RouteOrigin", "FlightRoute", "Airport", cardinality = "many-to-one")
    ),
    actions = list(
      action_type("UpdateStatus", "Airport",
                  effects = list(effect_def("update", "Airport")))
    )
  )
}

# ---------------------------------------------------------------------------
# bundle_diff()
# ---------------------------------------------------------------------------

test_that("bundle_diff on identical bundles is empty", {
  b <- make_base()
  d <- bundle_diff(b, b)
  expect_s3_class(d, "ontology_bundle_diff")
  expect_true(is_empty_diff(d))
})

test_that("bundle_diff detects added object type", {
  base <- make_base()
  new  <- bundle(
    bundle_id = "diff-base", bundle_version = "1.1.0",
    objects = c(base$objects, list(
      object_type("FlightRoute",
        list(property_def("route_id", "string", nullable = FALSE)),
        "route_id"
      )
    )),
    links   = base$links,
    actions = base$actions
  )
  d <- bundle_diff(base, new)
  expect_false(is_empty_diff(d))
  expect_length(d$objects$added,    1L)
  expect_length(d$objects$removed,  0L)
  expect_length(d$objects$modified, 0L)
  expect_equal(d$objects$added[[1]]$id, "FlightRoute")
})

test_that("bundle_diff detects removed link type", {
  base <- make_base()
  new  <- bundle(
    bundle_id = "diff-base", bundle_version = "1.1.0",
    objects = base$objects,
    links   = list()         # RouteOrigin removed
  )
  d <- bundle_diff(base, new)
  expect_length(d$links$removed,  1L)
  expect_length(d$links$added,    0L)
  expect_length(d$links$modified, 0L)
  expect_equal(d$links$removed[[1]]$id, "RouteOrigin")
})

test_that("bundle_diff detects modified action type", {
  base <- make_base()
  # Same id, different field: add display_name
  new_action <- action_type("UpdateStatus", "Airport",
    display_name = "Update Airport Status",
    effects = list(effect_def("update", "Airport"))
  )
  new <- bundle(
    bundle_id = "diff-base", bundle_version = "1.1.0",
    objects = base$objects,
    links   = base$links,
    actions = list(new_action)
  )
  d <- bundle_diff(base, new)
  expect_length(d$actions$modified, 1L)
  expect_length(d$actions$added,    0L)
  expect_length(d$actions$removed,  0L)
  expect_equal(d$actions$modified[[1]]$base$id, "UpdateStatus")
  expect_equal(d$actions$modified[[1]]$new$id,  "UpdateStatus")
  expect_null(d$actions$modified[[1]]$base$display)
  expect_equal(d$actions$modified[[1]]$new$display$name, "Update Airport Status")
})

test_that("bundle_diff detects additions, removals, and modifications together", {
  base <- make_base()
  new  <- bundle(
    bundle_id = "diff-base", bundle_version = "2.0.0",
    # Airport modified (add property), Airline removed, FlightRoute added
    objects = list(
      object_type("Airport",
        list(property_def("airport_id", "string", nullable = FALSE),
             property_def("name",       "string"),
             property_def("country",    "string")),   # new property
        "airport_id"
      ),
      object_type("FlightRoute",
        list(property_def("route_id", "string", nullable = FALSE)),
        "route_id"
      )
    ),
    links   = base$links,
    actions = base$actions
  )
  d <- bundle_diff(base, new)
  expect_length(d$objects$added,    1L)   # FlightRoute
  expect_length(d$objects$removed,  1L)   # Airline
  expect_length(d$objects$modified, 1L)   # Airport
  expect_equal(d$objects$added[[1]]$id,   "FlightRoute")
  expect_equal(d$objects$removed[[1]]$id, "Airline")
  expect_equal(d$objects$modified[[1]]$base$id, "Airport")
})

test_that("bundle_diff rejects non-bundle inputs", {
  b <- make_base()
  expect_error(bundle_diff("not-a-bundle", b), "ontology_bundle")
  expect_error(bundle_diff(b, 42),            "ontology_bundle")
})

# ---------------------------------------------------------------------------
# bundle_apply_diff()
# ---------------------------------------------------------------------------

test_that("bundle_apply_diff with empty diff reproduces base", {
  base <- make_base()
  d    <- bundle_diff(base, base)
  out  <- bundle_apply_diff(base, d)
  expect_s3_class(out, "ontology_bundle")
  expect_equal(length(out$objects), length(base$objects))
  expect_equal(length(out$links),   length(base$links))
})

test_that("bundle_apply_diff applies additions", {
  base <- make_base()
  new  <- bundle(
    bundle_id = "diff-base", bundle_version = "1.1.0",
    objects = c(base$objects, list(
      object_type("FlightRoute",
        list(property_def("route_id", "string", nullable = FALSE)),
        "route_id"
      )
    )),
    links   = base$links,
    actions = base$actions
  )
  out <- bundle_apply_diff(base, bundle_diff(base, new))
  expect_length(out$objects, 3L)
  ids <- vapply(out$objects, function(x) x$id, character(1))
  expect_true("FlightRoute" %in% ids)
})

test_that("bundle_apply_diff applies removals", {
  base <- make_base()
  new  <- bundle(
    bundle_id = "diff-base", bundle_version = "1.1.0",
    objects = list(base$objects[[1]]),  # only Airport, Airline removed
    links   = base$links,
    actions = base$actions
  )
  out <- bundle_apply_diff(base, bundle_diff(base, new))
  expect_length(out$objects, 1L)
  expect_equal(out$objects[[1]]$id, "Airport")
})

test_that("bundle_apply_diff applies modifications", {
  base <- make_base()
  modified_action <- action_type("UpdateStatus", "Airport",
    display_name = "Update Airport Status",
    effects = list(effect_def("update", "Airport"))
  )
  new <- bundle(
    bundle_id = "diff-base", bundle_version = "1.1.0",
    objects = base$objects,
    links   = base$links,
    actions = list(modified_action)
  )
  out <- bundle_apply_diff(base, bundle_diff(base, new))
  expect_length(out$actions, 1L)
  expect_equal(out$actions[[1]]$display$name, "Update Airport Status")
})

test_that("bundle_apply_diff rejects wrong input types", {
  base <- make_base()
  d    <- bundle_diff(base, base)
  expect_error(bundle_apply_diff("x", d),    "ontology_bundle")
  expect_error(bundle_apply_diff(base, list()), "ontology_bundle_diff")
})

# ---------------------------------------------------------------------------
# bundle_merge()
# ---------------------------------------------------------------------------

test_that("bundle_merge round-trip: diff then apply reproduces new", {
  base <- make_base()
  new  <- bundle(
    bundle_id = "diff-base", bundle_version = "2.0.0",
    objects = list(
      object_type("Airport",
        list(property_def("airport_id", "string", nullable = FALSE),
             property_def("name",       "string"),
             property_def("country",    "string")),
        "airport_id"
      ),
      object_type("FlightRoute",
        list(property_def("route_id", "string", nullable = FALSE)),
        "route_id"
      )
    ),
    links   = list(),
    actions = base$actions
  )

  merged <- bundle_merge(base, new)

  # Should contain exactly the objects from new
  merged_ids <- sort(vapply(merged$objects, function(x) x$id, character(1)))
  new_ids    <- sort(vapply(new$objects,    function(x) x$id, character(1)))
  expect_equal(merged_ids, new_ids)

  # Links removed in new should be absent
  expect_length(merged$links, 0L)
})

test_that("bundle_merge on identical bundles returns equivalent bundle", {
  base   <- make_base()
  merged <- bundle_merge(base, base)
  expect_equal(length(merged$objects), length(base$objects))
  expect_equal(length(merged$links),   length(base$links))
})

# ---------------------------------------------------------------------------
# print.ontology_bundle_diff()
# ---------------------------------------------------------------------------

test_that("print.ontology_bundle_diff renders without error", {
  base <- make_base()
  new  <- bundle(
    bundle_id = "diff-base", bundle_version = "1.1.0",
    objects = list(base$objects[[1]]),   # Airline removed
    links   = base$links,
    actions = base$actions,
    queries = list(
      query_def("NearbyAirports", "objectSet",
        returns_object_type_id = "Airport",
        def_kind = "sql", def_body = "SELECT * FROM airports")
    )
  )
  d <- bundle_diff(base, new)
  expect_output(print(d), "BundleDiff")
  expect_output(print(d), "removed")
  expect_output(print(d), "no changes")
})

test_that("is_empty_diff returns FALSE for non-empty diff", {
  base <- make_base()
  new  <- bundle(
    bundle_id = "diff-base", bundle_version = "1.1.0",
    objects = list(base$objects[[1]]),
    links   = base$links
  )
  expect_false(is_empty_diff(bundle_diff(base, new)))
})
