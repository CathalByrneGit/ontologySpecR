# Test accessor layer

# --- as_bundle ---
test_that("as_bundle returns bundle unchanged", {
  b <- bundle("test", "1.0.0")
  expect_identical(as_bundle(b), b)
})

test_that("as_bundle reads from file path", {
  path <- system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  skip_if(path == "", message = "Example file not found (not installed)")
  b <- as_bundle(path)
  expect_true(is_bundle(b))
  expect_equal(b$bundleId, "aviation-demo")
})

test_that("as_bundle maps legacy objectTypes to objects", {
  raw <- list(
    bundleId = "legacy-test",
    bundleVersion = "1.0.0",
    specVersion = "0.1.0",
    objectTypes = list(
      list(
        id = "Thing",
        properties = list(list(id = "thing_id", type = "string")),
        primaryKey = list(properties = list("thing_id"))
      )
    ),
    links = list(),
    interfaces = list(),
    actions = list(),
    queries = list()
  )
  expect_warning(b <- as_bundle(raw), "objectTypes.*objects")
  expect_true(is_bundle(b))
  expect_length(b$objects, 1)
})

test_that("as_bundle maps legacy linkTypes to links", {
  raw <- list(
    bundleId = "legacy-links",
    bundleVersion = "1.0.0",
    specVersion = "0.1.0",
    objects = list(),
    linkTypes = list(
      list(id = "FooBar", from = "Foo", to = "Bar")
    ),
    interfaces = list(),
    actions = list(),
    queries = list()
  )
  expect_warning(b <- as_bundle(raw), "linkTypes.*links")
  expect_length(b$links, 1)
})

test_that("as_bundle maps legacy table_name to source$table", {
  raw <- list(
    bundleId = "legacy-table",
    bundleVersion = "1.0.0",
    specVersion = "0.1.0",
    objects = list(
      list(
        id = "Widget",
        table_name = "widgets",
        properties = list(list(id = "widget_id", type = "string")),
        primaryKey = list(properties = list("widget_id"))
      )
    ),
    links = list(),
    interfaces = list(),
    actions = list(),
    queries = list()
  )
  expect_warning(b <- as_bundle(raw), "table_name.*source")
  ot <- bundle_object_type(b, "Widget")
  expect_equal(object_table(ot), "widgets")
})

test_that("as_bundle maps legacy primary_key to primaryKey$properties", {
  raw <- list(
    bundleId = "legacy-pk",
    bundleVersion = "1.0.0",
    specVersion = "0.1.0",
    objects = list(
      list(
        id = "Item",
        primary_key = "item_id",
        properties = list(list(id = "item_id", type = "string"))
      )
    ),
    links = list(),
    interfaces = list(),
    actions = list(),
    queries = list()
  )
  expect_warning(b <- as_bundle(raw), "primary_key.*primaryKey")
  ot <- bundle_object_type(b, "Item")
  expect_equal(object_pk(ot), "item_id")
})

test_that("as_bundle maps legacy action_id to id", {
  raw <- list(
    bundleId = "legacy-action",
    bundleVersion = "1.0.0",
    specVersion = "0.1.0",
    objects = list(
      list(
        id = "Foo",
        properties = list(list(id = "foo_id", type = "string")),
        primaryKey = list(properties = list("foo_id"))
      )
    ),
    links = list(),
    interfaces = list(),
    actions = list(
      list(action_id = "DoSomething", targets = list("Foo"), parameters = list())
    ),
    queries = list()
  )
  expect_warning(b <- as_bundle(raw), "action_id.*id")
  act <- bundle_action_type(b, "DoSomething")
  expect_equal(act$id, "DoSomething")
})

test_that("as_bundle errors on invalid input", {
  expect_error(as_bundle(42), "must be an ontology_bundle")
})

# --- Bundle metadata accessors ---
test_that("bundle_id returns bundleId", {
  b <- bundle("my-bundle", "1.2.3")
  expect_equal(bundle_id(b), "my-bundle")
})

test_that("bundle_version returns bundleVersion", {
  b <- bundle("my-bundle", "1.2.3")
  expect_equal(bundle_version(b), "1.2.3")
})

test_that("bundle_spec_version returns specVersion", {
  b <- bundle("my-bundle", "1.0.0", spec_version = "0.2.0")
  expect_equal(bundle_spec_version(b), "0.2.0")
})

# --- Collection accessors ---
test_that("bundle_object_types returns named list", {
  b <- bundle("test", "1.0.0",
    objects = list(
      object_type("Alpha", list(property_def("id", "string")), "id"),
      object_type("Beta", list(property_def("id", "string")), "id")
    )
  )
  types <- bundle_object_types(b)
  expect_type(types, "list")
  expect_named(types, c("Alpha", "Beta"))
})

test_that("bundle_link_types returns named list", {
  b <- bundle("test", "1.0.0",
    links = list(
      link_type("FooBar", "Foo", "Bar"),
      link_type("BarBaz", "Bar", "Baz")
    )
  )
  types <- bundle_link_types(b)
  expect_named(types, c("FooBar", "BarBaz"))
})

test_that("bundle_action_types returns named list", {
  b <- bundle("test", "1.0.0",
    objects = list(
      object_type("Target", list(property_def("id", "string")), "id")
    ),
    actions = list(
      action_type("DoIt", "Target", list())
    )
  )
  types <- bundle_action_types(b)
  expect_named(types, "DoIt")
})

test_that("bundle_queries returns named list", {
  b <- bundle("test", "1.0.0",
    queries = list(
      query_def("MyQuery", "objectSet", "SomeType")
    )
  )
  queries <- bundle_queries(b)
  expect_named(queries, "MyQuery")
})

test_that("bundle_concepts returns named list", {
  b <- bundle("test", "1.0.0",
    concepts = list(
      concept_def("my_concept", "SomeType", "scope1", 1L, "x > 0")
    )
  )
  concepts <- bundle_concepts(b)
  expect_named(concepts, "my_concept")
})

test_that("bundle_templates returns named list", {
  b <- bundle("test", "1.0.0",
    templates = list(
      concept_template_def("my_tpl", "SomeType", "x > {{val}}")
    )
  )
  templates <- bundle_templates(b)
  expect_named(templates, "my_tpl")
})

# --- Single entity accessors ---
test_that("bundle_object_type returns correct type", {
  b <- bundle("test", "1.0.0",
    objects = list(
      object_type("Alpha", list(property_def("id", "string")), "id")
    )
  )
  ot <- bundle_object_type(b, "Alpha")
  expect_equal(ot$id, "Alpha")
})

test_that("bundle_object_type errors on unknown id", {
  b <- bundle("test", "1.0.0",
    objects = list(
      object_type("Alpha", list(property_def("id", "string")), "id")
    )
  )
  expect_error(bundle_object_type(b, "Beta"), "Unknown object type.*Alpha")
})

test_that("bundle_link_type returns correct type", {
  b <- bundle("test", "1.0.0",
    links = list(link_type("FooBar", "Foo", "Bar"))
  )
  lt <- bundle_link_type(b, "FooBar")
  expect_equal(lt$id, "FooBar")
})

test_that("bundle_link_type errors on unknown id", {
  b <- bundle("test", "1.0.0")
  expect_error(bundle_link_type(b, "Missing"), "Unknown link type.*no link types")
})

test_that("bundle_action_type returns correct type", {
  b <- bundle("test", "1.0.0",
    actions = list(action_type("DoIt", "Target", list()))
  )
  at <- bundle_action_type(b, "DoIt")
  expect_equal(at$id, "DoIt")
})

test_that("bundle_action_type errors on unknown id", {
  b <- bundle("test", "1.0.0",
    actions = list(action_type("DoIt", "Target", list()))
  )
  expect_error(bundle_action_type(b, "DoOther"), "Unknown action type.*DoIt")
})

# --- Object type accessors ---
test_that("object_table returns source$table", {
  ot <- object_type("Widget",
    list(property_def("id", "string")),
    "id",
    source_table = "widgets"
  )
  expect_equal(object_table(ot), "widgets")
})

test_that("object_table falls back to id", {
  ot <- object_type("Widget",
    list(property_def("id", "string")),
    "id"
  )
  expect_equal(object_table(ot), "Widget")
})

test_that("object_table errors for query-backed type", {
  ot <- object_type("Computed",
    list(property_def("id", "string")),
    "id",
    source_kind = "query",
    source_sql = "SELECT * FROM raw"
  )
  expect_error(object_table(ot), "query-backed.*object_source_sql")
})

test_that("object_source_sql returns sql", {
  ot <- object_type("Computed",
    list(property_def("id", "string")),
    "id",
    source_kind = "query",
    source_sql = "SELECT * FROM raw"
  )
  expect_equal(object_source_sql(ot), "SELECT * FROM raw")
})

test_that("object_source returns full source", {
  ot <- object_type("Widget",
    list(property_def("id", "string")),
    "id",
    source_kind = "table",
    source_table = "widgets",
    source_schema = "public"
  )
  src <- object_source(ot)
  expect_equal(src$kind, "table")
  expect_equal(src$table, "widgets")
  expect_equal(src$schema, "public")
})

test_that("object_pk returns properties", {
  ot <- object_type("Widget",
    list(
      property_def("part_a", "string"),
      property_def("part_b", "string")
    ),
    primary_key_def(c("part_a", "part_b"))
  )
  expect_equal(object_pk(ot), c("part_a", "part_b"))
})

test_that("object_pk works with simple string pk", {
  ot <- object_type("Simple",
    list(property_def("id", "string")),
    "id"
  )
  pk <- object_pk(ot)
  expect_true("id" %in% pk)
})

test_that("object_property_ids returns property ids", {
  ot <- object_type("Widget",
    list(
      property_def("id", "string"),
      property_def("name", "string"),
      property_def("count", "integer")
    ),
    "id"
  )
  expect_equal(object_property_ids(ot), c("id", "name", "count"))
})

# --- Link type accessors ---
test_that("link_endpoints returns from and to", {
  lt <- link_type("FooBar", "Foo", "Bar")
  eps <- link_endpoints(lt)
  expect_equal(eps$from, "Foo")
  expect_equal(eps$to, "Bar")
})

# --- spec_example_bundle ---
test_that("spec_example_bundle loads aviation-demo", {
  skip_if(system.file("examples", "aviation-demo.json", package = "ontologySpecR") == "",
          message = "Example file not found (not installed)")
  b <- spec_example_bundle()
  expect_true(is_bundle(b))
  expect_equal(bundle_id(b), "aviation-demo")
})

# --- property accessors ---

make_prop_ot <- function() {
  object_type(
    id = "Thing",
    properties = list(
      property_def("thing_id", "string", nullable = FALSE),
      property_def("label", "string", source_column = "label_col"),
      property_def("shout", "string", source_expression = "upper(label_col)")
    ),
    primary_key = "thing_id"
  )
}

test_that("property_column returns source column, falling back to the id", {
  ot <- make_prop_ot()
  expect_equal(property_column(ot$properties[[2]]), "label_col")  # column-backed
  expect_equal(property_column(ot$properties[[1]]), "thing_id")   # bare property
  expect_equal(property_column(ot$properties[[3]]), "shout")      # expression only
})

test_that("property_expression returns the expression or NULL", {
  ot <- make_prop_ot()
  expect_equal(property_expression(ot$properties[[3]]), "upper(label_col)")
  expect_null(property_expression(ot$properties[[2]]))
  expect_null(property_expression(ot$properties[[1]]))
})

test_that("object_properties names properties by id", {
  ot <- make_prop_ot()
  props <- object_properties(ot)
  expect_equal(names(props), c("thing_id", "label", "shout"))
  expect_equal(props$label$type, "string")
  expect_equal(object_properties(object_type("E", list(), primary_key = "x")), list())
})

test_that("object_property returns one property and lists valid ids on error", {
  ot <- make_prop_ot()
  expect_equal(object_property(ot, "label")$id, "label")
  expect_error(object_property(ot, "nope"),
               "Unknown property 'nope' on object type 'Thing'. Valid ids: thing_id, label, shout",
               fixed = TRUE)
  empty <- object_type("E", list(), primary_key = "x")
  expect_error(object_property(empty, "x"), "Valid ids: (none)", fixed = TRUE)
})

test_that("as_bundle maps legacy property source$name to source$column", {
  warned <- .legacy_warned
  rm(list = ls(warned, all.names = TRUE), envir = warned)
  raw <- list(
    bundleId = "legacy-prop", bundleVersion = "1.0.0",
    objects = list(list(
      id = "Thing",
      properties = list(
        list(id = "thing_id", type = "string"),
        list(id = "label", type = "string", source = list(name = "label_col"))
      ),
      primaryKey = list(properties = list("thing_id"))
    ))
  )
  expect_warning(b <- as_bundle(raw), "source$name", fixed = TRUE)
  prop <- object_property(bundle_object_type(b, "Thing"), "label")
  expect_equal(property_column(prop), "label_col")
  expect_null(prop$source$name)
})

test_that("as_bundle prefers an explicit source$column over legacy name", {
  raw <- list(
    bundleId = "legacy-prop2", bundleVersion = "1.0.0",
    objects = list(list(
      id = "Thing",
      properties = list(
        list(id = "label", type = "string",
             source = list(name = "old", column = "new"))
      ),
      primaryKey = list(properties = list("label"))
    ))
  )
  b <- as_bundle(raw)
  expect_equal(property_column(object_property(bundle_object_type(b, "Thing"), "label")), "new")
})

test_that("as_bundle maps legacy join from_keys/to_keys", {
  warned <- .legacy_warned
  rm(list = ls(warned, all.names = TRUE), envir = warned)
  raw <- list(
    bundleId = "legacy-join", bundleVersion = "1.0.0",
    links = list(list(id = "L", from = "A", to = "B",
                      join = list(from_keys = list("a_id"), to_keys = list("b_id"))))
  )
  w <- character()
  b <- withCallingHandlers(
    as_bundle(raw),
    warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") }
  )
  expect_true(any(grepl("join$from_keys", w, fixed = TRUE)))
  expect_true(any(grepl("join$to_keys", w, fixed = TRUE)))
  expect_equal(link_join_keys(bundle_link_type(b, "L")),
               list(from = "a_id", to = "b_id"))
})

# --- link_join_keys ---

test_that("link_join_keys handles single and composite keys", {
  single <- link_type("L", "A", "B", join_from_keys = "a_id", join_to_keys = "b_id")
  expect_equal(link_join_keys(single), list(from = "a_id", to = "b_id"))

  composite <- link_type("C", "A", "B",
                         join_from_keys = c("a1", "a2"), join_to_keys = c("b1", "b2"))
  expect_equal(link_join_keys(composite),
               list(from = c("a1", "a2"), to = c("b1", "b2")))
})

test_that("link_join_keys errors on missing join or mismatched lengths", {
  expect_error(link_join_keys(link_type("NoJoin", "A", "B")),
               "Link type 'NoJoin' is missing join keys", fixed = TRUE)
  one_sided <- link_type("OneSided", "A", "B", join_from_keys = "a")
  expect_error(link_join_keys(one_sided), "missing join keys", fixed = TRUE)
  mismatch <- link_type("Mismatch", "A", "B",
                        join_from_keys = c("a1", "a2"), join_to_keys = "b1")
  expect_error(link_join_keys(mismatch), "2 fromKeys but 1 toKeys", fixed = TRUE)
})

# --- schema-qualified tables ---
test_that("object_schema is NULL when the type has no schema", {
  ot <- object_type("Widget", list(property_def("id", "string")), "id",
                    source_table = "widgets")
  expect_null(object_schema(ot))
  expect_identical(object_table_id(ot), list(schema = NULL, table = "widgets"))
})

test_that("object_schema and object_table_id return the schema", {
  ot <- object_type("Patient", list(property_def("id", "string")), "id",
                    source_table = "patients", source_schema = "analytics")
  expect_identical(object_schema(ot), "analytics")
  expect_identical(object_table_id(ot),
                   list(schema = "analytics", table = "patients"))
})

test_that("an empty-string schema is treated as none", {
  ot <- object_type("Widget", list(property_def("id", "string")), "id",
                    source_table = "widgets", source_schema = "")
  expect_null(object_schema(ot))
  expect_null(object_table_id(ot)$schema)
})

test_that("object_table_id aborts for query-backed types", {
  ot <- object_type("Computed", list(property_def("id", "string")), "id",
                    source_kind = "query", source_sql = "SELECT 1 AS id",
                    source_schema = "analytics")
  expect_error(object_table_id(ot), "query-backed")
})

test_that("object_table_id falls back to the id when there is no table", {
  ot <- object_type("Widget", list(property_def("id", "string")), "id")
  expect_identical(object_table_id(ot), list(schema = NULL, table = "Widget"))
})

test_that("schema survives a bundle round trip", {
  b <- bundle("schema-rt", "1.0.0", objects = list(
    object_type("Patient", list(property_def("id", "string")), "id",
                source_table = "patients", source_schema = "analytics")
  ))
  b2 <- bundle_from_json(bundle_to_json(b))
  expect_identical(object_table_id(bundle_object_type(b2, "Patient")),
                   list(schema = "analytics", table = "patients"))
})
