# ontologySpecR 0.2.1

## New features

* Property and link accessors, so downstream packages stop reading these
  fields by hand:
  - `property_column(p)` — `source$column`, falling back to the property id
  - `property_expression(p)` — `source$expression` or `NULL`
  - `object_properties(ot)` — properties as a list named by id
  - `object_property(ot, id)` — one property; unknown ids abort and list the
    valid ones
  - `link_join_keys(lt)` — `list(from =, to =)` character vectors from
    `join$fromKeys` / `join$toKeys`; aborts if either side is missing or the
    lengths differ
* `as_bundle()` now also maps legacy property `source$name` -> `source$column`
  and legacy link `join$from_keys` / `join$to_keys` -> `join$fromKeys` /
  `join$toKeys` (with the usual one-time warning).

# ontologySpecR 0.2.0

## New features

* Added accessor layer for reading bundle contents (`R/accessors.R`). All
  downstream packages should use these accessors rather than accessing `$`
  fields directly:

  - `as_bundle()` — coerces file paths, raw lists, or existing bundles to
    `ontology_bundle` objects; handles legacy key mappings with warnings
  - `bundle_id()`, `bundle_version()`, `bundle_spec_version()` — bundle metadata
  - `bundle_object_types()`, `bundle_link_types()`, `bundle_interface_types()`,
    `bundle_action_types()`, `bundle_queries()`, `bundle_concepts()`,
    `bundle_templates()` — return named lists keyed by id
  - `bundle_object_type()`, `bundle_link_type()`, `bundle_action_type()` —
    return single entities with informative errors on unknown ids
  - `object_table()`, `object_source_sql()`, `object_source()`, `object_pk()`,
    `object_property_ids()` — object type accessors
  - `link_endpoints()` — returns `list(from = ..., to = ...)`
  - `spec_example_bundle()` — loads the aviation-demo bundle for testing

* Legacy key mappings are now handled automatically with warnings:
  - Bundle level: `objectTypes`/`object_types` → `objects`, etc.
  - Object type level: `table_name` → `source$table`,
    `primary_key`/`primary_key_id` → `primaryKey$properties`
  - Action level: `action_id` → `id`

## Changes

* `validate_bundle()` now maps legacy keys in JSON input to canonical keys
  (with a warning) before validating, so legacy bundles are accepted.

* `read_bundle()` and `bundle_from_json()` now go through `as_bundle()`,
  gaining legacy key mapping support.

* Updated ecosystem diagram in README (auditR merged into conceptR).

# ontologySpecR 0.1.0

* Initial release with core type system and JSON Schema.
* S3 classes for object types, link types, interfaces, action types, queries,
  concept definitions, and concept templates.
* Bundle serialization/deserialization (`bundle_to_json()`, `bundle_from_json()`,
  `read_bundle()`, `write_bundle()`).
* JSON Schema validation (`validate_bundle()`).
