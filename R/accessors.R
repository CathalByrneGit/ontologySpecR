# ============================================================================
# Bundle Accessor Layer
#
# Canonical interface for reading bundle contents. All downstream packages
# should use these accessors rather than accessing `$` fields directly.
# This layer handles legacy key mappings (objectTypes -> objects, etc.) and
# provides informative error messages for missing entities.
# ============================================================================

# Track whether we've warned about legacy keys this session
.legacy_warned <- new.env(parent = emptyenv())

#' Coerce to an ontology bundle
#'
#' Converts a raw list (e.g. from `jsonlite::fromJSON()`) or a file path
#' into an `ontology_bundle` S3 object. This is the **only** tolerant reader
#' in the ecosystem: it maps legacy key names to canonical ones and warns

#' when legacy keys are encountered.
#'
#' Legacy mappings handled:
#' - Bundle level: `objectTypes`/`object_types` -> `objects`,
#'   `linkTypes`/`link_types` -> `links`, etc.
#' - Object type level: `table_name` -> `source$table`,
#'   flat `primary_key`/`primary_key_id` -> `primaryKey$properties`
#' - Property level: `source$name` -> `source$column`
#' - Link level: `join$from_keys`/`join$to_keys` -> `join$fromKeys`/`join$toKeys`
#' - Action level: `action_id` -> `id`
#'
#' @param x An `ontology_bundle`, a list, or a path to a JSON file.
#' @return An `ontology_bundle` S3 object.
#' @export
#' @examples
#' # From a file
#' b <- as_bundle(system.file("examples", "aviation-demo.json",
#'                            package = "ontologySpecR"))
#'
#' # From an existing bundle (returned as-is)
#' as_bundle(b)
as_bundle <- function(x) {
 if (is_bundle(x)) {
    return(x)
  }

  # File path
 if (is.character(x) && length(x) == 1 && file.exists(x)) {
    json <- paste(readLines(x, warn = FALSE), collapse = "\n")
    raw <- jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE)
    return(as_bundle(raw))
  }

  if (!is.list(x)) {
    stop("`x` must be an ontology_bundle, a list, or a path to a JSON file.",
         call. = FALSE)
  }

  # Map legacy bundle-level keys
  raw <- map_legacy_bundle_keys(x)

  # Rebuild to get proper S3 objects
  rebuild_bundle(raw)
}

#' @keywords internal
map_legacy_bundle_keys <- function(raw) {
  # Bundle-level key mappings
  key_mappings <- list(
    objects    = c("objectTypes", "object_types"),
    links      = c("linkTypes", "link_types"),
    interfaces = c("interfaceTypes", "interface_types"),
    actions    = c("actionTypes", "action_types"),
    queries    = c("queryDefs", "query_defs")
  )

  for (canonical in names(key_mappings)) {
    if (is.null(raw[[canonical]])) {
      for (legacy in key_mappings[[canonical]]) {
        if (!is.null(raw[[legacy]])) {
          warn_legacy_key(legacy, canonical)
          raw[[canonical]] <- raw[[legacy]]
          raw[[legacy]] <- NULL
          break
        }
      }
    }
  }

  # Map legacy object-type-level keys
  if (!is.null(raw$objects)) {
    raw$objects <- lapply(raw$objects, map_legacy_object_type_keys)
  }

  # Map legacy link-level keys
  if (!is.null(raw$links)) {
    raw$links <- lapply(raw$links, map_legacy_link_keys)
  }

  # Map legacy action-level keys
  if (!is.null(raw$actions)) {
    raw$actions <- lapply(raw$actions, map_legacy_action_keys)
  }

  raw
}

#' @keywords internal
map_legacy_object_type_keys <- function(ot) {
  # table_name -> source$table
  if (!is.null(ot$table_name) && is.null(ot$source$table)) {
    warn_legacy_key("table_name", "source$table")
    if (is.null(ot$source)) ot$source <- list()
    ot$source$table <- ot$table_name
    ot$source$kind <- ot$source$kind %||% "table"
    ot$table_name <- NULL
  }

  # Flat primary_key / primary_key_id -> primaryKey$properties
  if (is.null(ot$primaryKey)) {
    pk_val <- ot$primary_key %||% ot$primary_key_id
    if (!is.null(pk_val)) {
      warn_legacy_key("primary_key/primary_key_id", "primaryKey$properties")
      if (is.character(pk_val)) {
        ot$primaryKey <- list(properties = as.list(pk_val), strategy = "natural")
      } else if (is.list(pk_val) && is.null(pk_val$properties)) {
        # It's a list but without $properties structure
        ot$primaryKey <- list(properties = pk_val, strategy = "natural")
      } else {
        ot$primaryKey <- pk_val
      }
      ot$primary_key <- NULL
      ot$primary_key_id <- NULL
    }
  }

  # property source$name -> source$column
  if (!is.null(ot$properties)) {
    ot$properties <- lapply(ot$properties, map_legacy_property_keys)
  }

  ot
}

#' @keywords internal
map_legacy_property_keys <- function(prop) {
  if (!is.null(prop$source$name) && is.null(prop$source$column)) {
    warn_legacy_key("source$name", "source$column")
    prop$source$column <- prop$source$name
    prop$source$name <- NULL
  }
  prop
}

#' @keywords internal
map_legacy_link_keys <- function(link) {
  if (!is.null(link$join$from_keys) && is.null(link$join$fromKeys)) {
    warn_legacy_key("join$from_keys", "join$fromKeys")
    link$join$fromKeys <- link$join$from_keys
    link$join$from_keys <- NULL
  }
  if (!is.null(link$join$to_keys) && is.null(link$join$toKeys)) {
    warn_legacy_key("join$to_keys", "join$toKeys")
    link$join$toKeys <- link$join$to_keys
    link$join$to_keys <- NULL
  }
  link
}

#' @keywords internal
map_legacy_action_keys <- function(action) {
  # action_id -> id
  if (is.null(action$id) && !is.null(action$action_id)) {
    warn_legacy_key("action_id", "id")
    action$id <- action$action_id
    action$action_id <- NULL
  }

  action
}

#' @keywords internal
warn_legacy_key <- function(legacy, canonical) {
  key <- paste0(legacy, "->", canonical)
  if (is.null(.legacy_warned[[key]])) {
    .legacy_warned[[key]] <- TRUE
    warning(
      sprintf("Legacy key '%s' found; use '%s' instead.", legacy, canonical),
      call. = FALSE
    )
  }
}

# ============================================================================
# Bundle Metadata Accessors
# ============================================================================

#' Get the bundle identifier
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return Character.
#' @export
bundle_id <- function(b) {
  b <- as_bundle(b)
  b$bundleId
}

#' Get the bundle version
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return Character.
#' @export
bundle_version <- function(b) {
  b <- as_bundle(b)
  b$bundleVersion
}

#' Get the bundle spec version
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return Character.
#' @export
bundle_spec_version <- function(b) {
  b <- as_bundle(b)
  b$specVersion
}

# ============================================================================
# Bundle Collection Accessors (return named lists)
# ============================================================================

#' Get all object types from a bundle
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return A named list of object types, keyed by id.
#' @export
bundle_object_types <- function(b) {
  b <- as_bundle(b)
  index_by_id(b$objects %||% list())
}

#' Get all link types from a bundle
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return A named list of link types, keyed by id.
#' @export
bundle_link_types <- function(b) {
  b <- as_bundle(b)
  index_by_id(b$links %||% list())
}

#' Get all interface types from a bundle
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return A named list of interface types, keyed by id.
#' @export
bundle_interface_types <- function(b) {
  b <- as_bundle(b)
  index_by_id(b$interfaces %||% list())
}

#' Get all action types from a bundle
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return A named list of action types, keyed by id.
#' @export
bundle_action_types <- function(b) {
  b <- as_bundle(b)
  index_by_id(b$actions %||% list())
}

#' Get all queries from a bundle
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return A named list of queries, keyed by id.
#' @export
bundle_queries <- function(b) {
  b <- as_bundle(b)
  index_by_id(b$queries %||% list())
}

#' Get all concepts from a bundle
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return A named list of concepts, keyed by id.
#' @export
bundle_concepts <- function(b) {
  b <- as_bundle(b)
  index_by_id(b$concepts %||% list())
}

#' Get all concept templates from a bundle
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @return A named list of templates, keyed by id.
#' @export
bundle_templates <- function(b) {
  b <- as_bundle(b)
  index_by_id(b$templates %||% list())
}

#' @keywords internal
index_by_id <- function(items) {
  if (length(items) == 0) return(list())
  ids <- vapply(items, function(item) item$id %||% "", character(1))
  stats::setNames(items, ids)
}

# ============================================================================
# Single Entity Accessors (with informative errors)
# ============================================================================

#' Get a single object type by id
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @param id Character. The object type id.
#' @return An object type.
#' @export
bundle_object_type <- function(b, id) {
  types <- bundle_object_types(b)
  if (is.null(types[[id]])) {
    abort_unknown_id("object type", id, names(types))
  }
  types[[id]]
}

#' Get a single link type by id
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @param id Character. The link type id.
#' @return A link type.
#' @export
bundle_link_type <- function(b, id) {
  types <- bundle_link_types(b)
  if (is.null(types[[id]])) {
    abort_unknown_id("link type", id, names(types))
  }
  types[[id]]
}

#' Get a single action type by id
#'
#' @param b A bundle (or anything `as_bundle()` accepts).
#' @param id Character. The action type id.
#' @return An action type.
#' @export
bundle_action_type <- function(b, id) {
  types <- bundle_action_types(b)
  if (is.null(types[[id]])) {
    abort_unknown_id("action type", id, names(types))
  }
  types[[id]]
}

#' @keywords internal
abort_unknown_id <- function(entity_type, id, valid_ids) {
  if (length(valid_ids) == 0) {
    stop(sprintf("Unknown %s: '%s'. Bundle contains no %ss.",
                 entity_type, id, entity_type), call. = FALSE)
  }
  stop(sprintf("Unknown %s: '%s'. Valid ids: %s",
               entity_type, id, paste(valid_ids, collapse = ", ")),
       call. = FALSE)
}

# ============================================================================
# Object Type Accessors
# ============================================================================

#' Get the table name for an object type
#'
#' Returns the backing table name from `source$table`, falling back to `id`.
#' Aborts if the object type is query-backed; use `object_source_sql()` instead.
#'
#' @param ot An object type.
#' @return Character. The table name.
#' @export
object_table <- function(ot) {
  source <- ot$source
  if (!is.null(source$kind) && source$kind == "query") {
    stop(sprintf(
      "Object type '%s' is query-backed (source$kind = 'query'). Use object_source_sql() instead.",
      ot$id
    ), call. = FALSE)
  }
  source$table %||% ot$id
}

#' Get the source SQL for a query-backed object type
#'
#' @param ot An object type.
#' @return Character. The SQL expression.
#' @export
object_source_sql <- function(ot) {
  ot$source$sql
}

#' Get the full source binding for an object type
#'
#' @param ot An object type.
#' @return A list with kind, table, schema, uri, sql as applicable.
#' @export
object_source <- function(ot) {
  ot$source
}

#' Get primary key properties for an object type
#'
#' @param ot An object type.
#' @return Character vector of property ids that form the primary key.
#' @export
object_pk <- function(ot) {
  pk <- ot$primaryKey
  if (is.null(pk)) {
    stop(sprintf("Object type '%s' has no primaryKey.", ot$id), call. = FALSE)
  }

  # Handle structured primaryKey (from ontologySpecR constructors)
  if (is.list(pk) && !is.null(pk$properties)) {
    props <- pk$properties
    if (is.list(props)) return(unlist(props))
    return(props)
  }

  # Handle raw character (legacy or simple case)
  if (is.character(pk)) {
    return(pk)
  }

  # Handle ontology_primary_key_def S3 object
  if (inherits(pk, "ontology_primary_key_def")) {
    return(pk$properties)
  }

  stop(sprintf("Could not extract primary key from object type '%s'.", ot$id),
       call. = FALSE)
}

#' Get property ids for an object type
#'
#' @param ot An object type.
#' @return Character vector of property ids.
#' @export
object_property_ids <- function(ot) {
  props <- ot$properties %||% list()
  vapply(props, function(p) p$id %||% "", character(1))
}

#' Get the properties of an object type, named by id
#'
#' @param ot An object type.
#' @return A named list of property definitions, keyed by id.
#' @export
object_properties <- function(ot) {
  index_by_id(ot$properties %||% list())
}

#' Get a single property of an object type
#'
#' @param ot An object type.
#' @param id Character. The property id.
#' @return A property definition. Aborts on an unknown id, listing the valid ones.
#' @export
object_property <- function(ot, id) {
  props <- object_properties(ot)
  if (is.null(props[[id]])) {
    valid <- if (length(props) == 0) "(none)" else paste(names(props), collapse = ", ")
    stop(sprintf("Unknown property '%s' on object type '%s'. Valid ids: %s",
                 id, ot$id, valid), call. = FALSE)
  }
  props[[id]]
}

#' Get the backing column of a property
#'
#' Returns `source$column`, falling back to the property id when the property
#' has no explicit column mapping.
#'
#' @param p A property definition.
#' @return Character.
#' @export
property_column <- function(p) {
  p$source$column %||% p$id
}

#' Get the source expression of a property
#'
#' @param p A property definition.
#' @return Character, or `NULL` if the property has no source expression.
#' @export
property_expression <- function(p) {
  p$source$expression
}

# ============================================================================
# Link Type Accessors
# ============================================================================

#' Get the from/to endpoints of a link type
#'
#' @param lt A link type.
#' @return A list with `from` and `to` object type ids.
#' @export
link_endpoints <- function(lt) {
  list(from = lt$from, to = lt$to)
}

#' Get the join keys of a link type
#'
#' Reads `join$fromKeys` / `join$toKeys` and flattens them to character
#' vectors. Aborts if either side is missing or the two lengths differ.
#'
#' @param lt A link type.
#' @return A list with `from` and `to` character vectors of property ids.
#' @export
link_join_keys <- function(lt) {
  from <- as.character(unlist(lt$join$fromKeys, use.names = FALSE))
  to <- as.character(unlist(lt$join$toKeys, use.names = FALSE))
  if (length(from) == 0 || length(to) == 0) {
    stop(sprintf("Link type '%s' is missing join keys.", lt$id), call. = FALSE)
  }
  if (length(from) != length(to)) {
    stop(sprintf(
      "Link type '%s' has %d fromKeys but %d toKeys; they must match.",
      lt$id, length(from), length(to)
    ), call. = FALSE)
  }
  list(from = from, to = to)
}

# ============================================================================
# Convenience: Example Bundle
# ============================================================================

#' Load the example aviation bundle
#'
#' Returns the aviation-demo.json bundle shipped with ontologySpecR. Useful
#' for testing and examples across the ecosystem.
#'
#' @return An `ontology_bundle`.
#' @export
#' @examples
#' b <- spec_example_bundle()
#' bundle_id(b)
#' # [1] "aviation-demo"
spec_example_bundle <- function() {
  path <- system.file("examples", "aviation-demo.json", package = "ontologySpecR")
  if (path == "") {
    stop("Example bundle not found. Is ontologySpecR installed correctly?",
         call. = FALSE)
  }
  read_bundle(path)
}
