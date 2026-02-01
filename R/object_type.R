#' Create an object type definition
#'
#' Defines a semantic object type representing a real-world entity,
#' including its properties, primary key, interface implementations,
#' and backend source binding.
#'
#' @param id Character. Unique identifier for this object type.
#' @param properties List of `ontology_property_def` objects.
#' @param primary_key An `ontology_primary_key_def` object, or a character
#'   vector of property IDs (which will be wrapped automatically).
#' @param display_name Character or NULL. Human-readable name.
#' @param display_plural Character or NULL. Plural form.
#' @param display_description Character or NULL. Description.
#' @param display_icon Character or NULL. Icon identifier.
#' @param implements Character vector or NULL. Interface IDs this type implements.
#' @param source_kind Character or NULL. Backend kind: `"table"`, `"view"`,
#'   `"query"`, `"external"`.
#' @param source_uri Character or NULL. Backend URI.
#' @param source_table Character or NULL. Table name.
#' @param source_schema Character or NULL. Schema name.
#' @param source_sql Character or NULL. Defining SQL.
#' @param extensions Named list or NULL. Free-form extension data.
#' @return An S3 object of class `ontology_object_type`.
#' @export
#' @examples
#' ot <- object_type(
#'   id = "Airport",
#'   properties = list(
#'     property_def("airport_id", "string", nullable = FALSE),
#'     property_def("name", "string"),
#'     property_def("latitude", "number"),
#'     property_def("longitude", "number")
#'   ),
#'   primary_key = "airport_id",
#'   display_name = "Airport",
#'   source_kind = "table",
#'   source_table = "airports"
#' )
object_type <- function(id, properties, primary_key,
                        display_name = NULL, display_plural = NULL,
                        display_description = NULL, display_icon = NULL,
                        implements = NULL,
                        source_kind = NULL, source_uri = NULL,
                        source_table = NULL, source_schema = NULL,
                        source_sql = NULL,
                        extensions = NULL) {
  assert_id(id, "id")

  if (!is.list(properties)) {
    stop("`properties` must be a list of property_def objects.", call. = FALSE)
  }

  # Allow shorthand: character vector -> primary_key_def

if (is.character(primary_key)) {
    primary_key <- primary_key_def(primary_key)
  }
  if (!is_primary_key_def(primary_key)) {
    stop("`primary_key` must be a primary_key_def or character vector.", call. = FALSE)
  }

  source <- NULL
  if (!is.null(source_kind)) {
    assert_choice(source_kind, c("table", "view", "query", "external"), "source_kind")
    source <- compact(list(
      kind = source_kind,
      uri = source_uri,
      table = source_table,
      schema = source_schema,
      sql = source_sql
    ))
  }

  structure(
    compact(list(
      id = id,
      display = build_display(
        name = display_name, plural_name = display_plural,
        description = display_description, icon = display_icon
      ),
      properties = properties,
      primaryKey = primary_key,
      implements = if (!is.null(implements)) as.list(implements) else NULL,
      source = source,
      extensions = extensions
    )),
    class = "ontology_object_type"
  )
}

#' Test if an object is an object type
#' @param x Object to test.
#' @return Logical.
#' @export
is_object_type <- function(x) inherits(x, "ontology_object_type")

#' @export
print.ontology_object_type <- function(x, ...) {
  n_props <- length(x$properties)
  cat(sprintf("<ObjectType> %s (%d properties, PK: %s)\n",
              x$id, n_props,
              paste(x$primaryKey$properties, collapse = ", ")))
  invisible(x)
}

#' @export
as_list.ontology_object_type <- function(x, ...) {
  compact(list(
    id = x$id,
    display = x$display,
    properties = lapply(x$properties, as_list),
    primaryKey = as_list(x$primaryKey),
    implements = x$implements,
    source = x$source,
    extensions = x$extensions
  ))
}
