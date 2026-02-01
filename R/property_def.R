#' Create a property definition
#'
#' Defines a single property on an object type, including its data type,
#' nullability, display metadata, and optional source mapping.
#'
#' @param id Character. Unique identifier for this property.
#' @param type Character. Data type, one of: `"string"`, `"integer"`,
#'   `"number"`, `"boolean"`, `"date"`, `"datetime"`, `"json"`.
#' @param nullable Logical. Whether NULL values are allowed (default `TRUE`).
#' @param display_name Character or NULL. Human-readable display name.
#' @param display_description Character or NULL. Description text.
#' @param source_column Character or NULL. Backend column name.
#' @param source_expression Character or NULL. Transform expression.
#' @param extensions Named list or NULL. Free-form extension data.
#' @return An S3 object of class `ontology_property_def`.
#' @export
#' @examples
#' property_def("airport_code", "string", nullable = FALSE,
#'              display_name = "Airport Code")
property_def <- function(id, type, nullable = TRUE,
                         display_name = NULL, display_description = NULL,
                         source_column = NULL, source_expression = NULL,
                         extensions = NULL) {
  assert_id(id, "id")
  valid_types <- c("string", "integer", "number", "boolean", "date", "datetime", "json")
  assert_choice(type, valid_types, "type")

  source <- compact(list(column = source_column, expression = source_expression))
  if (length(source) == 0) source <- NULL

  structure(
    compact(list(
      id = id,
      type = type,
      nullable = nullable,
      display = build_display(name = display_name, description = display_description),
      source = source,
      extensions = extensions
    )),
    class = "ontology_property_def"
  )
}

#' Test if an object is a property definition
#' @param x Object to test.
#' @return Logical.
#' @export
is_property_def <- function(x) inherits(x, "ontology_property_def")

#' @export
print.ontology_property_def <- function(x, ...) {
  cat(sprintf("<PropertyDef> %s : %s%s\n",
              x$id, x$type,
              if (isTRUE(x$nullable)) " (nullable)" else ""))
  invisible(x)
}

#' @export
as_list.ontology_property_def <- function(x, ...) {
  compact(list(
    id = x$id,
    type = x$type,
    nullable = x$nullable,
    display = x$display,
    source = x$source,
    extensions = x$extensions
  ))
}
