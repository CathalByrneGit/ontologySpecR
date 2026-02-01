#' Create a query definition
#'
#' Defines a reusable query that can be parameterized and returns
#' object sets, tables, or scalar values.
#'
#' @param id Character. Unique identifier for this query.
#' @param returns_kind Character. Return type: `"objectSet"`, `"table"`,
#'   or `"scalar"`.
#' @param returns_object_type_id Character or NULL. Object type ID for
#'   objectSet returns.
#' @param parameters List of `ontology_parameter_def` objects.
#' @param display_name Character or NULL. Human-readable name.
#' @param display_description Character or NULL. Description.
#' @param def_kind Character or NULL. Definition language: `"sql"`, `"r"`,
#'   or `"dsl"`.
#' @param def_body Character or NULL. The query body.
#' @param extensions Named list or NULL. Free-form extension data.
#' @return An S3 object of class `ontology_query_def`.
#' @export
#' @examples
#' query_def(
#'   id = "NearbyAirports",
#'   returns_kind = "objectSet",
#'   returns_object_type_id = "Airport",
#'   parameters = list(
#'     parameter_def("lat", "number", required = TRUE),
#'     parameter_def("lon", "number", required = TRUE),
#'     parameter_def("radius_km", "number", required = TRUE)
#'   ),
#'   def_kind = "sql",
#'   def_body = "SELECT * FROM airports WHERE distance(latitude, longitude, :lat, :lon) < :radius_km"
#' )
query_def <- function(id, returns_kind,
                      returns_object_type_id = NULL,
                      parameters = list(),
                      display_name = NULL, display_description = NULL,
                      def_kind = NULL, def_body = NULL,
                      extensions = NULL) {
  assert_id(id, "id")
  assert_choice(returns_kind, c("objectSet", "table", "scalar"), "returns_kind")
  if (!is.null(def_kind)) {
    assert_choice(def_kind, c("sql", "r", "dsl"), "def_kind")
  }

  returns <- compact(list(
    kind = returns_kind,
    objectTypeId = returns_object_type_id
  ))

  definition <- NULL
  if (!is.null(def_kind)) {
    definition <- compact(list(kind = def_kind, body = def_body))
  }

  structure(
    compact(list(
      id = id,
      display = build_display(name = display_name, description = display_description),
      parameters = if (length(parameters) > 0) parameters else NULL,
      returns = returns,
      definition = definition,
      extensions = extensions
    )),
    class = "ontology_query_def"
  )
}

#' Test if an object is a query definition
#' @param x Object to test.
#' @return Logical.
#' @export
is_query_def <- function(x) inherits(x, "ontology_query_def")

#' @export
print.ontology_query_def <- function(x, ...) {
  ret <- x$returns$kind
  if (!is.null(x$returns$objectTypeId)) {
    ret <- paste0(ret, "<", x$returns$objectTypeId, ">")
  }
  cat(sprintf("<QueryDef> %s -> %s\n", x$id, ret))
  invisible(x)
}

#' @export
as_list.ontology_query_def <- function(x, ...) {
  compact(list(
    id = x$id,
    display = x$display,
    parameters = if (!is.null(x$parameters)) lapply(x$parameters, as_list) else NULL,
    returns = x$returns,
    definition = x$definition,
    extensions = x$extensions
  ))
}
