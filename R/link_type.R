#' Create a link type definition
#'
#' Defines a typed relationship between two object types, including
#' cardinality, directionality, and join semantics.
#'
#' @param id Character. Unique identifier for this link type.
#' @param from Character. Source object type ID.
#' @param to Character. Target object type ID.
#' @param cardinality Character. One of: `"one-to-one"`, `"one-to-many"`,
#'   `"many-to-one"`, `"many-to-many"`. Default `"many-to-many"`.
#' @param directed Logical. Whether the link is directed (default `TRUE`).
#' @param display_name Character or NULL. Human-readable name.
#' @param display_description Character or NULL. Description.
#' @param join_from_keys Character vector or NULL. Property IDs on source side.
#' @param join_to_keys Character vector or NULL. Property IDs on target side.
#' @param join_sql Character or NULL. Explicit SQL for link edges.
#' @param extensions Named list or NULL. Free-form extension data.
#' @return An S3 object of class `ontology_link_type`.
#' @export
#' @examples
#' link_type(
#'   id = "FlightRoute",
#'   from = "Airport",
#'   to = "Airport",
#'   cardinality = "many-to-many",
#'   display_name = "Flight Route"
#' )
link_type <- function(id, from, to,
                      cardinality = "many-to-many",
                      directed = TRUE,
                      display_name = NULL, display_description = NULL,
                      join_from_keys = NULL, join_to_keys = NULL,
                      join_sql = NULL,
                      extensions = NULL) {
  assert_id(id, "id")
  assert_id(from, "from")
  assert_id(to, "to")
  assert_choice(
    cardinality,
    c("one-to-one", "one-to-many", "many-to-one", "many-to-many"),
    "cardinality"
  )

  join <- compact(list(
    fromKeys = if (!is.null(join_from_keys)) as.list(join_from_keys) else NULL,
    toKeys = if (!is.null(join_to_keys)) as.list(join_to_keys) else NULL,
    sql = join_sql
  ))
  if (length(join) == 0) join <- NULL

  structure(
    compact(list(
      id = id,
      display = build_display(name = display_name, description = display_description),
      from = from,
      to = to,
      cardinality = cardinality,
      directed = directed,
      join = join,
      extensions = extensions
    )),
    class = "ontology_link_type"
  )
}

#' Test if an object is a link type
#' @param x Object to test.
#' @return Logical.
#' @export
is_link_type <- function(x) inherits(x, "ontology_link_type")

#' @export
print.ontology_link_type <- function(x, ...) {
  arrow <- if (isTRUE(x$directed)) "->" else "<->"
  cat(sprintf("<LinkType> %s: %s %s %s (%s)\n",
              x$id, x$from, arrow, x$to, x$cardinality))
  invisible(x)
}

#' @export
as_list.ontology_link_type <- function(x, ...) {
  compact(list(
    id = x$id,
    display = x$display,
    from = x$from,
    to = x$to,
    cardinality = x$cardinality,
    directed = x$directed,
    join = x$join,
    extensions = x$extensions
  ))
}
