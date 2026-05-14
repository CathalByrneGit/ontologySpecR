#' Create a link type definition
#'
#' Defines a typed relationship between two object types, including
#' cardinality, directionality, join semantics, and optional DuckPGQ
#' source-table mapping.
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
#' @param source_table Character or NULL. Backing edge table name (required for
#'   DuckPGQ DDL generation). Must be set together with `source_from_col` and
#'   `source_to_col`.
#' @param source_from_col Character or NULL. FK column in `source_table`
#'   pointing to the `from` object type's primary key.
#' @param source_to_col Character or NULL. FK column in `source_table`
#'   pointing to the `to` object type's primary key.
#' @param source_filter_col Character or NULL. Discriminator column used to
#'   select rows of this link type from a shared edge table (e.g. `"rel_type"`).
#'   Must be set together with `source_filter_val`.
#' @param source_filter_val Character vector or NULL. Allowed values for
#'   `source_filter_col` (e.g. `c("officer of", "director of")`).
#'   Must be a non-empty vector when provided.
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
                      source_table = NULL,
                      source_from_col = NULL,
                      source_to_col = NULL,
                      source_filter_col = NULL,
                      source_filter_val = NULL,
                      extensions = NULL) {
  assert_id(id, "id")
  assert_id(from, "from")
  assert_id(to, "to")
  assert_choice(
    cardinality,
    c("one-to-one", "one-to-many", "many-to-one", "many-to-many"),
    "cardinality"
  )

  # Source mapping validation
  src_trio <- c(
    table   = !is.null(source_table),
    fromCol = !is.null(source_from_col),
    toCol   = !is.null(source_to_col)
  )
  if (any(src_trio) && !all(src_trio)) {
    stop(
      "`source_table`, `source_from_col`, and `source_to_col` must all be set together.",
      call. = FALSE
    )
  }

  filter_set <- c(!is.null(source_filter_col), !is.null(source_filter_val))
  if (filter_set[[1]] != filter_set[[2]]) {
    stop("`source_filter_col` and `source_filter_val` must both be set or both NULL.",
         call. = FALSE)
  }
  if (!is.null(source_filter_val)) {
    if (!is.character(source_filter_val) || length(source_filter_val) == 0L) {
      stop("`source_filter_val` must be a non-empty character vector.", call. = FALSE)
    }
  }

  join <- compact(list(
    fromKeys = if (!is.null(join_from_keys)) as.list(join_from_keys) else NULL,
    toKeys = if (!is.null(join_to_keys)) as.list(join_to_keys) else NULL,
    sql = join_sql
  ))
  if (length(join) == 0) join <- NULL

  source <- compact(list(
    table     = source_table,
    fromCol   = source_from_col,
    toCol     = source_to_col,
    filterCol = source_filter_col,
    filterVal = if (!is.null(source_filter_val)) as.list(source_filter_val) else NULL
  ))
  if (length(source) == 0) source <- NULL

  structure(
    compact(list(
      id = id,
      display = build_display(name = display_name, description = display_description),
      from = from,
      to = to,
      cardinality = cardinality,
      directed = directed,
      join = join,
      source = source,
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
    source = x$source,
    extensions = x$extensions
  ))
}

