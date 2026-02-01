#' Create a primary key definition
#'
#' Defines which properties form the primary key for an object type.
#'
#' @param properties Character vector. Property IDs that form the primary key.
#' @param strategy Character. Key strategy: `"natural"` or `"surrogate"`.
#'   Default `"natural"`.
#' @return An S3 object of class `ontology_primary_key_def`.
#' @export
#' @examples
#' primary_key_def("airport_id")
#' primary_key_def(c("region", "code"), strategy = "natural")
primary_key_def <- function(properties, strategy = "natural") {
  if (!is.character(properties) || length(properties) == 0) {
    stop("`properties` must be a non-empty character vector.", call. = FALSE)
  }
  assert_choice(strategy, c("natural", "surrogate"), "strategy")

  structure(
    list(
      properties = properties,
      strategy = strategy
    ),
    class = "ontology_primary_key_def"
  )
}

#' Test if an object is a primary key definition
#' @param x Object to test.
#' @return Logical.
#' @export
is_primary_key_def <- function(x) inherits(x, "ontology_primary_key_def")

#' @export
print.ontology_primary_key_def <- function(x, ...) {
  cat(sprintf("<PrimaryKeyDef> [%s] (%s)\n",
              paste(x$properties, collapse = ", "), x$strategy))
  invisible(x)
}

#' @export
as_list.ontology_primary_key_def <- function(x, ...) {
  compact(list(
    properties = as.list(x$properties),
    strategy = x$strategy
  ))
}
