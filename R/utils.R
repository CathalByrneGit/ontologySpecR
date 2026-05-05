#' Remove NULL entries from a list
#'
#' @param x A list.
#' @return A list with NULL entries removed.
#' @keywords internal
compact <- function(x) {

  x[!vapply(x, is.null, logical(1))]
}

#' Assert that a value is a non-empty character string
#'
#' @param x Value to check.
#' @param name Name used in error message.
#' @keywords internal
assert_id <- function(x, name = "id") {
  if (!is.character(x) || length(x) != 1 || is.na(x) || nchar(x) == 0) {
    stop(sprintf("`%s` must be a non-empty character string.", name), call. = FALSE)
  }
  invisible(x)
}

#' Assert that a value is one of the allowed choices
#'
#' @param x Value to check.
#' @param choices Character vector of allowed values.
#' @param name Name used in error message.
#' @keywords internal
assert_choice <- function(x, choices, name = "value") {
  if (!is.null(x) && (!is.character(x) || length(x) != 1 || !(x %in% choices))) {
    stop(sprintf(
      "`%s` must be one of: %s",
      name, paste(dQuote(choices, FALSE), collapse = ", ")
    ), call. = FALSE)
  }
  invisible(x)
}

#' Null-coalescing operator
#'
#' Returns `a` if non-NULL, otherwise `b`. Useful for supplying defaults when
#' deserialising JSON where fields may be absent.
#'
#' @param a Value to test.
#' @param b Fallback value returned when `a` is `NULL`.
#' @return `a` if non-NULL, otherwise `b`.
#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Build a display list
#'
#' @param name Display name.
#' @param plural_name Plural display name.
#' @param description Description text.
#' @param icon Icon identifier.
#' @return A named list (or NULL if all NULL).
#' @keywords internal
build_display <- function(name = NULL, plural_name = NULL, description = NULL,
                          icon = NULL) {
  out <- compact(list(
    name = name,
    pluralName = plural_name,
    description = description,
    icon = icon
  ))
  if (length(out) == 0) NULL else out
}
