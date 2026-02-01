#' Convert an ontology object to a plain list
#'
#' Generic function to convert ontologySpecR S3 objects into plain R
#' lists suitable for JSON serialization.
#'
#' @param x An ontologySpecR S3 object.
#' @param ... Additional arguments (unused).
#' @return A named list.
#' @export
as_list <- function(x, ...) {
  UseMethod("as_list")
}
