#' Validate a bundle against the ontologySpecR JSON Schema
#'
#' Validates a bundle (or raw JSON string) against the built-in JSON Schema.
#' Returns `TRUE` if valid, or a character vector of error messages if not.
#'
#' @param x An `ontology_bundle` object or a character string containing
#'   bundle JSON.
#' @param verbose Logical. If `TRUE`, print validation errors.
#'   Default `FALSE`.
#' @return `TRUE` if the bundle is valid, otherwise a character vector of
#'   validation error messages.
#' @export
#' @importFrom jsonvalidate json_validate
validate_bundle <- function(x, verbose = FALSE) {
  if (is_bundle(x)) {
    json <- bundle_to_json(x, pretty = FALSE)
  } else if (is.character(x) && length(x) == 1) {
    json <- x
  } else {
    stop("`x` must be an ontology_bundle or a JSON string.", call. = FALSE)
  }

  schema_path <- system.file(
    "schemas", "ontologySpecR.bundle.schema.json",
    package = "ontologySpecR",
    mustWork = TRUE
  )

  result <- jsonvalidate::json_validate(
    json,
    schema_path,
    verbose = TRUE,
    engine = "ajv"
  )

  if (isTRUE(result)) {
    if (verbose) message("Bundle is valid.")
    return(TRUE)
  }

  errors <- attr(result, "errors")
  if (!is.null(errors)) {
    msgs <- paste0(errors$schemaPath, ": ", errors$message)
  } else {
    msgs <- "Bundle is invalid (no detailed errors available)."
  }

  if (verbose) {
    message("Bundle validation failed:")
    for (msg in msgs) message("  - ", msg)
  }

  msgs
}
