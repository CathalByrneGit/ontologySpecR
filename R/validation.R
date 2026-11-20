#' Validate a bundle against the ontologySpecR JSON Schema
#'
#' Validates a bundle (or raw JSON string) against the built-in JSON Schema.
#'
#' @param x An `ontology_bundle` object or a character string containing
#'   bundle JSON. Legacy keys in JSON input (e.g. `objectTypes`,
#'   `table_name`, `action_id`) are mapped to canonical keys, with a warning,
#'   before validation.
#' @param error Logical. If `TRUE` (default), throws an error on validation
#'   failure. If `FALSE`, returns a character vector of validation messages
#'   (empty character vector means valid).
#' @param verbose Logical. If `TRUE`, print validation messages to the console.
#'   Default `FALSE`.
#' @return `TRUE` invisibly on success. If `error = FALSE`, returns a
#'   character vector of validation messages (empty = valid).
#' @export
#' @importFrom jsonvalidate json_validate
validate_bundle <- function(x, error = TRUE, verbose = FALSE) {
  if (is_bundle(x)) {
    json <- bundle_to_json(x, pretty = FALSE)
  } else if (is.character(x) && length(x) == 1) {
    json <- normalize_legacy_json(x)
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
    if (error) return(invisible(TRUE))
    return(character(0))
  }

  errs <- attr(result, "errors")
  msgs <- if (!is.null(errs)) {
    paste0(errs$schemaPath, ": ", errs$message)
  } else {
    "Bundle is invalid (no detailed errors available)."
  }

  if (verbose) {
    message("Bundle validation failed:")
    for (msg in msgs) message("  - ", msg)
  }

  if (error) {
    stop(paste(c("Bundle validation failed:", msgs), collapse = "\n  "),
         call. = FALSE)
  }

  msgs
}

#' @keywords internal
normalize_legacy_json <- function(json) {
  raw <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(raw) || is.null(names(raw))) return(json)

  mapped <- map_legacy_bundle_keys(raw)
  if (identical(mapped, raw)) return(json)

  as.character(jsonlite::toJSON(mapped, auto_unbox = TRUE, null = "null"))
}
