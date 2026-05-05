#' Convert a bundle to JSON
#'
#' Serializes an ontology bundle to a JSON string.
#'
#' @param x An `ontology_bundle` object.
#' @param pretty Logical. Whether to pretty-print the JSON (default `TRUE`).
#' @return A character string containing JSON.
#' @export
#' @importFrom jsonlite toJSON
bundle_to_json <- function(x, pretty = TRUE) {
  if (!is_bundle(x)) {
    stop("`x` must be an ontology_bundle.", call. = FALSE)
  }
  jsonlite::toJSON(as_list(x), auto_unbox = TRUE, pretty = pretty, null = "null")
}

#' Parse a bundle from JSON
#'
#' Deserializes a JSON string into an `ontology_bundle` object.
#'
#' @param json Character. A JSON string representing a bundle.
#' @return An `ontology_bundle` object.
#' @export
#' @importFrom jsonlite fromJSON
bundle_from_json <- function(json) {
  raw <- jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE)
  rebuild_bundle(raw)
}

#' Write a bundle to a JSON file
#'
#' @param x An `ontology_bundle` object.
#' @param path Character. File path to write to.
#' @param pretty Logical. Whether to pretty-print (default `TRUE`).
#' @return The file path (invisibly).
#' @export
write_bundle <- function(x, path, pretty = TRUE) {
  json <- bundle_to_json(x, pretty = pretty)
  writeLines(json, path)
  invisible(path)
}

#' Read a bundle from a JSON file
#'
#' @param path Character. File path to read from.
#' @return An `ontology_bundle` object.
#' @export
read_bundle <- function(path) {
  json <- paste(readLines(path, warn = FALSE), collapse = "\n")
  bundle_from_json(json)
}

# --- Internal reconstruction helpers ---

#' @keywords internal
rebuild_bundle <- function(raw) {
  bundle(
    bundle_id = raw$bundleId,
    bundle_version = raw$bundleVersion,
    spec_version = raw$specVersion %||% "0.1.0",
    metadata = raw$metadata,
    objects = lapply(raw$objects %||% list(), rebuild_object_type),
    links = lapply(raw$links %||% list(), rebuild_link_type),
    interfaces = lapply(raw$interfaces %||% list(), rebuild_interface_type),
    actions = lapply(raw$actions %||% list(), rebuild_action_type),
    queries = lapply(raw$queries %||% list(), rebuild_query_def),
    concepts = lapply(raw$concepts %||% list(), rebuild_concept_def),
    templates = lapply(raw$templates %||% list(), rebuild_concept_template_def),
    extensions = raw$extensions
  )
}

#' @keywords internal
rebuild_property_def <- function(raw) {
  property_def(
    id = raw$id,
    type = raw$type,
    nullable = raw$nullable %||% TRUE,
    display_name = raw$display$name,
    display_description = raw$display$description,
    source_column = raw$source$column,
    source_expression = raw$source$expression,
    extensions = raw$extensions
  )
}

#' @keywords internal
rebuild_primary_key_def <- function(raw) {
  props <- if (is.list(raw$properties)) unlist(raw$properties) else raw$properties
  primary_key_def(
    properties = props,
    strategy = raw$strategy %||% "natural"
  )
}

#' @keywords internal
rebuild_object_type <- function(raw) {
  object_type(
    id = raw$id,
    properties = lapply(raw$properties %||% list(), rebuild_property_def),
    primary_key = rebuild_primary_key_def(raw$primaryKey),
    display_name = raw$display$name,
    display_plural = raw$display$pluralName,
    display_description = raw$display$description,
    display_icon = raw$display$icon,
    implements = if (!is.null(raw$implements)) unlist(raw$implements) else NULL,
    source_kind = raw$source$kind,
    source_uri = raw$source$uri,
    source_table = raw$source$table,
    source_schema = raw$source$schema,
    source_sql = raw$source$sql,
    extensions = raw$extensions
  )
}

#' @keywords internal
rebuild_link_type <- function(raw) {
  link_type(
    id = raw$id,
    from = raw$from,
    to = raw$to,
    cardinality = raw$cardinality %||% "many-to-many",
    directed = raw$directed %||% TRUE,
    display_name = raw$display$name,
    display_description = raw$display$description,
    join_from_keys = if (!is.null(raw$join$fromKeys)) unlist(raw$join$fromKeys) else NULL,
    join_to_keys = if (!is.null(raw$join$toKeys)) unlist(raw$join$toKeys) else NULL,
    join_sql = raw$join$sql,
    extensions = raw$extensions
  )
}

#' @keywords internal
rebuild_interface_type <- function(raw) {
  interface_type(
    id = raw$id,
    display_name = raw$display$name,
    display_description = raw$display$description,
    required_properties = lapply(
      raw$requiredProperties %||% list(),
      function(p) property_requirement(p$id, p$type, p$nullable %||% TRUE)
    ),
    required_links = lapply(
      raw$requiredLinks %||% list(),
      function(l) link_requirement(l$linkTypeId, l$minCount %||% 0L, l$maxCount)
    ),
    required_actions = if (!is.null(raw$requiredActions)) unlist(raw$requiredActions) else character(0),
    extensions = raw$extensions
  )
}

#' @keywords internal
rebuild_parameter_def <- function(raw) {
  parameter_def(
    id = raw$id,
    type = raw$type,
    required = raw$required %||% FALSE,
    display_name = raw$display$name,
    display_description = raw$display$description
  )
}

#' @keywords internal
rebuild_effect_def <- function(raw) {
  effect_def(
    kind = raw$kind,
    object_type_id = raw$objectTypeId,
    notes = raw$notes
  )
}

#' @keywords internal
rebuild_action_type <- function(raw) {
  action_type(
    id = raw$id,
    targets = if (is.list(raw$targets)) unlist(raw$targets) else raw$targets,
    parameters = lapply(raw$parameters %||% list(), rebuild_parameter_def),
    display_name = raw$display$name,
    display_description = raw$display$description,
    effects = lapply(raw$effects %||% list(), rebuild_effect_def),
    impl_kind = raw$implementation$kind,
    impl_entrypoint = raw$implementation$entrypoint,
    policy = raw$policy,
    extensions = raw$extensions
  )
}

#' @keywords internal
rebuild_query_def <- function(raw) {
  query_def(
    id = raw$id,
    returns_kind = raw$returns$kind,
    returns_object_type_id = raw$returns$objectTypeId,
    parameters = lapply(raw$parameters %||% list(), rebuild_parameter_def),
    display_name = raw$display$name,
    display_description = raw$display$description,
    def_kind = raw$definition$kind,
    def_body = raw$definition$body,
    extensions = raw$extensions
  )
}

#' @keywords internal
rebuild_concept_def <- function(raw) {
  concept_def(
    id               = raw$id,
    object_type_id   = raw$objectTypeId,
    scope            = raw$scope,
    version          = raw$version,
    sql_expr         = raw$sqlExpr,
    status           = raw$status %||% "draft",
    rationale        = raw$rationale,
    source_standard  = raw$sourceStandard,
    template_id      = raw$templateId,
    parameter_values = raw$parameterValues,
    display_name     = raw$display$name,
    display_description = raw$display$description,
    extensions       = raw$extensions
  )
}

#' @keywords internal
rebuild_concept_template_def <- function(raw) {
  concept_template_def(
    id              = raw$id,
    object_type_id  = raw$objectTypeId,
    base_sql_expr   = raw$baseSqlExpr,
    parameters      = lapply(raw$parameters %||% list(), rebuild_parameter_def),
    source_standard = raw$sourceStandard,
    display_name    = raw$display$name,
    display_description = raw$display$description,
    extensions      = raw$extensions
  )
}
