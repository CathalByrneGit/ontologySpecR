#' Create a concept definition
#'
#' A `concept_def` describes a versioned, scoped SQL expression that evaluates
#' a boolean concept against an object type. It is the portable representation
#' consumed by `conceptR`.
#'
#' @param id Character. Stable identifier, e.g. `"ready_for_discharge"`.
#' @param object_type_id Character. Which object type this applies to.
#' @param scope Character. Context bucket, e.g. `"clinical"`, `"operations"`.
#' @param version Integer. Monotonically increasing within `(id, scope)`.
#' @param sql_expr Character. SQL expression returning boolean.
#' @param status Character. One of `"draft"`, `"active"`, `"deprecated"`.
#'   Default `"draft"`.
#' @param rationale Character or NULL. Why this definition exists.
#' @param source_standard Character or NULL. E.g. `"ILO"`, `"WHO"`.
#' @param template_id Character or NULL. Parent template if inherited.
#' @param parameter_values Named list or NULL. Parameter overrides from template.
#' @param display_name Character or NULL.
#' @param display_description Character or NULL.
#' @param extensions Named list or NULL. Free-form extension data.
#' @return An S3 object of class `ontology_concept_def`.
#' @export
#' @examples
#' cd <- concept_def(
#'   id = "busy_airport",
#'   object_type_id = "Airport",
#'   scope = "operations",
#'   version = 1L,
#'   sql_expr = "daily_passengers > 50000"
#' )
concept_def <- function(
  id,
  object_type_id,
  scope,
  version,
  sql_expr,
  status           = "draft",
  rationale        = NULL,
  source_standard  = NULL,
  template_id      = NULL,
  parameter_values = NULL,
  display_name     = NULL,
  display_description = NULL,
  extensions       = NULL
) {
  assert_id(id, "id")
  assert_id(object_type_id, "object_type_id")
  assert_id(scope, "scope")

  if (!is.numeric(version) || length(version) != 1L || is.na(version) ||
      version < 1 || version != as.integer(version)) {
    stop("`version` must be a positive integer.", call. = FALSE)
  }

  assert_id(sql_expr, "sql_expr")
  assert_choice(status, c("draft", "active", "deprecated"), "status")

  structure(
    compact(list(
      id               = id,
      objectTypeId     = object_type_id,
      scope            = scope,
      version          = as.integer(version),
      sqlExpr          = sql_expr,
      status           = status,
      rationale        = rationale,
      sourceStandard   = source_standard,
      templateId       = template_id,
      parameterValues  = parameter_values,
      display          = build_display(display_name, description = display_description),
      extensions       = extensions
    )),
    class = "ontology_concept_def"
  )
}

#' Test if an object is a concept definition
#' @param x Object to test.
#' @return Logical.
#' @export
is_concept_def <- function(x) inherits(x, "ontology_concept_def")

#' @export
print.ontology_concept_def <- function(x, ...) {
  cat(sprintf(
    "<ConceptDef> %s v%d [%s] scope=%s on %s\n",
    x$id, x$version, x$status, x$scope, x$objectTypeId
  ))
  cat(sprintf("  sql: %s\n", x$sqlExpr))
  invisible(x)
}

#' @export
as_list.ontology_concept_def <- function(x, ...) {
  compact(list(
    id               = x$id,
    objectTypeId     = x$objectTypeId,
    scope            = x$scope,
    version          = x$version,
    sqlExpr          = x$sqlExpr,
    status           = x$status,
    rationale        = x$rationale,
    sourceStandard   = x$sourceStandard,
    templateId       = x$templateId,
    parameterValues  = x$parameterValues,
    display          = x$display,
    extensions       = x$extensions
  ))
}

# ---------------------------------------------------------------------------

#' Create a concept template definition
#'
#' A `concept_template_def` defines a parameterised SQL expression with
#' `{{param_name}}` placeholders. Concept definitions can inherit from a
#' template and supply concrete parameter values.
#'
#' @param id Character. Stable identifier.
#' @param object_type_id Character. Which object type this template applies to.
#' @param base_sql_expr Character. SQL expression with `{{param_name}}`
#'   placeholders.
#' @param parameters List of `ontology_parameter_def` objects describing each
#'   placeholder.
#' @param source_standard Character or NULL.
#' @param display_name Character or NULL.
#' @param display_description Character or NULL.
#' @param extensions Named list or NULL.
#' @return An S3 object of class `ontology_concept_template_def`.
#' @export
#' @examples
#' tmpl <- concept_template_def(
#'   id = "utilisation_threshold",
#'   object_type_id = "Airport",
#'   base_sql_expr = "utilisation_rate > {{threshold}}",
#'   parameters = list(
#'     parameter_def("threshold", "number", required = FALSE,
#'                   display_name = "Threshold",
#'                   display_description = "Utilisation rate threshold (default 0.8)")
#'   )
#' )
concept_template_def <- function(
  id,
  object_type_id,
  base_sql_expr,
  parameters       = list(),
  source_standard  = NULL,
  display_name     = NULL,
  display_description = NULL,
  extensions       = NULL
) {
  assert_id(id, "id")
  assert_id(object_type_id, "object_type_id")
  assert_id(base_sql_expr, "base_sql_expr")

  if (!is.list(parameters)) {
    stop("`parameters` must be a list.", call. = FALSE)
  }

  structure(
    compact(list(
      id             = id,
      objectTypeId   = object_type_id,
      baseSqlExpr    = base_sql_expr,
      parameters     = parameters,
      sourceStandard = source_standard,
      display        = build_display(display_name, description = display_description),
      extensions     = extensions
    )),
    class = "ontology_concept_template_def"
  )
}

#' Test if an object is a concept template definition
#' @param x Object to test.
#' @return Logical.
#' @export
is_concept_template_def <- function(x) inherits(x, "ontology_concept_template_def")

#' @export
print.ontology_concept_template_def <- function(x, ...) {
  cat(sprintf(
    "<ConceptTemplateDef> %s on %s (%d param(s))\n",
    x$id, x$objectTypeId, length(x$parameters)
  ))
  cat(sprintf("  base_sql: %s\n", x$baseSqlExpr))
  invisible(x)
}

#' @export
as_list.ontology_concept_template_def <- function(x, ...) {
  compact(list(
    id             = x$id,
    objectTypeId   = x$objectTypeId,
    baseSqlExpr    = x$baseSqlExpr,
    parameters     = lapply(x$parameters, as_list),
    sourceStandard = x$sourceStandard,
    display        = x$display,
    extensions     = x$extensions
  ))
}
