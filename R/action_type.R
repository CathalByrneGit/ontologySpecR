#' Create a parameter definition
#'
#' Defines a parameter for an action or query.
#'
#' @param id Character. Parameter identifier.
#' @param type Character. Data type.
#' @param required Logical. Whether the parameter is required (default `FALSE`).
#' @param display_name Character or NULL. Display name.
#' @param display_description Character or NULL. Description.
#' @return An S3 object of class `ontology_parameter_def`.
#' @export
parameter_def <- function(id, type, required = FALSE,
                          display_name = NULL, display_description = NULL) {
  assert_id(id, "id")
  valid_types <- c("string", "integer", "number", "boolean", "date", "datetime", "json")
  assert_choice(type, valid_types, "type")

  structure(
    compact(list(
      id = id,
      type = type,
      required = required,
      display = build_display(name = display_name, description = display_description)
    )),
    class = "ontology_parameter_def"
  )
}

#' Test if an object is a parameter definition
#' @param x Object to test.
#' @return Logical.
#' @export
is_parameter_def <- function(x) inherits(x, "ontology_parameter_def")

#' @export
print.ontology_parameter_def <- function(x, ...) {
  req <- if (isTRUE(x$required)) " (required)" else ""
  cat(sprintf("<ParameterDef> %s : %s%s\n", x$id, x$type, req))
  invisible(x)
}

#' @export
as_list.ontology_parameter_def <- function(x, ...) {
  compact(list(
    id = x$id,
    type = x$type,
    required = x$required,
    display = x$display
  ))
}

#' Create an effect definition
#'
#' Describes what an action changes at a high level.
#'
#' @param kind Character. Effect kind: `"create"`, `"update"`, `"delete"`,
#'   or `"emit"`.
#' @param object_type_id Character or NULL. Target object type ID.
#' @param notes Character or NULL. Additional notes.
#' @return An S3 object of class `ontology_effect_def`.
#' @export
effect_def <- function(kind, object_type_id = NULL, notes = NULL) {
  assert_choice(kind, c("create", "update", "delete", "emit"), "kind")

  structure(
    compact(list(
      kind = kind,
      objectTypeId = object_type_id,
      notes = notes
    )),
    class = "ontology_effect_def"
  )
}

#' Test if an object is an effect definition
#' @param x Object to test.
#' @return Logical.
#' @export
is_effect_def <- function(x) inherits(x, "ontology_effect_def")

#' @export
print.ontology_effect_def <- function(x, ...) {
  target <- if (!is.null(x$objectTypeId)) paste0(" on ", x$objectTypeId) else ""
  cat(sprintf("<EffectDef> %s%s\n", x$kind, target))
  invisible(x)
}

#' @export
as_list.ontology_effect_def <- function(x, ...) {
  compact(list(
    kind = x$kind,
    objectTypeId = x$objectTypeId,
    notes = x$notes
  ))
}

#' Create an action type definition
#'
#' Defines an action that can be performed on object types, including
#' parameters, effects, implementation details, and policy hooks.
#'
#' @param id Character. Unique identifier for this action type.
#' @param targets Character vector. Object type IDs this action operates on.
#' @param parameters List of `ontology_parameter_def` objects.
#' @param display_name Character or NULL. Human-readable name.
#' @param display_description Character or NULL. Description.
#' @param effects List of `ontology_effect_def` objects.
#' @param impl_kind Character or NULL. Implementation kind: `"r"`, `"sql"`,
#'   `"http"`, `"plugin"`.
#' @param impl_entrypoint Character or NULL. Entry point identifier.
#' @param policy Named list or NULL. Policy configuration.
#' @param extensions Named list or NULL. Free-form extension data.
#' @return An S3 object of class `ontology_action_type`.
#' @export
#' @examples
#' action_type(
#'   id = "UpdateAirportStatus",
#'   targets = "Airport",
#'   parameters = list(
#'     parameter_def("new_status", "string", required = TRUE)
#'   ),
#'   effects = list(
#'     effect_def("update", "Airport")
#'   ),
#'   impl_kind = "r",
#'   impl_entrypoint = "update_airport_status"
#' )
action_type <- function(id, targets, parameters = list(),
                        display_name = NULL, display_description = NULL,
                        effects = list(),
                        impl_kind = NULL, impl_entrypoint = NULL,
                        policy = NULL,
                        extensions = NULL) {
  assert_id(id, "id")
  if (!is.character(targets) || length(targets) == 0) {
    stop("`targets` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.null(impl_kind)) {
    assert_choice(impl_kind, c("r", "sql", "http", "plugin"), "impl_kind")
  }

  implementation <- NULL
  if (!is.null(impl_kind)) {
    implementation <- compact(list(kind = impl_kind, entrypoint = impl_entrypoint))
  }

  structure(
    compact(list(
      id = id,
      display = build_display(name = display_name, description = display_description),
      targets = as.list(targets),
      parameters = if (length(parameters) > 0) parameters else list(),
      effects = if (length(effects) > 0) effects else NULL,
      implementation = implementation,
      policy = policy,
      extensions = extensions
    )),
    class = "ontology_action_type"
  )
}

#' Test if an object is an action type
#' @param x Object to test.
#' @return Logical.
#' @export
is_action_type <- function(x) inherits(x, "ontology_action_type")

#' @export
print.ontology_action_type <- function(x, ...) {
  targets <- paste(x$targets, collapse = ", ")
  n_params <- length(x$parameters)
  cat(sprintf("<ActionType> %s -> [%s] (%d params)\n",
              x$id, targets, n_params))
  invisible(x)
}

#' @export
as_list.ontology_action_type <- function(x, ...) {
  compact(list(
    id = x$id,
    display = x$display,
    targets = x$targets,
    parameters = lapply(x$parameters, as_list),
    effects = if (!is.null(x$effects)) lapply(x$effects, as_list) else NULL,
    implementation = x$implementation,
    policy = x$policy,
    extensions = x$extensions
  ))
}
