#' Create a property requirement for an interface
#'
#' Specifies a property that conforming object types must provide.
#'
#' @param id Character. Property identifier.
#' @param type Character. Required data type.
#' @param nullable Logical. Whether NULL is allowed.
#' @return An S3 object of class `ontology_property_requirement`.
#' @export
property_requirement <- function(id, type, nullable = TRUE) {
  assert_id(id, "id")
  valid_types <- c("string", "integer", "number", "boolean", "date", "datetime", "json")
  assert_choice(type, valid_types, "type")

  structure(
    list(id = id, type = type, nullable = nullable),
    class = "ontology_property_requirement"
  )
}

#' Test if an object is a property requirement
#' @param x Object to test.
#' @return Logical.
#' @export
is_property_requirement <- function(x) inherits(x, "ontology_property_requirement")

#' @export
print.ontology_property_requirement <- function(x, ...) {

  cat(sprintf("<PropertyRequirement> %s : %s\n", x$id, x$type))
  invisible(x)
}

#' @export
as_list.ontology_property_requirement <- function(x, ...) {
  compact(list(id = x$id, type = x$type, nullable = x$nullable))
}

#' Create a link requirement for an interface
#'
#' Specifies a link type that conforming object types must participate in.
#'
#' @param link_type_id Character. Required link type ID.
#' @param min_count Integer. Minimum number of linked objects (default 0).
#' @param max_count Integer or NULL. Maximum number (NULL = unlimited).
#' @return An S3 object of class `ontology_link_requirement`.
#' @export
link_requirement <- function(link_type_id, min_count = 0L, max_count = NULL) {
  assert_id(link_type_id, "link_type_id")

  structure(
    compact(list(
      linkTypeId = link_type_id,
      minCount = as.integer(min_count),
      maxCount = if (!is.null(max_count)) as.integer(max_count) else NULL
    )),
    class = "ontology_link_requirement"
  )
}

#' Test if an object is a link requirement
#' @param x Object to test.
#' @return Logical.
#' @export
is_link_requirement <- function(x) inherits(x, "ontology_link_requirement")

#' @export
print.ontology_link_requirement <- function(x, ...) {
  max_str <- if (is.null(x$maxCount)) "Inf" else x$maxCount
  cat(sprintf("<LinkRequirement> %s [%d, %s]\n",
              x$linkTypeId, x$minCount, max_str))
  invisible(x)
}

#' @export
as_list.ontology_link_requirement <- function(x, ...) {
  compact(list(
    linkTypeId = x$linkTypeId,
    minCount = x$minCount,
    maxCount = x$maxCount
  ))
}

#' Check that an object type satisfies an interface
#'
#' Validates that an `ontology_object_type` provides all properties, and
#' (when a bundle is supplied) all link types and action types declared as
#' required by an `ontology_interface_type`.
#'
#' @param object_type An `ontology_object_type`.
#' @param interface An `ontology_interface_type`.
#' @param bundle An `ontology_bundle` or NULL. Required for link and action
#'   checks; those checks are skipped when NULL.
#' @param error Logical. If `TRUE` (default), abort on the first violation.
#'   If `FALSE`, return a character vector of violation messages (empty = valid).
#' @return `TRUE` invisibly on success, or a character vector of violation
#'   messages when `error = FALSE`.
#' @export
check_implements <- function(object_type, interface, bundle = NULL, error = TRUE) {
  if (!is_object_type(object_type)) {
    stop("`object_type` must be an ontology_object_type.", call. = FALSE)
  }
  if (!is_interface_type(interface)) {
    stop("`interface` must be an ontology_interface_type.", call. = FALSE)
  }

  violations <- character(0)
  ot_id <- object_type$id

  # --- Property requirements ---
  props      <- object_type$properties %||% list()
  prop_ids   <- vapply(props, function(p) p$id,       character(1))
  prop_types <- vapply(props, function(p) p$type,     character(1))
  prop_null  <- vapply(props, function(p) isTRUE(p$nullable), logical(1))
  names(prop_types) <- prop_ids
  names(prop_null)  <- prop_ids

  for (req in interface$requiredProperties %||% list()) {
    if (!(req$id %in% prop_ids)) {
      violations <- c(violations, sprintf(
        "[%s] missing required property '%s' (type: %s)",
        ot_id, req$id, req$type
      ))
      next
    }
    if (prop_types[[req$id]] != req$type) {
      violations <- c(violations, sprintf(
        "[%s] property '%s' has type '%s' but interface requires '%s'",
        ot_id, req$id, prop_types[[req$id]], req$type
      ))
    }
    if (!isTRUE(req$nullable) && isTRUE(prop_null[[req$id]])) {
      violations <- c(violations, sprintf(
        "[%s] property '%s' is nullable but interface '%s' requires non-nullable",
        ot_id, req$id, interface$id
      ))
    }
  }

  # --- Link and action requirements (bundle-level) ---
  if (!is.null(bundle)) {
    link_ids   <- vapply(bundle$links   %||% list(), function(l) l$id, character(1))
    action_ids <- vapply(bundle$actions %||% list(), function(a) a$id, character(1))

    for (req in interface$requiredLinks %||% list()) {
      if (!(req$linkTypeId %in% link_ids)) {
        violations <- c(violations, sprintf(
          "[%s] missing required link type '%s'",
          ot_id, req$linkTypeId
        ))
      }
    }

    for (act_id in unlist(interface$requiredActions %||% list())) {
      if (!(act_id %in% action_ids)) {
        violations <- c(violations, sprintf(
          "[%s] missing required action type '%s'",
          ot_id, act_id
        ))
      }
    }
  }

  if (length(violations) == 0) return(invisible(TRUE))

  if (error) {
    stop(paste(violations, collapse = "\n"), call. = FALSE)
  }
  violations
}

#'
#' Defines a polymorphic interface ("shape contract") that object types
#' can implement. Specifies required properties, links, and actions.
#'
#' @param id Character. Unique identifier for this interface.
#' @param display_name Character or NULL. Human-readable name.
#' @param display_description Character or NULL. Description.
#' @param required_properties List of `ontology_property_requirement` objects.
#' @param required_links List of `ontology_link_requirement` objects.
#' @param required_actions Character vector. Action type IDs that must be
#'   available.
#' @param extensions Named list or NULL. Free-form extension data.
#' @return An S3 object of class `ontology_interface_type`.
#' @export
#' @examples
#' interface_type(
#'   id = "GeoLocated",
#'   display_name = "Geo-Located",
#'   required_properties = list(
#'     property_requirement("latitude", "number"),
#'     property_requirement("longitude", "number")
#'   )
#' )
interface_type <- function(id,
                           display_name = NULL, display_description = NULL,
                           required_properties = list(),
                           required_links = list(),
                           required_actions = character(0),
                           extensions = NULL) {
  assert_id(id, "id")

  structure(
    compact(list(
      id = id,
      display = build_display(name = display_name, description = display_description),
      requiredProperties = if (length(required_properties) > 0) required_properties else NULL,
      requiredLinks = if (length(required_links) > 0) required_links else NULL,
      requiredActions = if (length(required_actions) > 0) as.list(required_actions) else NULL,
      extensions = extensions
    )),
    class = "ontology_interface_type"
  )
}

#' Test if an object is an interface type
#' @param x Object to test.
#' @return Logical.
#' @export
is_interface_type <- function(x) inherits(x, "ontology_interface_type")

#' @export
print.ontology_interface_type <- function(x, ...) {
  n_props <- length(x$requiredProperties)
  n_links <- length(x$requiredLinks)
  cat(sprintf("<InterfaceType> %s (%d required props, %d required links)\n",
              x$id, n_props, n_links))
  invisible(x)
}

#' @export
as_list.ontology_interface_type <- function(x, ...) {
  compact(list(
    id = x$id,
    display = x$display,
    requiredProperties = if (!is.null(x$requiredProperties)) {
      lapply(x$requiredProperties, as_list)
    },
    requiredLinks = if (!is.null(x$requiredLinks)) {
      lapply(x$requiredLinks, as_list)
    },
    requiredActions = x$requiredActions,
    extensions = x$extensions
  ))
}
