#' Create an ontology bundle
#'
#' A bundle is the top-level container for an ontology specification,
#' holding all object types, link types, interfaces, action types, and
#' queries. Bundles can be serialized to JSON and validated against the
#' ontologySpecR JSON Schema.
#'
#' @param bundle_id Character. Stable identifier for this bundle.
#' @param bundle_version Character. SemVer for this bundle release.
#' @param spec_version Character. Schema version (default `"0.1.0"`).
#' @param objects List of `ontology_object_type` objects.
#' @param links List of `ontology_link_type` objects.
#' @param interfaces List of `ontology_interface_type` objects.
#' @param actions List of `ontology_action_type` objects.
#' @param queries List of `ontology_query_def` objects.
#' @param concepts List of `ontology_concept_def` objects.
#' @param templates List of `ontology_concept_template_def` objects.
#' @param metadata Named list or NULL. Bundle metadata (name, description,
#'   authors, tags).
#' @param extensions Named list or NULL. Free-form extension data.
#' @param validate Logical. If `TRUE`, call `validate_interfaces()` after
#'   construction and abort on any violation. Default `FALSE` so that bundles
#'   can be built incrementally without triggering premature errors.
#' @return An S3 object of class `ontology_bundle`.
#' @export
#' @examples
#' b <- bundle(
#'   bundle_id = "aviation-demo",
#'   bundle_version = "0.1.0",
#'   objects = list(
#'     object_type("Airport",
#'       properties = list(
#'         property_def("airport_id", "string", nullable = FALSE),
#'         property_def("name", "string")
#'       ),
#'       primary_key = "airport_id"
#'     )
#'   )
#' )
bundle <- function(bundle_id, bundle_version,
                   spec_version = "0.1.0",
                   objects = list(),
                   links = list(),
                   interfaces = list(),
                   actions = list(),
                   queries = list(),
                   concepts = list(),
                   templates = list(),
                   metadata = NULL,
                   extensions = NULL,
                   validate = FALSE) {
  assert_id(bundle_id, "bundle_id")

  b <- structure(
    compact(list(
      specVersion = spec_version,
      bundleId = bundle_id,
      bundleVersion = bundle_version,
      metadata = metadata,
      objects = objects,
      links = links,
      interfaces = interfaces,
      actions = actions,
      queries = queries,
      concepts = concepts,
      templates = templates,
      extensions = extensions
    )),
    class = "ontology_bundle"
  )

  if (validate) validate_interfaces(b, error = TRUE)

  b
}

#' Test if an object is an ontology bundle
#' @param x Object to test.
#' @return Logical.
#' @export
is_bundle <- function(x) inherits(x, "ontology_bundle")

#' @export
print.ontology_bundle <- function(x, ...) {
  cat(sprintf(
    "<OntologyBundle> %s v%s (spec %s)\n  %d objects, %d links, %d interfaces, %d actions, %d queries, %d concepts, %d templates\n",
    x$bundleId, x$bundleVersion, x$specVersion,
    length(x$objects), length(x$links), length(x$interfaces),
    length(x$actions), length(x$queries),
    length(x$concepts), length(x$templates)
  ))
  invisible(x)
}

#' @export
as_list.ontology_bundle <- function(x, ...) {
  compact(list(
    specVersion = x$specVersion,
    bundleId = x$bundleId,
    bundleVersion = x$bundleVersion,
    metadata = x$metadata,
    objects = lapply(x$objects, as_list),
    links = lapply(x$links, as_list),
    interfaces = lapply(x$interfaces, as_list),
    actions = lapply(x$actions, as_list),
    queries = lapply(x$queries, as_list),
    concepts = lapply(x$concepts, as_list),
    templates = lapply(x$templates, as_list),
    extensions = x$extensions
  ))
}

#' Validate all interface implementations in a bundle
#'
#' Checks that every object type which declares `implements` satisfies the
#' corresponding interface contracts (required properties, links, actions).
#'
#' @param bundle An `ontology_bundle`.
#' @param error Logical. If `TRUE` (default), abort with a formatted message
#'   when violations are found. If `FALSE`, return a named list mapping each
#'   violating object type id to its character vector of violation messages.
#'   An empty list means fully valid.
#' @return Named list of violations (empty = valid), or aborts when
#'   `error = TRUE` and violations exist.
#' @export
validate_interfaces <- function(bundle, error = TRUE) {
  if (!is_bundle(bundle)) {
    stop("`bundle` must be an ontology_bundle.", call. = FALSE)
  }

  if (length(bundle$interfaces) == 0 || length(bundle$objects) == 0) {
    return(invisible(list()))
  }

  ifaces <- setNames(
    bundle$interfaces,
    vapply(bundle$interfaces, function(i) i$id, character(1))
  )

  all_violations <- list()

  for (ot in bundle$objects) {
    impl_ids <- unlist(ot$implements %||% list())
    if (length(impl_ids) == 0) next

    ot_violations <- character(0)

    for (iface_id in impl_ids) {
      if (!iface_id %in% names(ifaces)) {
        ot_violations <- c(ot_violations, sprintf(
          "[%s] implements unknown interface '%s'", ot$id, iface_id
        ))
        next
      }
      v <- check_implements(ot, ifaces[[iface_id]], bundle = bundle, error = FALSE)
      if (!isTRUE(v)) ot_violations <- c(ot_violations, v)
    }

    if (length(ot_violations) > 0) all_violations[[ot$id]] <- ot_violations
  }

  if (length(all_violations) == 0) return(invisible(list()))

  if (error) {
    msgs <- unlist(all_violations, use.names = FALSE)
    stop(paste(c("Interface validation failed:", msgs), collapse = "\n  "),
         call. = FALSE)
  }

  all_violations
}
