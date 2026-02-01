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
#' @param metadata Named list or NULL. Bundle metadata (name, description,
#'   authors, tags).
#' @param extensions Named list or NULL. Free-form extension data.
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
                   metadata = NULL,
                   extensions = NULL) {
  assert_id(bundle_id, "bundle_id")

  structure(
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
      extensions = extensions
    )),
    class = "ontology_bundle"
  )
}

#' Test if an object is an ontology bundle
#' @param x Object to test.
#' @return Logical.
#' @export
is_bundle <- function(x) inherits(x, "ontology_bundle")

#' @export
print.ontology_bundle <- function(x, ...) {
  cat(sprintf(
    "<OntologyBundle> %s v%s (spec %s)\n  %d objects, %d links, %d interfaces, %d actions, %d queries\n",
    x$bundleId, x$bundleVersion, x$specVersion,
    length(x$objects), length(x$links), length(x$interfaces),
    length(x$actions), length(x$queries)
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
    extensions = x$extensions
  ))
}
