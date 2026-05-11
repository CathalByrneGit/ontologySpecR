# --- Internal helpers -------------------------------------------------------

#' @keywords internal
diff_primitive_list <- function(base_list, new_list) {
  base_ids <- vapply(base_list, function(x) x$id, character(1))
  new_ids  <- vapply(new_list,  function(x) x$id, character(1))

  base_by_id <- setNames(base_list, base_ids)
  new_by_id  <- setNames(new_list,  new_ids)

  added   <- lapply(setdiff(new_ids,  base_ids), function(id) new_by_id[[id]])
  removed <- lapply(setdiff(base_ids, new_ids),  function(id) base_by_id[[id]])

  modified <- list()
  for (id in intersect(base_ids, new_ids)) {
    b <- base_by_id[[id]]
    n <- new_by_id[[id]]
    if (!identical(as_list(b), as_list(n))) {
      modified <- c(modified, list(list(base = b, new = n)))
    }
  }

  list(added = added, removed = removed, modified = modified)
}

# --- Public API -------------------------------------------------------------

#' Compute a structural diff between two bundles
#'
#' Compares `new` against `base`, identifying added, removed, and modified
#' ontology primitives across all primitive types (object types, link types,
#' interfaces, action types, queries, concepts, templates).
#'
#' @param base An `ontology_bundle` — the reference (e.g. current production).
#' @param new  An `ontology_bundle` — the proposed change (e.g. a feature branch).
#' @return An S3 object of class `ontology_bundle_diff`. Each top-level field
#'   corresponds to a primitive type and contains `$added`, `$removed`, and
#'   `$modified` sub-lists. `$modified` entries are lists with `$base` and
#'   `$new` holding the before/after primitive.
#' @export
bundle_diff <- function(base, new) {
  if (!is_bundle(base)) stop("`base` must be an ontology_bundle.", call. = FALSE)
  if (!is_bundle(new))  stop("`new` must be an ontology_bundle.",  call. = FALSE)

  structure(
    list(
      objects    = diff_primitive_list(base$objects    %||% list(), new$objects    %||% list()),
      links      = diff_primitive_list(base$links      %||% list(), new$links      %||% list()),
      interfaces = diff_primitive_list(base$interfaces %||% list(), new$interfaces %||% list()),
      actions    = diff_primitive_list(base$actions    %||% list(), new$actions    %||% list()),
      queries    = diff_primitive_list(base$queries    %||% list(), new$queries    %||% list()),
      concepts   = diff_primitive_list(base$concepts   %||% list(), new$concepts   %||% list()),
      templates  = diff_primitive_list(base$templates  %||% list(), new$templates  %||% list())
    ),
    class = "ontology_bundle_diff"
  )
}

#' Test whether a diff contains no changes
#'
#' @param d An `ontology_bundle_diff` from [bundle_diff()].
#' @return Logical scalar.
#' @export
is_empty_diff <- function(d) {
  for (type_diff in d) {
    if (length(type_diff$added)    > 0L ||
        length(type_diff$removed)  > 0L ||
        length(type_diff$modified) > 0L) return(FALSE)
  }
  TRUE
}

#' @export
print.ontology_bundle_diff <- function(x, ...) {
  cat("<BundleDiff>\n")
  types <- c("objects", "links", "interfaces", "actions",
             "queries", "concepts", "templates")
  for (type in types) {
    d   <- x[[type]]
    na  <- length(d$added)
    nr  <- length(d$removed)
    nm  <- length(d$modified)
    lbl <- sprintf("  %-12s", paste0(type, ":"))
    if (na == 0L && nr == 0L && nm == 0L) {
      cat(lbl, "no changes\n")
    } else {
      cat(sprintf("%s+%d added, %d removed, %d modified\n", lbl, na, nr, nm))
    }
  }
  invisible(x)
}

#' Apply a diff to a bundle
#'
#' Produces a new bundle by applying the additions, removals, and modifications
#' recorded in `diff` to `base`. Equivalent to a forward merge.
#'
#' @param base An `ontology_bundle`.
#' @param diff An `ontology_bundle_diff` produced by [bundle_diff()].
#' @return A new `ontology_bundle` (`base` is not mutated).
#' @export
bundle_apply_diff <- function(base, diff) {
  if (!is_bundle(base)) stop("`base` must be an ontology_bundle.", call. = FALSE)
  if (!inherits(diff, "ontology_bundle_diff")) {
    stop("`diff` must be an ontology_bundle_diff.", call. = FALSE)
  }

  apply_type_diff <- function(base_list, type_diff) {
    removed_ids  <- vapply(type_diff$removed,  function(x) x$id,     character(1))
    modified_ids <- vapply(type_diff$modified, function(x) x$new$id, character(1))
    drop_ids     <- c(removed_ids, modified_ids)

    kept     <- Filter(function(x) !x$id %in% drop_ids, base_list)
    replaced <- lapply(type_diff$modified, function(x) x$new)

    c(kept, replaced, type_diff$added)
  }

  bundle(
    bundle_id      = base$bundleId,
    bundle_version = base$bundleVersion,
    spec_version   = base$specVersion   %||% "0.1.0",
    metadata       = base$metadata,
    objects        = apply_type_diff(base$objects    %||% list(), diff$objects),
    links          = apply_type_diff(base$links      %||% list(), diff$links),
    interfaces     = apply_type_diff(base$interfaces %||% list(), diff$interfaces),
    actions        = apply_type_diff(base$actions    %||% list(), diff$actions),
    queries        = apply_type_diff(base$queries    %||% list(), diff$queries),
    concepts       = apply_type_diff(base$concepts   %||% list(), diff$concepts),
    templates      = apply_type_diff(base$templates  %||% list(), diff$templates),
    extensions     = base$extensions
  )
}

#' Merge two bundles
#'
#' Convenience wrapper: computes `bundle_diff(base, new)` then applies it with
#' [bundle_apply_diff()]. The result contains all primitives from `new`,
#' preserving the `bundleId` and metadata of `base`.
#'
#' Note: only two-way merges are supported. If the same primitive has been
#' independently modified in two branches diverging from a common ancestor, a
#' three-way merge is required — call [bundle_diff()] and [bundle_apply_diff()]
#' directly and resolve conflicts manually.
#'
#' @param base An `ontology_bundle`.
#' @param new  An `ontology_bundle`.
#' @return A new `ontology_bundle`.
#' @export
bundle_merge <- function(base, new) {
  if (!is_bundle(base)) stop("`base` must be an ontology_bundle.", call. = FALSE)
  if (!is_bundle(new))  stop("`new` must be an ontology_bundle.",  call. = FALSE)
  bundle_apply_diff(base, bundle_diff(base, new))
}
