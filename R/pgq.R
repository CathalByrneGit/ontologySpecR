# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

# SQL single-quote escape (standard SQL: ' -> '')
sql_sq <- function(x) gsub("'", "''", x, fixed = TRUE)

# Resolve an object type's source table + PK source column.
# Returns list(table, pk_col) or NULL if source_table is not set.
ot_source_info <- function(ot) {
  tbl <- ot$source$table
  if (is.null(tbl)) return(NULL)
  pk_prop_id <- unlist(ot$primaryKey$properties)[[1L]]
  pk_col <- pk_prop_id
  for (p in ot$properties %||% list()) {
    if (p$id == pk_prop_id) {
      pk_col <- p$source$column %||% pk_prop_id
      break
    }
  }
  list(table = tbl, pk_col = pk_col)
}

# Map a DuckDB data type string to an ontology property type.
duck_type_to_ontology <- function(duck_type) {
  t <- toupper(trimws(duck_type))
  if (grepl("^(TINY|SMALL|INT|UBIG|BIGINT|HUGEINT|UINTEGER|USMALLINT|UTINYINT|INTEGER)",
            t)) "integer"
  else if (grepl("^(FLOAT|DOUBLE|DECIMAL|NUMERIC|REAL)", t)) "number"
  else if (grepl("^BOOL", t)) "boolean"
  else if (t == "DATE") "date"
  else if (grepl("^TIMESTAMP|^DATETIME", t)) "datetime"
  else if (grepl("^(JSON|STRUCT|MAP|LIST|ARRAY)", t)) "json"
  else "string"
}

# ---------------------------------------------------------------------------
# bundle_to_pgq_ddl()
# ---------------------------------------------------------------------------

#' Generate a CREATE PROPERTY GRAPH SQL statement from a bundle
#'
#' Translates the object types and link types in a bundle into DuckPGQ DDL.
#' Object types become vertex tables; link types with `source_table` set
#' become edge tables.
#'
#' @param bundle An `ontology_bundle`.
#' @param graph_name Character or NULL. Name for the property graph. Defaults
#'   to `bundle$bundleId` with hyphens replaced by underscores.
#' @param vertex_tables Character vector or NULL. Object type IDs to include.
#'   `NULL` includes all object types that have `source$table` set.
#' @param edge_tables Character vector or NULL. Link type IDs to include.
#'   `NULL` includes all link types that have `source$table`, `source$fromCol`,
#'   and `source$toCol` set.
#' @param replace Logical. Use `CREATE OR REPLACE PROPERTY GRAPH` (default
#'   `TRUE`).
#' @return A character string containing the SQL DDL statement.
#' @export
bundle_to_pgq_ddl <- function(bundle,
                               graph_name    = NULL,
                               vertex_tables = NULL,
                               edge_tables   = NULL,
                               replace       = TRUE) {
  if (!is_bundle(bundle)) stop("`bundle` must be an ontology_bundle.", call. = FALSE)

  # --- Graph name ---
  if (is.null(graph_name)) {
    graph_name <- gsub("-", "_", bundle$bundleId, fixed = TRUE)
  }
  if (!grepl("^[A-Za-z_][A-Za-z0-9_]*$", graph_name)) {
    stop("Graph name '", graph_name, "' is not a valid SQL identifier.",
         call. = FALSE)
  }

  # --- Index object types by id ---
  all_ots <- setNames(bundle$objects %||% list(),
                      vapply(bundle$objects %||% list(), function(o) o$id, character(1)))

  # --- Vertex tables ---
  vtx_candidates <- if (!is.null(vertex_tables)) {
    Filter(function(o) o$id %in% vertex_tables, bundle$objects %||% list())
  } else {
    bundle$objects %||% list()
  }

  vtx_rows <- list()
  for (ot in vtx_candidates) {
    inf <- ot_source_info(ot)
    if (is.null(inf)) {
      warning(sprintf(
        "Object type '%s' has no source$table — skipped in VERTEX TABLES.", ot$id
      ), call. = FALSE)
      next
    }
    vtx_rows <- c(vtx_rows, list(list(table = inf$table, label = ot$id)))
  }

  if (length(vtx_rows) == 0L) {
    stop("No object types with source$table set — cannot generate VERTEX TABLES.",
         call. = FALSE)
  }

  # --- Edge tables ---
  lnk_candidates <- if (!is.null(edge_tables)) {
    Filter(function(l) l$id %in% edge_tables, bundle$links %||% list())
  } else {
    bundle$links %||% list()
  }

  edge_entries <- list()
  for (lt in lnk_candidates) {
    src <- lt$source
    if (is.null(src) || is.null(src$table)) {
      warning(sprintf("Link type '%s' has no source$table — skipped.", lt$id),
              call. = FALSE)
      next
    }
    if (is.null(src$fromCol) || is.null(src$toCol)) {
      warning(sprintf(
        "Link type '%s' has source$table but missing source$fromCol or source$toCol — skipped.",
        lt$id
      ), call. = FALSE)
      next
    }

    from_ot <- all_ots[[lt$from]]
    to_ot   <- all_ots[[lt$to]]

    if (is.null(from_ot)) {
      warning(sprintf("Link type '%s': from object type '%s' not found — skipped.",
                      lt$id, lt$from), call. = FALSE)
      next
    }
    if (is.null(to_ot)) {
      warning(sprintf("Link type '%s': to object type '%s' not found — skipped.",
                      lt$id, lt$to), call. = FALSE)
      next
    }

    from_inf <- ot_source_info(from_ot)
    to_inf   <- ot_source_info(to_ot)

    if (is.null(from_inf)) {
      warning(sprintf("Link type '%s': from object type '%s' has no source$table — skipped.",
                      lt$id, lt$from), call. = FALSE)
      next
    }
    if (is.null(to_inf)) {
      warning(sprintf("Link type '%s': to object type '%s' has no source$table — skipped.",
                      lt$id, lt$to), call. = FALSE)
      next
    }

    where_clause <- ""
    if (!is.null(src$filterCol) && !is.null(src$filterVal)) {
      vals_sql <- paste(
        sprintf("'%s'", sql_sq(unlist(src$filterVal))),
        collapse = ", "
      )
      where_clause <- sprintf("\n      WHERE %s IN (%s)", src$filterCol, vals_sql)
    }

    entry <- sprintf(
      "    %s\n      SOURCE KEY (%s) REFERENCES %s (%s)\n      DESTINATION KEY (%s) REFERENCES %s (%s)%s\n      LABEL %s",
      src$table,
      src$fromCol, from_inf$table, from_inf$pk_col,
      src$toCol,   to_inf$table,   to_inf$pk_col,
      where_clause,
      lt$id
    )
    edge_entries <- c(edge_entries, list(entry))
  }

  # --- Assemble SQL ---
  create_kw <- if (replace) "CREATE OR REPLACE PROPERTY GRAPH" else "CREATE PROPERTY GRAPH"

  vtx_sql <- paste(
    vapply(vtx_rows, function(r) sprintf("    %s LABEL %s", r$table, r$label), character(1)),
    collapse = ",\n"
  )

  sql <- sprintf(
    "%s %s\n  VERTEX TABLES (\n%s\n  )",
    create_kw, graph_name, vtx_sql
  )

  if (length(edge_entries) > 0L) {
    edge_sql <- paste(edge_entries, collapse = ",\n")
    sql <- paste0(sql, sprintf("\n  EDGE TABLES (\n%s\n  )", edge_sql))
  } else {
    warning("No link types produced edge table entries — generating vertex-only graph.",
            call. = FALSE)
  }

  paste0(sql, ";")
}

# ---------------------------------------------------------------------------
# bundle_from_pgq_introspect()
# ---------------------------------------------------------------------------

#' Infer a bundle from an existing DuckPGQ property graph
#'
#' Reverse-engineers an `ontology_bundle` from a property graph already
#' loaded in a DuckDB connection. The result is draft quality and should be
#' reviewed before use in production.
#'
#' Requires the `DBI` package and a DuckDB connection with the `duckpgq`
#' extension loaded. The introspection queries target DuckPGQ catalog
#' table-valued functions; the exact function signatures may vary by duckpgq
#' version — adjust `vtx_sql` / `edge_sql` as needed.
#'
#' @param connection A DBI connection to DuckDB with `duckpgq` loaded.
#' @param graph_name Character. Name of the property graph to introspect.
#' @return A draft `ontology_bundle`.
#' @export
bundle_from_pgq_introspect <- function(connection, graph_name) {
  if (!requireNamespace("DBI", quietly = TRUE)) {
    stop("Package 'DBI' is required for bundle_from_pgq_introspect().",
         call. = FALSE)
  }
  if (!is.character(graph_name) || length(graph_name) != 1L || nchar(graph_name) == 0L) {
    stop("`graph_name` must be a non-empty character string.", call. = FALSE)
  }

  gn_esc <- sql_sq(graph_name)

  # -- Vertex tables --
  # DuckPGQ catalog TVFs (adjust names/columns for your duckpgq version)
  vtx_rows <- tryCatch(
    DBI::dbGetQuery(connection, sprintf(
      "SELECT label, table_name, primary_key
       FROM duckpgq_vertex_tables()
       WHERE graph_name = '%s'",
      gn_esc
    )),
    error = function(e) {
      stop(
        "Could not query DuckPGQ vertex catalog for graph '", graph_name, "'.\n",
        "Ensure duckpgq is installed/loaded and the graph exists.\n",
        "Error: ", conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # -- Edge tables --
  edge_rows <- tryCatch(
    DBI::dbGetQuery(connection, sprintf(
      "SELECT label, table_name, source_fk, destination_fk,
              filter_column, filter_value
       FROM duckpgq_edge_tables()
       WHERE graph_name = '%s'",
      gn_esc
    )),
    error = function(e) {
      warning("Could not query DuckPGQ edge catalog: ", conditionMessage(e),
              call. = FALSE)
      data.frame(
        label = character(0), table_name = character(0),
        source_fk = character(0), destination_fk = character(0),
        filter_column = character(0), filter_value = character(0),
        stringsAsFactors = FALSE
      )
    }
  )

  # -- Object types: one per vertex label --
  objects <- lapply(seq_len(nrow(vtx_rows)), function(i) {
    tbl   <- vtx_rows$table_name[[i]]
    label <- vtx_rows$label[[i]]
    pk    <- vtx_rows$primary_key[[i]]

    cols <- tryCatch(
      DBI::dbGetQuery(connection, sprintf(
        "SELECT column_name, data_type
         FROM information_schema.columns
         WHERE table_name = '%s'
         ORDER BY ordinal_position",
        sql_sq(tbl)
      )),
      error = function(e) {
        data.frame(column_name = character(0), data_type = character(0),
                   stringsAsFactors = FALSE)
      }
    )

    props <- if (nrow(cols) > 0L) {
      lapply(seq_len(nrow(cols)), function(j) {
        property_def(
          id            = cols$column_name[[j]],
          type          = duck_type_to_ontology(cols$data_type[[j]]),
          nullable      = TRUE,
          source_column = cols$column_name[[j]]
        )
      })
    } else {
      list()
    }

    pk_id <- if (!is.null(pk) && nchar(pk) > 0L) pk else {
      if (length(props) > 0L) props[[1L]]$id else "id"
    }

    object_type(
      id           = label,
      properties   = props,
      primary_key  = pk_id,
      source_kind  = "table",
      source_table = tbl
    )
  })

  # -- Link types: one per edge label --
  # Group edge_rows by label (multiple filter_value rows possible per label)
  edge_labels <- if (nrow(edge_rows) > 0L) unique(edge_rows$label) else character(0)

  links <- lapply(edge_labels, function(lbl) {
    rows <- edge_rows[edge_rows$label == lbl, , drop = FALSE]
    r    <- rows[1L, ]

    filter_vals <- if (!is.null(r$filter_column) && !is.na(r$filter_column) &&
                       nchar(r$filter_column) > 0L) {
      unique(rows$filter_value[!is.na(rows$filter_value)])
    } else NULL

    link_type(
      id                = lbl,
      from              = lbl,   # placeholder — caller should correct
      to                = lbl,
      source_table      = r$table_name,
      source_from_col   = r$source_fk,
      source_to_col     = r$destination_fk,
      source_filter_col = if (!is.null(filter_vals)) r$filter_column else NULL,
      source_filter_val = filter_vals
    )
  })

  bundle(
    bundle_id      = graph_name,
    bundle_version = "0.1.0",
    metadata = list(
      description = paste0(
        "Auto-generated from DuckPGQ property graph '", graph_name, "' on ",
        format(Sys.time(), "%Y-%m-%d"), ". Review before production use."
      )
    ),
    objects = objects,
    links   = links
  )
}
