set_par <- function(con, name, values) {
  values <- as.character(values)
  sql <- paste0("DELETE FROM par WHERE name = ?")
  DBI::dbExecute(con, sql, params = list(name))
  name <- rep(name, length(values))
  sql <- "INSERT INTO par (name, value, idx) VALUES (?, ?, ?)"
  DBI::dbExecute(con, sql, params = list(name, values, seq_along(values)))
}

get_par <- function(con) {
  sql <- "SELECT name, value FROM par ORDER BY name, idx"
  df <- DBI::dbGetQuery(con, sql)
  par <- df |>
    dplyr::summarize(value = list(.data$value), .by = "name") |>
    tibble::deframe()
  par$reset_seed <- as.logical(par$reset_seed)
  par
}

check_reserved_columns <- function(df, reserved) {
  if (any(reserved %in% names(df))) {
    stop("`df` cannot contain column(s) '", paste(reserved, collapse = "', '"),
         "'.", call. = FALSE)
  }
  invisible(TRUE)
}

check_foreign_keys <- function(df, key_list) {
  purrr::iwalk(key_list, function(keys, key_col) {
    key_table <- sub("_id$", "", key_col)
    if (!is.character(keys) || length(keys) == 0) {
      stop("`", key_table, "` must be a character vector.", call. = FALSE)
    }
    if (!all(keys %in% names(df))) {
      stop("Each `", key_table, "` value must be a column name in `df`.",
           call. = FALSE)
    }
    if (!is.null(names(keys)) && any(names(keys) == "")) {
      stop("If `", key_table, "` is a named character vector, ",
           "then all values must be named.", call. = FALSE)
    }
  })
  invisible(TRUE)
}

#=======================================
# Begin RNG state preservation
#=======================================
# These functions will preserve the user's RNG state for the purpose of
# replication of previous GeoTox results. When performing the following tasks,
# temporary tables are created by the DBI implementation where 10 random
# characters are appended to the table name. To prevent this from affecting the
# user's RNG state, the .Random.seed is reset and any new temporary tables are
# removed in case the functions are called multiple times in a row.

write_table <- function(
    con, table, df, overwrite = FALSE, field.types = NULL, reset_seed = FALSE
) {
  if (reset_seed) seed <- .Random.seed
  DBI::dbWriteTable(
    con, table, df, overwrite = overwrite, field.types = field.types
  )
  if (reset_seed) .Random.seed <<- seed
}

append_table <- function(con, table, df, reset_seed = FALSE) {
  if (reset_seed) seed <- .Random.seed
  DBI::dbAppendTable(con, table, df)
  if (reset_seed) .Random.seed <<- seed
}

update_table <- function(
    con, table, df, by = "id", copy = TRUE, reset_seed = FALSE
) {
  if (reset_seed) {
    seed <- .Random.seed
    tbls <- DBI::dbListTables(con)
  }
  dplyr::rows_update(
    dplyr::tbl(con, table),
    df,
    by = by,
    in_place = TRUE,
    unmatched = "ignore",
    copy = copy
  )
  if (reset_seed) {
    rm_tbl <- setdiff(DBI::dbListTables(con), tbls)
    if (length(rm_tbl) == 1) {
      DBI::dbRemoveTable(con, rm_tbl)
    }
    .Random.seed <<- seed
  }
}
#=======================================
# End RNG state preservation
#=======================================

fetch_foreign_keys <- function(con, df, foreign_keys, reset_seed) {
  purrr::iwalk(foreign_keys, function(keys, key_col) {
    key_table <- sub("_id$", "", key_col)
    # Add any new data to foreign key table. Even if the table already exists
    # and no new data is being added, calling add_table is simple and we don't
    # care about efficiency in these cases.
    key_df <- df |> dplyr::select(tidyselect::all_of(keys)) |> dplyr::distinct()
    # Use key_names to map the foreign key columns back to the main table
    if (!is.null(names(keys))) {
      key_df <- stats::setNames(key_df, names(keys))
      key_names <- stats::setNames(names(keys), keys)
    } else {
      key_names <- stats::setNames(keys, keys)
    }
    # The foreign key tables will not have foreign keys themselves, so we can
    # recursively call add_table with foreign_keys = NULL.
    add_table(con, key_table, key_df, reset_seed = reset_seed)
    # Get foreign key IDs
    key_df <- DBI::dbReadTable(con, key_table) |>
      dplyr::select(tidyselect::all_of(c("id", unname(key_names))))
    # Add foreign key IDs to the main table
    df <<- df |>
      dplyr::left_join(key_df, by = key_names) |>
      dplyr::select(-tidyselect::all_of(names(key_names))) |>
      dplyr::rename(tidyselect::all_of(stats::setNames("id", key_col)))
  })
  df
}

overwrite_table <- function(con, table, df, foreign_keys, reset_seed = FALSE) {
  key_cols <- c("id", names(foreign_keys))
  df <- df |>
    # Add primary key
    dplyr::mutate(id = dplyr::row_number()) |>
    # Reorder columns
    dplyr::select(tidyselect::all_of(c(key_cols, setdiff(names(df), key_cols))))
  # Write the table and add key constraints
  write_table(con, table, df, overwrite = TRUE, reset_seed = reset_seed)
  # sql <- paste("ALTER TABLE", table, "ADD PRIMARY KEY (id)")
  # DBI::dbExecute(con, sql)
  # purrr::walk(names(foreign_keys), function(key_col) {
  #   # Adding foreign keys via ALTER TABLE is not implemented yet in duckdb,
  #   # instead, just ensure they aren't NULL.
  #   sql <- paste("ALTER TABLE", table, "ALTER", key_col, "SET NOT NULL")
  #   DBI::dbExecute(con, sql)
  # })
}

add_table <- function(
    con, table, df, foreign_keys = NULL, distinct = TRUE, overwrite = FALSE,
    reset_seed = FALSE
) {
  reserved <- c("id", names(foreign_keys))
  check_reserved_columns(df, reserved)

  # Add foreign keys
  if (!is.null(foreign_keys)) {
    check_foreign_keys(df, foreign_keys)
    df <- fetch_foreign_keys(con, df, foreign_keys, reset_seed)
  }

  if (distinct) {
    df <- df |> dplyr::distinct()
  }

  if (!DBI::dbExistsTable(con, table) | overwrite) {
    overwrite_table(con, table, df, foreign_keys, reset_seed = reset_seed)
  } else {
    # Load current table
    tbl <- DBI::dbReadTable(con, table)
    # Add new columns to the existing table
    cols <- setdiff(names(df), names(tbl))
    if (length(cols) > 0) {
      for (col in cols) {
        if (is.integer(df[[col]])) {
          type <- "INTEGER"
        } else if (is.numeric(df[[col]])) {
          type <- "DOUBLE"
        } else {
          type <- "TEXT"
        }
        sql <- paste("ALTER TABLE", table, "ADD COLUMN", col, type)
        DBI::dbExecute(con, sql)
      }
      # refresh table
      tbl <- DBI::dbReadTable(con, table)
    }
    # Add new data
    # Exclude new "cols" from the join. Give them a suffix of ".new" so they
    # can be removed easily after the join.
    if (length(cols) > 0) {
      by_cols <- intersect(names(df), setdiff(names(tbl), cols))
      if (length(by_cols) > 0) {
        df <- dplyr::full_join(
          df,
          tbl,
          by = by_cols,
          suffix = c("", ".new")
        ) |>
          dplyr::select(-tidyselect::all_of(paste0(cols, ".new")))
      } else {
        # If no overlapping columns, assume all are new rows
        df <- dplyr::full_join(df, tbl, by = intersect(names(df), names(tbl)))
      }
    } else {
      df <- dplyr::full_join(df, tbl, by = intersect(names(df), names(tbl)))
    }
    # Add new column data to existing rows
    if (length(cols) > 0) {
      params <- df |>
        dplyr::filter(!is.na(.data$id)) |>
        dplyr::select(tidyselect::all_of(c("id", cols)))
      if (nrow(params) > 0) {
        update_table(con, table, params, reset_seed = reset_seed)
      }
    }
    # New rows
    params <- df |> dplyr::filter(is.na(.data$id))
    if (nrow(params) > 0) {
      # Assign new IDs
      if (nrow(tbl) == 0) {
        max_id <- 0
      } else {
        max_id <- max(tbl$id)
      }
      params <- params |> dplyr::mutate(id = dplyr::row_number() + max_id)
      # Append new rows to DB
      append_table(con, table, params, reset_seed = reset_seed)
    }
  }
}
