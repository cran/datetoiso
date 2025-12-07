#' @importFrom magrittr %>%
NULL
#' Harmonizes variable classes between two data frames
#'
#' This function aligns the classes of variables in `old_df` with those of
#' their corresponding variables in `new_df`. For each column name shared
#' across both data frames, the function detects the class in `new_df` and
#' coerces the matching column in `old_df` to the same class. Supported
#' conversions include `character`, `numeric`, `integer`, `logical`, `factor`,
#' and `Date`. Any variable whose class is not explicitly handled is left
#' unchanged.
#'
#' @param new_df A data frame containing the most recent version of the data.
#' @param old_df A data frame containing the preceding version of the data,
#'   used as the reference for comparison.
#' @author Lukasz Andrzejewski
#'
#' @return A modified version of `old_df` in which all shared columns are coerced to match the variable classes of `new_df`.
get_same_class <- function(old_df, new_df) {

  common_names <- intersect(names(new_df), names(old_df))

  old_df[common_names] <- sapply(common_names, function(col_name) {
    source_class <- class(new_df[[col_name]])

    switch(
      source_class,
      "character" = as.character(old_df[[col_name]]),
      "numeric"   = as.numeric(old_df[[col_name]]),
      "integer"   = as.integer(old_df[[col_name]]),
      "logical"   = as.logical(old_df[[col_name]]),
      "factor"    = as.factor(old_df[[col_name]]),
      "Date"      = as.Date(old_df[[col_name]]),
      old_df[[col_name]]
    )
  }, simplify = FALSE)

  return(old_df)
}


#' Identifies and summarizes value-level changes between two datasets
#'
#' This function compares corresponding rows of two data frames and generates
#' two columns:
#'
#' * **changed_cols** — a list-column containing the names of variables in which
#'   at least one change has been detected.
#' * **change_details** — a list-column describing the specific modifications for
#'   each changed variable, expressed as "previous value - new value".
#'
#' @param new_df A data frame containing the most recent version of the data.
#' @param old_df A data frame containing the preceding version of the data,
#'   used as the reference for comparison.
#' @author Lukasz Andrzejewski
#'
#' @return A data frame augmented with two additional columns:
#'   `changed_cols` and `change_details`
compare_rows_with_same_index <- function(new_df, old_df) {
  if (nrow(new_df) != nrow(old_df)) {
    stop("new_df and old_df must have the same number of rows for row-wise comparison")
  }

  col_names <- colnames(new_df)

  new_df %>%
    dplyr::mutate(
      changed_cols = purrr::pmap_chr(
        c(new_df, old_df),
        function(...) {
          values <- list(...)
          n <- length(values) / 2
          new_vals <- values[1:n]
          old_vals <- values[(n + 1):(2 * n)]

          changed <- mapply(
            function(new, old) if (is.na(new) && is.na(old)) FALSE else !identical(new, old),
            new_vals, old_vals
          )

          changed_cols_row <- col_names[changed]
          if (length(changed_cols_row) == 0) "" else paste(changed_cols_row, collapse = ", ")
        }
      ),

      change_details = purrr::pmap_chr(
        c(new_df, old_df),
        function(...) {
          values <- list(...)
          n <- length(values) / 2
          new_vals <- values[1:n]
          old_vals <- values[(n + 1):(2 * n)]

          details <- mapply(
            function(new, old, col) {
              if (is.na(new) && is.na(old)) return(NULL)
              if (!identical(new, old)) paste0(col, ": ", old, " -> ", new) else NULL
            },
            new_vals, old_vals, col_names,
            SIMPLIFY = FALSE
          )

          details <- Filter(Negate(is.null), details)
          if (length(details) == 0) "" else paste(details, collapse = ", ")
        }
      )
    )
}


#' Reconcile Two Data Frames
#'
#' This function compares two data frames —`new_df` (the updated version) and
#' `old_df` (the previous version) —to identify differences between them.
#' The comparison can be performed across all shared columns or restricted
#' to a specified subset of columns.
#'
#' @param new_df A data frame containing the most recent version of the data.
#' @param old_df A data frame containing the preceding version of the data,
#'   used as the reference for comparison.
#' @param lookup_columns A character vector specifying which columns should be
#'   used for comparison. By default `NA`, meaning that all columns common to
#'   both `new_df` and `old_df` are included. If one or more column names are
#'   provided, only those columns will be compared.
#' @author Lukasz Andrzejewski
#'
#' @return A data frame summarizing differences between `new_df` and `old_df`,
#' including which columns changed and the details of those changes.
reconcile_without_index <- function(old_df, new_df, lookup_columns = NA) {

  old_df_same_class <- get_same_class(old_df = old_df, new_df = new_df)
  names_old <- names(old_df)
  names_new <- names(new_df)

  if(all(is.na(lookup_columns))){
    common_names <- names_new[names_new %in% names_old]
  } else {
    common_names <- lookup_columns
  }

  new_variable <- names_new[!names_new %in% names_old]
  if (length(new_variable) == 0) new_variable <- NA
  old_variable <- names_old[!names_old %in% names_new]
  if (length(old_variable) == 0) old_variable <- NA


  new_or_changed_records <- new_df %>% dplyr::anti_join(old_df_same_class, by = common_names) %>%
    dplyr::mutate(changed_cols = "UNK",
                  record_status = "NEW or CHANGED",
                  change_details = "",
                  comment = "Missing index, cannot detect changes")

  if(length(common_names) > 1){
    same_records <- new_df %>% dplyr::inner_join(old_df_same_class[, common_names], by = common_names) %>%
      dplyr::mutate(changed_cols = "",
                    record_status = "UNCHANGED",
                    change_details = "",
                    comment = "No changes in this record")
  } else {
    same_records <- new_df[new_df[[common_names]] %in% old_df_same_class[[common_names]], ] %>%
      dplyr::mutate(changed_cols = "",
                    record_status = "UNCHANGED",
                    change_details = "",
                    comment = "No changes in this record")
  }

  final_output <- dplyr::bind_rows(new_or_changed_records, same_records)

  if(nrow(final_output) > nrow(new_df)){
    print("warinig: duplicates")
  }

  return(final_output)

}


#' Reconcile Two Versions of a Data Frame
#'
#' This function compares a new and an old version of a data set to identify
#' inserted, deleted, and updated records, as well as column-level changes.
#' The comparison can be performed using a specified index column (or columns),
#' or—if no index is provided—based on a full-row comparison across all
#' common columns.
#'
#' When `index` is supplied, rows are matched by the specified index variable(s),
#' allowing the function to detect newly added records, removed records, and
#' detailed field-level changes. When `index = NA`, the function falls back to
#' a full reconciliation based on the auxiliary comparison routine,
#' using all common columns as the key.
#'
#' Column comparison is further controlled by `lookup_columns`: if this argument
#' is left as `NA`, all columns common to `new_df` and `old_df` are evaluated;
#' otherwise, only the specified subset of columns is compared.
#'
#' @param new_df A data frame containing the most recent version of the data.
#' @param old_df A data frame containing the preceding version of the data,
#'   used as the reference for comparison.
#' @param index A character vector specifying the variable(s) that uniquely
#'   identify records (e.g., `"recordid"`). If `NA`, all common columns are
#'   used as the matching key, but some enhanced functionality (such as
#'   detecting newly added or removed rows) will not be available.
#' @param lookup_columns A character vector specifying which columns should be
#'   compared. By default `NA`, meaning that all columns common to both
#'   `new_df` and `old_df` are used. If specific column names are provided,
#'   comparisons are restricted to those columns.
#' @author Lukasz Andrzejewski
#'
#' @return A data frame summarizing the reconciliation results. For each record,
#'   the output includes the current values, index variables, detected status
#'   (`"NEW"`, `"DELETED"`, `"UPDATED"`, `"UNCHANGED"`), the set of changed
#'   columns, and a human-readable description of the differences.
#' @export
#'
#' @examples reconcile(data.frame(col1 = c("AA", "B"), id = c(1, 2)),
#' data.frame(col1 = c("A", "B"), id = c(1, 3)), index = "id")
reconcile <- function(new_df, old_df, index = NA, lookup_columns = NA) {

  old_df_same_class <- get_same_class(old_df = old_df, new_df = new_df)
  names_old <- names(old_df)
  names_new <- names(new_df)

  if(all(is.na(lookup_columns))){
    common_names <- names_new[names_new %in% names_old]
  } else {
    common_names <- c(index, lookup_columns)
  }

  new_variable <- names_new[!names_new %in% names_old]
  if (length(new_variable) == 0) new_variable <- NA
  old_variable <- names_old[!names_old %in% names_new]
  if (length(old_variable) == 0) old_variable <- NA

  if(all(is.na(index))){
    output <- reconcile_without_index(new_df = new_df, old_df = old_df, lookup_columns = lookup_columns)

    output$comment <- ifelse(!is.na(new_variable),
                             glue::glue(paste0("{output$comment}, added variable: {new_variable}")), output$comment)


    output$comment <- ifelse(!is.na(old_variable) & is.na(new_variable),
                             glue::glue(paste0("{output$comment}, removed variable: {old_variable}")),
                             ifelse(!is.na(old_variable) & !is.na(new_variable),
                                    glue::glue(paste0("{output$comment}, removed variable: {old_variable}")),
                                    output$comment))

    return(output)
  }


  new_records <- new_df[, names_new] %>% dplyr::anti_join(old_df_same_class[, common_names], by = index) %>%
    dplyr::mutate(changed_cols = "ALL",
                  record_status = "NEW",
                  change_details = "new record created",
                  comment = "")

  deleted_records <- old_df_same_class[, common_names] %>% dplyr::anti_join(new_df[, names_new], by = index) %>%
    dplyr::mutate(changed_cols = "ALL",
                  record_status = "DELETED",
                  change_details = "record deleted",
                  comment = "")


  changed_records <- new_df %>% dplyr::anti_join(new_records, by = index)
  old_df_same_records <- old_df_same_class %>% dplyr::inner_join(changed_records, by = index, suffix = c(".x", ""))
  keep_names <- c(names(old_df_same_records)[grepl(".x$", names(old_df_same_records))], index)
  old_df_same_records <- old_df_same_records[, keep_names]
  old_df_same_records <- old_df_same_records %>% dplyr::rename_with(~ gsub("\\.x$", "", x = names(old_df_same_records)))

  sort_by <- c(index, common_names[!common_names %in% index])
  new_changed_records_sorted <- changed_records %>% dplyr::arrange(dplyr::across(dplyr::all_of(sort_by)))
  old_df_same_records_sorted <- old_df_same_records %>% dplyr::arrange(dplyr::across(dplyr::all_of(sort_by)))

  same_index_comparison <- compare_rows_with_same_index(new_df = new_changed_records_sorted[, common_names],
                                                        old_df = old_df_same_records_sorted[, common_names])

  if(all(!is.na(lookup_columns))) {
    same_index_comparison <- cbind(new_changed_records_sorted[!names(new_changed_records_sorted) %in% common_names], same_index_comparison)
  }

  if (is.na(new_variable)) {
    same_index_comparison <- same_index_comparison %>%
      dplyr::mutate(record_status =
            dplyr::case_when(changed_cols != "" ~ "UPDATED",
                            TRUE ~ "UNCHANGED"),
                    comment = "")
  } else {
  same_index_comparison <- suppressMessages(same_index_comparison %>% dplyr::left_join(new_df[, c(new_variable, index)])) %>%
      dplyr::mutate(record_status =
            dplyr::case_when(changed_cols != "" ~ "UPDATED",
                          TRUE ~ "UNCHANGED"),
                    comment = "")
  }


  output <- dplyr::bind_rows(new_records, deleted_records, same_index_comparison) %>% dplyr::arrange(dplyr::across(dplyr::all_of(index)))

  output$comment <- ifelse(!is.na(new_variable),
    glue::glue(paste0("added variable: {new_variable}")), "")


  output$comment <- ifelse(!is.na(old_variable) & is.na(new_variable),
      glue::glue(paste0("removed variable: {old_variable}")),
        ifelse(!is.na(old_variable) & !is.na(new_variable),
             glue::glue(paste0("{output$comment}, removed variable: {old_variable}")),
                output$comment))

  if(nrow(output) > nrow(new_df)){
    print("warinig: records missing in current dataset or duplicates")
  }

  return(output)
}
