.ifnull = function(x,default) {
  if (is.null(x)) default else x
}

# Dispatch normalisation based on data type and subtype ---

#' Validate AvonCap raw data
#'
#' Runs a set of QA checks. This function dispatches the call
#' in a data set specific function using the `type` and `subtype` of the
#' data set. The checks are in source files named `validate-xxx.R` depending
#' on the data source.
#'
#' @param rawData - the raw data from `load_data()`
#' @param ... not used / passed to the validation function specific to
#'   the type of data.
#'
#' @return the same input with a new `data_quality_failures` attribute containing
#'    issues.
#' @export
validate_data = function(
    rawData,
    ...
) {

  type = attr(rawData,"type")
  subtype = attr(rawData,"subtype")
  valid_fn = c("validate",type,subtype) %>% stringr::str_replace("-","_") %>% paste0(collapse=".")
  message("validate data using: ",valid_fn)
  valid_fn = tryCatch(
    # utils::getFromNamespace(norm_fn,"avoncap"),
    get(valid_fn),
    error = function(e) {
      message("No data validation defined for ",valid_fn)
      return(function(x, ...) x)
    }
  )
  # dispatch the call to the specific subtype of the call
  # or to a noop function if there is no specific method
  rawData %>%
    .id_col() %>%
    valid_fn(...) %>%
    .tidy_up() %>%
    .restore_attributes(rawData) %>%
    return()

}

# Validation helper functions ----
# These support the functions within this file and are internal

# Change the source data adding a .row_number id columns
.id_col = function(df) {
  if (".row_number" %in% colnames(df)) return(df)
  return(df %>% dplyr::mutate(.row_number = dplyr::row_number()))
}

.active_filter = function(df, active_only = TRUE) {
  if (active_only && ".active" %in% colnames(df)) df = df %>% dplyr::filter(.active)
  return(df)
}

# expects a tibble of row_number and failures and
# adds as metadata
.update_validation = function(df, failures, label) {
  if (nrow(failures)==0) return(df)
  failures = failures %>% dplyr::select(tidyselect::any_of(c(".row_number", ".error_type", ".variable", ".subgroup" )))
  if (!".subgroup" %in% colnames(failures)) failures = failures %>% dplyr::mutate(.subgroup = "all")
  message(label, " in ", nrow(failures), " rows")
  missing = df %>% .get_failures()

  missing = dplyr::bind_rows(missing, failures)
  df %>% magrittr::set_attr("data_quality_failures", missing)
}

.get_failures = function(df) {
  tmp = df %>% attr("data_quality_failures")
  if (!is.null(tmp)) {
    return(tmp)
  }
  return(tibble::tibble(.row_number=integer(),.error_type=character(), .variable=character(), .subgroup=character()))
}



.identify_failures = function(df, failure_expr, active_only=TRUE) {
  failure_expr = rlang::enexpr(failure_expr)
  tmp = df %>% .active_filter(active_only) %>% dplyr::filter(!!failure_expr)
  return(tmp)
}

.tidy_up = function(df) {
  df %>%
    dplyr::select(-tidyselect::any_of(c(".active", ".row_number", ".error_type", ".variable", ".subgroup" )))
}

# Validation test functions ----
# These functions are used in a magrittr pipe to define the validation
# pipeline. These are the only ones expected to be seen in the validate.xxx.yyy
# functions

# Change the source data adding a .active column
# set an active filter (in the data)
# this clears existing active filters.
.active_col = function(df, ex, label, na.rm=TRUE) {
  ex = rlang::enexpr(ex)
  reason = format(ex) %>% trimws() %>% paste0(collapse=" ")
  return(
    df %>%
      dplyr::mutate(.active = !!ex, .subgroup = label) %>%
      dplyr::mutate(.active = ifelse(is.na(.active), !na.rm, .active))
  )
}

# clear an active filter
.clear_active = function(df) {
  return(
    df %>%
      dplyr::mutate(.active = TRUE, .subgroup = "all records")
  )
}

.col_present = function(df, cols) {
  missing = cols[!cols %in% colnames(df)]
  tmp = tibble::tibble(
    .row_number = NA_integer_,
    .error_type = "missing column",
    .variable = missing
  )
  .update_validation(df,tmp,sprintf("missing columns: %s",paste0(missing,collapse=", ")))
}

# tmp = iris %>% .id_col() %>% dplyr::mutate(Sepal.Width = ifelse(Sepal.Width > mean(Sepal.Width), NA, Sepal.Width)) %>% .not_empty("Sepal.Width")
# tmp %>% .get_failures()
.not_na = function(df, cols) {

  df = df %>% .col_present(cols)
  for (col in intersect(cols,colnames(df))) {
      col = as.symbol(col)
      tmp = df %>%
        .identify_failures(is.na(!!col) & !is.nan(!!col)) %>%
        dplyr::mutate(
          .error_type = "missing value",
          .variable = rlang::as_label(col)
        )
      df = .update_validation(df,tmp, sprintf("missing value in %s",rlang::as_label(col)))
  }
  return(df)

}

.not_empty = function(df, cols) {

  df = df %>% .col_present(cols)
  for (col in intersect(cols,colnames(df))) {
    col = as.symbol(col)
    tmp = df %>%
      .identify_failures(is.na(!!col) | !is.finite(!!col)) %>%
      dplyr::mutate(
        .error_type = "missing value",
        .variable = rlang::as_label(col)
      )
    df = .update_validation(df,tmp, sprintf("non-finite value in %s",rlang::as_label(col)))
  }
  return(df)

}


.checkbox_not_empty = function(df, cols, active_only=TRUE) {

  for (col in cols) {

    tmp = df %>%
      .active_filter(active_only) %>%
      dplyr::select(.row_number, .subgroup, tidyselect::starts_with(col)) %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with(col)) %>%
      dplyr::group_by(.row_number, .subgroup) %>%
      dplyr::summarise(any_pres = any(value == 1)) %>%
      dplyr::filter(!any_pres) %>%
      dplyr::mutate(
        .error_type = "none checked in checkbox",
        .variable = col
      )
    df = .update_validation(df,tmp, sprintf("missing value in %s",col))
  }
  return(df)
}

.conflicting_values = function(df, ..., label, active_only=TRUE) {
  f = rlang::enexprs(...)
  reason = lapply(f, format) %>%
    lapply(trimws) %>% sapply(paste0, collapse=" ") %>% paste0(collapse = ", ")
  tmp = df %>% dplyr::filter(...) %>%
    .active_filter(active_only) %>%
    dplyr::mutate(
      .error_type = label,
      .variable = reason
    )
  df = .update_validation(df, tmp, label)
  return(df)
}

# Update the
.copy_identifiers = function(df, retain) {
  tmp = .get_failures(df)
  meta = df %>% dplyr::select(tidyselect::any_of(retain),.row_number)
  tmp = meta %>% dplyr::inner_join(tmp, by=".row_number")
  df = df %>% magrittr::set_attr("data_quality_failures", tmp)
  return(df)
}


.remove_known_issues = function(df, known_df, by) {

  tmp = .get_failures(df)
  tmp = tmp %>% dplyr::anti_join(known_df, by=by)
  df = df %>% magrittr::set_attr("data_quality_failures", tmp)
  return(df)
}

# Recover and export validation data ----

#' Write out data quality issues
#'
#' @param df the raw data frame
#' @param file the output data quality file
#'
#' @return the list of failures as a dataframe
#' @export
write_issues = function(df, file) {
  date = attributes(df)$date
  file = fs::path_ext_remove(file)
  if (!is.null(date)) file = paste0(file,"-",as.character(date))
  file = fs::path_ext_set(file,"xlsx")

  failures = df %>% .get_failures()

  issues = failures %>% dplyr::group_by(.row_number) %>% dplyr::summarise(issues = dplyr::n()) %>% dplyr::group_by(issues) %>% dplyr::summarise(`item count`=dplyr::n()) %>% dplyr::mutate(issues = sprintf("%d issues",issues))
  issues = failures %>% dplyr::summarise(`item count`=dplyr::n_distinct(.row_number)) %>% dplyr::mutate(issues = "Items with issues") %>% dplyr::bind_rows(issues) %>% dplyr::select(issues,`item count`)
  issues = tibble::tibble(issues = "Total items", `item count`=nrow(df)) %>% dplyr::bind_rows(issues)

  missing = failures %>% dplyr::filter(.error_type %in% c("missing value","none checked in checkbox")) %>% dplyr::group_by(`patient subgroup` = .subgroup, `missing value` = .variable) %>% dplyr::count() %>% dplyr::arrange(`patient subgroup`,dplyr::desc(n))

  inconsistent = failures %>% dplyr::filter(!.error_type %in% c("missing value","none checked in checkbox")) %>% dplyr::group_by(`error` = .error_type) %>% dplyr::count() %>% dplyr::arrange(dplyr::desc(n))

  sheet = list(
    "Items by number of issues" = issues,
    "Missing items by field" = missing,
    "Inconsistent entries" = inconsistent,
    "Data quality list" = failures
  )

  writexl::write_xlsx(sheet, file)
  return(failures)
}
