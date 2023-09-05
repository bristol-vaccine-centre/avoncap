.ifnull = function(x,default) {
  if (is.null(x)) default else x
}

# Dispatch normalisation based on data type and subtype ---

#' Validate AvonCap raw data
#'
#' Runs a set of QA checks
#'
#' files Most of the sanitisation code is held in the
#' `zzz-avoncap-mappings.R` file.
#'
#' @param rawData - the raw data from `load_data()`
#' @inheritDotParams normalise_generic
#'
#' @return a tracked dataframe with
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
    .restore_attributes(rawData) %>%
    return()

}

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

  wb = openxlsx::createWorkbook()

  issues = failures %>% dplyr::group_by(.row_number) %>% dplyr::summarise(issues = dplyr::n()) %>% dplyr::group_by(issues) %>% dplyr::summarise(`item count`=dplyr::n()) %>% dplyr::mutate(issues = sprintf("%d issues",issues))
  issues = failures %>% dplyr::summarise(`item count`=dplyr::n_distinct(.row_number)) %>% dplyr::mutate(issues = "Items with issues") %>% dplyr::bind_rows(issues) %>% dplyr::select(issues,`item count`)
  issues = tibble::tibble(issues = "Total items", `item count`=nrow(df)) %>% dplyr::bind_rows(issues)

  openxlsx::addWorksheet(wb = wb, sheetName = "Items by number of issues")
  openxlsx::writeDataTable(wb = wb, sheet = 1, x = issues)
  openxlsx::setColWidths(wb, sheet = 1, cols = 1:length(issues), widths = "auto")

  missing = failures %>% dplyr::filter(.error_type %in% c("missing value","none checked in checkbox")) %>% dplyr::group_by(`patient subgroup` = .subgroup, `missing value` = .variable) %>% dplyr::count() %>% dplyr::arrange(`patient subgroup`,dplyr::desc(n))
  openxlsx::addWorksheet(wb = wb, sheetName = "Missing items by field")
  openxlsx::writeDataTable(wb = wb, sheet = 2, x = missing)
  openxlsx::setColWidths(wb, sheet = 2, cols = 1:length(missing), widths = "auto")

  inconsistent = failures %>% dplyr::filter(!.error_type %in% c("missing value","none checked in checkbox")) %>% dplyr::group_by(`error` = .error_type) %>% dplyr::count() %>% dplyr::arrange(dplyr::desc(n))
  openxlsx::addWorksheet(wb = wb, sheetName = "Inconsistent entries")
  openxlsx::writeDataTable(wb = wb, sheet = 3, x = inconsistent)
  openxlsx::setColWidths(wb, sheet = 3, cols = 1:length(inconsistent), widths = "auto")

  openxlsx::addWorksheet(wb = wb, sheetName = "Data quality list")
  openxlsx::writeDataTable(wb = wb, sheet = 4, x = failures)
  openxlsx::setColWidths(wb, sheet = 4, cols = 1:length(failures), widths = "auto")

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)

  return(failures)
}

# Dataset specific normalisation recipes ----

# # the generic
# normalise = function(rawData, ...) {
#   return(rawData)
# }

validate.avoncap_export.uad_controls = function(rawData, ...) {
  rawData
  # TODO
}

validate.avoncap_export.central = function(rawData, ...) {
  rawData %>%
    .not_empty(
      c("consented","ppc","include_patient",
        "hosp")
    ) %>%
    .conflicting_values(include_patient==1, ppc==2, label = "inconsistent consent") %>%
    .conflicting_values(include_patient==1, consented==2, label = "inconsistent consent") %>%
    # patients can only withdraw from the explicit consented arm supposedly
    .conflicting_values(include_patient==0, withdrawal == 1, label = "withdrawal of unconsented") %>%
    .active_col(include_patient == 1, "only actively consented") %>%
    .not_empty(c("withdrawal")) %>%
    # consent not declined and not withdrawn
    .active_col(!(include_patient == 1 & withdrawal == 1) & consented != 2 & ppc != 2, "consent for data", na.rm = FALSE) %>%
    .not_empty(
      c("hr", "systolic_bp", "diastolic_bp", "temperature", "rr", "pulse_ox",
        "fio2", "news_2_total", "crb_test_mai", "care_home",
        "ckd", "liver_disease", "diabetes", "gastric_ulcers", "pvd", "ctd",
        "immunodeficiency", "other_pn_disease", "cancer",
        "transplant", "smoking", "gender",
        "hospital_length_of_stay", "covid_19_diagnosis",
        "lrtd_30d_outcome", "week_number", "year",
        "highest_level_care_require", "ventilatory_support", "psi_class", "imd",
        "age_at_admission",
        "acute_illness", "covid19", "clinical_radio_diagnosis",
        "fever2", "pleurtic_cp", "cough2", "sput_prod", "dyspnoea",
        "tachypnoea2", "ausc_find", "radiologic",
        "symptom_days_preadmit", "ethnicity")
    ) %>%
    .checkbox_not_empty(
      c("resp_disease", "chd", "dementia", "neurological_disease", "hiv", "haem_malig", "final_soc_lrtd_diagnosis")
    ) %>%
    .conflicting_values(gender == 1, pregnancy!=1, label = "pregnant male") %>%
    .conflicting_values(age_at_admission > 70, pregnancy!=1, label = "pregnant >70 year old") %>%
    .conflicting_values(
      final_soc_lrtd_diagnosis___4==1,
      final_soc_lrtd_diagnosis___1==1 | final_soc_lrtd_diagnosis___2==1 | final_soc_lrtd_diagnosis___3==1 | final_soc_lrtd_diagnosis___5==1,
      label = "both pneumonia and NP-LRTI in final SOC dx") %>%
    # .conflicting_values(
    #   final_soc_lrtd_diagnosis___9==1,
    #   final_soc_lrtd_diagnosis___1==1 | final_soc_lrtd_diagnosis___2==1 | final_soc_lrtd_diagnosis___3==1 | final_soc_lrtd_diagnosis___5==1 | final_soc_lrtd_diagnosis___4==1,
    #   label = "non infectious and either pneumonia or NP-LRTI in final SOC dx") %>%

    .active_col(diabetes != 1, "diabetics") %>% .not_empty(c("dm_meds")) %>%
    .active_col(covid19 == 1 & enrollment_date > "2021-03-15", "covid+ after 15/3/21") %>% .not_empty(c("current")) %>%
    .active_col(
      !(include_patient == 1 & withdrawal == 1) & consented != 2 & ppc != 2 &
      gender == 2 & age_at_admission<70, "consented women under 70" ) %>% .not_empty(c("pregnancy")) %>%

    .tidy_up(c("record_number","study_year","hosp"))
}

validate.nhs_extract.deltave = function(rawData, ...) {
  rawData
  # TODO
}

validate.nhs_extract.pneumococcal = function(rawData, ...) {
  rawData
  # TODO
}

validate.urine_antigens = function(rawData, ...) {
  rawData
  # TODO
}

# Validation helper functions ----

.id_col = function(df) {
  if (".row_number" %in% colnames(df)) return(df)
  return(df %>% dplyr::mutate(.row_number = dplyr::row_number()))
}

.active_col = function(df, ex, label, na.rm=TRUE) {
  ex = rlang::enexpr(ex)
  reason = format(ex) %>% trimws() %>% paste0(collapse=" ")
  return(
    df %>%
        dplyr::mutate(.active = !!ex, .subgroup = label) %>%
        dplyr::mutate(.active = ifelse(is.na(.active), !na.rm, .active))
  )
}

.active_filter = function(df, active_only = TRUE) {
  if (active_only && ".active" %in% colnames(df)) df = df %>% dplyr::filter(.active)
  return(df)
}

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

.tidy_up = function(df, retain) {
  tmp = .get_failures(df)
  meta = df %>% dplyr::select(tidyselect::any_of(retain),.row_number)
  tmp = meta %>% dplyr::inner_join(tmp, by=".row_number")
  df %>% magrittr::set_attr("data_quality_failures", tmp) %>%
    dplyr::select(-tidyselect::any_of(c(".row_number", ".error_type", ".variable", ".subgroup" )))
}

## Validation test functions ----

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
.not_empty = function(df, cols) {

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
}
