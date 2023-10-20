#' Normalise the urinary antigen data (binax results)
#'
#' `r .document_mapping(map_urine_binax)`
#'
#' @concept map
#' @return a list
#' @export
map_urine_binax = function() {list(
  "RESULT" = .normalise_text(pneumo.binax_result,preprocess = ~ dplyr::case_when(
    toupper(.x) %in% c('IND','QNS') ~ "Other",
    toupper(.x) == "NEG" ~ "Negative",
    toupper(.x) == "POS" ~ "Positive",
    TRUE ~ "Unknown"
  )),
  "EVENT_DATE" = .normalise_date(pneumo.test_date,tryFormats="%e-%b-%y"),
  "SUBJECT" = .normalise_study_id(admin.consented_record_number),
  "BARCODE" = .normalise_name(pneumo.urine_antigen_sample_id)
)}

keys_urine_antigens_binax = function() {
  list(
    "consent" = "{admin.consented_record_number}",
    "sample" = "{admin.consented_record_number}-{pneumo.test_date}"
  )
}
