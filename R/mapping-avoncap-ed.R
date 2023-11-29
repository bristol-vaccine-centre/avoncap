# These mappings work for the
# avoncap-extract, central
# nhs-extract, deltave
# and with some renaming
# avoncap-extract, uad-controls

# they are a good starting point for the
# nhs_extract, pneumococcal


keys_avoncap_ed = function() {list(
  "admit" = "{admin.record_number}",
  "consent" = "{NA_character_}"
)}

#' ED consent
#'
#' `r .document_mapping(map_avoncap_consent)`
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_ed_consent = function() {list(
  "optout" = .normalise_yesno(admin.opted_out)
)}


#' Avoncap ED normalisation
#'
#' All the ED data is also mapped using the `map_avoncap_central()` list
#' as it si quite similar
#'
#' `r .document_mapping(map_avoncap_ed)`
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_ed = function() {list(

  # TODO:
  # 1) "did_the_patient_have_respi" = .normalise_yesno, actually 4 options, not clear what they are
  # 2) change this to deal with explicit NA options in the drop downs otherwise we are missing explicit missing values
  # in the dropdowns.
  "ed_hours" = .normalise_name(outcome.emergency_dept_length_of_stay),
  # Basically not a good idea to use this
  # "hospital_length_of_stay" = .normalise_name(outcome.emergency_dept_length_of_stay_days)
  "ed_reattendance" = .normalise_name(admin.ed_episodes_in_last_30_days),

  "hosp_adm_30d" = .normalise_yesno(outcome.admitted_within_30_days),
  "hosp_adm_7d" = .normalise_yesno(outcome.admitted_within_7_days),
  "home_d_1" = .normalise_name(outcome.days_since_last_ed_episode),


  # admission.cxr_pneumonia needed for later augmentation
  # needs to be the union of two fields
  "radiology_result_1___2" = .normalise_yesno(radio.consistent_with_pneumonia_1),
  "radiology_result_2___2" = .normalise_yesno(radio.consistent_with_pneumonia_2)


)}
