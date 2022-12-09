
keys_avoncap_radio  = function(instrument) {
  list(
    "radio" = sprintf("{admin.record_number}_radio_%d",instrument)
  )
}

#' Normalise the avoncap data radiology data
#'
#' `r .document_mapping(map_avoncap_radio)`
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_radio = function(instrument) {
  tmp = list(
    "radio_exam" = .normalise_yesno(radio.test_performed),
    "radiology_date" = .normalise_date(radio.test_date),
    "radiodays" = .normalise_name(radio.test_days_from_admission),
    "radio_test" = .normalise_list(radio.test_type, c("CXR", "CT scan (CT thorax, CTPA, HRCT)", "US thorax", "MRI", "Other")),
    # "radiology_othertest", text
    # "radiology_result" = .normalise_checkboxes(dplyr::vars(
    #   radio.normal,
    #   radio.consistent_with_pneumonia,
    #   radio.consistent_with_heart_failure,
    #   radio.consistent_with_pleural_effusion,
    #   radio.consistent_with_covid_19,
    #   radio.other_abnormal_finding)),
    # "radiology_other_result" = .normalise_checkboxes(dplyr::vars(
    #   radio.pulmonary_embolus,
    #   radio.pneumothorax_or_pneumomediastinum,
    #   radio.cavity_including_abscesses,
    #   radio.aspergillioma,
    #   radio.lung_malignancy,
    #   radio.lung_nodules,
    #   radio.tuberculosis,
    #   radio.empyema)),
    "radiology_result" = .normalise_checkboxes_to_nested_list(
      radio.alrtd_finding, c(
        "Normal", "Consistent with pneumonia", "Consistent with heart failure",
        "Consistent with pleural effusion", "Consistent with COVID-19",
        "Other abnormal finding"),
      "finding", "present"),
    "radiology_other_result" = .normalise_checkboxes_to_nested_list(
      radio.non_alrtd_finding, c(
        "Pulmonary embolus", "Pneumothorax and/or pneumomediastinum",
        "Cavity including abscesses", "Aspergillioma", "Lung malignancy",
        "Lung nodules", "Tuberculosis (old or new)", "Empyema"),
      "finding", "present")
    # "micro_other", text
  )

  names(tmp) = sprintf("%s_%d",names(tmp),instrument)
  return(c(
    "record_number" = .normalise_name(admin.record_number),
    "ac_study_number" = .normalise_study_id(admin.consented_record_number),
    tmp))
}

