# Dataset specific normalisation recipes ----

# # the generic
# normalise = function(rawData, ...) {
#   return(rawData)
# }

validate.avoncap_export.uad_controls = function(rawData, ...) {
  rawData
  # TODO
}

validate.avoncap_export.central = function(rawData, ..., dq = avoncap::load_data("avoncap-data-quality")) {
  tmp = rawData %>%
    .not_empty(
      c("consented","ppc","include_patient", "hosp")
    ) %>%
    .conflicting_values(include_patient==1, ppc==2, label = "inconsistent consent") %>%
    .conflicting_values(include_patient==1, consented==2, label = "inconsistent consent") %>%
    # patients can only withdraw from the explicit consented arm supposedly
    .conflicting_values(include_patient==0, withdrawal == 1, label = "withdrawal of unconsented") %>%
    .active_col(include_patient == 1, "only actively consented") %>%
    .not_empty(c("withdrawal")) %>%

    # CONSENTED PATIENTS ONLY: consent falg present not declined and not withdrawn
    .active_col(!(include_patient == 1 & withdrawal == 1) & !is.na(consented) & consented != 2 & ppc != 2, "consent for data", na.rm = FALSE) %>%
    # tolerates NaN for appropriately missing values
    .not_na(
      c("hr", "systolic_bp", "diastolic_bp", "temperature", "rr", "pulse_ox",
      "fio2", "imd", "symptom_days_preadmit", "hypertensives", "statins", "anticoagulants", "antiplatelets")
    ) %>%
    # does not tolerate NaN or infinite values
    .not_empty(
      c("news_2_total", "crb_test_mai", "care_home",
        "ckd", "liver_disease", "diabetes", "gastric_ulcers", "pvd", "ctd",
        "immunodeficiency", "other_pn_disease", "cancer",
        "transplant", "smoking", "gender",
        "hospital_length_of_stay", "covid_19_diagnosis",
        "lrtd_30d_outcome", "week_number", "year",
        "highest_level_care_require", "ventilatory_support", "psi_class",
        "age_at_admission",
        "acute_illness", "covid19", "clinical_radio_diagnosis",
        "fever2", "pleurtic_cp", "cough2", "sput_prod", "dyspnoea",
        "tachypnoea2", "ausc_find", "radiologic", "ethnicity","gp_practice_drop_down")
    ) %>%
    .checkbox_not_empty(
      c("resp_disease", "chd", "dementia", "neurological_disease", "hiv", "haem_malig", "final_soc_lrtd_diagnosis")
    ) %>%
    .conflicting_values(gender == 1, pregnancy!=1, label = "pregnant male") %>%
    .conflicting_values(age_at_admission > 70, pregnancy!=1, label = "pregnant >70 year old") %>%

    .conflicting_values(
      final_soc_lrtd_diagnosis___4==1,
      final_soc_lrtd_diagnosis___1==1 | final_soc_lrtd_diagnosis___2==1 | final_soc_lrtd_diagnosis___3==1,
      label = "both pneumonia and NP-LRTI in final SOC dx") %>%
    # .conflicting_values(
    #   final_soc_lrtd_diagnosis___9==1,
    #   final_soc_lrtd_diagnosis___1==1 | final_soc_lrtd_diagnosis___2==1 | final_soc_lrtd_diagnosis___3==1 | final_soc_lrtd_diagnosis___5==1 | final_soc_lrtd_diagnosis___4==1,
    #   label = "non infectious and either pneumonia or NP-LRTI in final SOC dx") %>%

    .conflicting_values(lrtd_30d_outcome > 1 & survival_days <= 30, label = "marked as survived in lrtd_30d_outcome but survival_days <= 30 days") %>%
    .conflicting_values(survival_1yr_days != survival_days, label = "30 day and 1 yr survival duration different") %>%
    .conflicting_values(hospital_length_of_stay > survival_days, label = "length of stay > 30 day survival duration") %>%
    .conflicting_values(hospital_length_of_stay > survival_1yr_days, label = "length of stay > 1 yr survival duration") %>%
    .clear_active() %>%

    # NOT HOSPTIAL ACQUIRED
    .active_col(
      !(include_patient == 1 & withdrawal == 1) & consented != 2 & ppc != 2 &
        hapcovid_screening == 0 & hospital_covid ==0, "Non hospital acquired cohort") %>%
    .conflicting_values(lrtd_30d_outcome == 1 & survival_days > 30, label = "marked as died lrtd_30d_outcome but survival_days > 30 days") %>%
    .clear_active() %>%

    # DIABETICS ONLY
    .active_col(diabetes != 1, "diabetics") %>%
    .not_empty(c("dm_meds")) %>%
    .clear_active() %>%

    # COVID POSITIVES ONLY
    .active_col(covid19 == 1 & enrollment_date > "2021-03-15", "covid+ after 15/3/21") %>%
    .not_empty(c("current")) %>%
    .clear_active() %>%

    # CONSENTED WOMEN ONLY
    .active_col(
      !(include_patient == 1 & withdrawal == 1) & !is.na(consented) & consented != 2 & ppc != 2 &
        gender == 2 & age_at_admission<70, "consented women under 70" ) %>%
    .not_empty(c("pregnancy")) %>%
    .clear_active() %>%


    # finalised the issues data and remove known issues
    .copy_identifiers(c("record_number","study_year","hosp")) %>%
    .remove_known_issues(dq, by=c("record_number",".variable"))
}

validate.nhs_extract.deltave = function(rawData, ...) {
  rawData
  # TODO
}

