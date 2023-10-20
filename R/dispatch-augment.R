# Dataset specific augmentation recipes ----

# the generic
augment = function(df, ...) {
  return(df)
}

augment.avoncap_export.uad_controls = function(df,...) {
  # Controls database has some naming inconsistencies:
  df %>%
    augment_generic(
      ~ .x # a no-op example
      , ...)
}

augment.avoncap_export.central = function(df,...) {
  df %>%
    augment_generic(
      derive_continuous_categories,
      derive_patient_identifier,
      derive_admission_episode,
      derive_gp_linkage,
      derive_simpler_comorbidities,

      derive_presumed_diagnosis_categories,
      derive_nosocomial_status,

      derive_covid_status,
      derive_diagnosis_categories,
      derive_infective_classification, # must be after diagnosis categories and covid status
      derive_aLRTD_categories,
      derive_nosocomial_covid_status,

      derive_genomic_variant,
      derive_vaccination_timings,
      derive_completed_vaccination_status,
      derive_effective_vaccination_status,
      derive_vaccine_combinations,

      derive_pneumococcal_categories,
      derive_pneumococcal_high_risk,
      derive_pneumococcal_risk_category,

      derive_WHO_outcome_score,
      derive_severe_disease_outcomes,
      derive_survival_times_avoncap,
      derive_survival_censoring,
      derive_quintile_category(survival.length_of_stay),
      derive_hospital_burden_outcomes,

      derive_reordered_factors(),
      ...,
    ) %>%
    .wipe_non_consented_data()
}

augment.avoncap_export.central.micro = function(df,...) {
  df %>%
    augment_generic(
      ~ .x # a no-op example
      ,...)
}

augment.avoncap_export.central.virol = function(df,...) {
  df %>%
    augment_generic(
      ~ .x # a no-op example
      ,...)
}

augment.avoncap_export.central.radio = function(df,...) {
  df %>%
    augment_generic(
      ~ .x # a no-op example
      ,...)
}

augment.avoncap_export.central.haem = function(df,...) {
  df %>%
    augment_generic(
      derive_haematology_categories
      ,...) %>%
    .wipe_non_consented_data()
}

augment.nhs_extract.deltave = function(df,...) {
  df %>%
    augment_generic(
      derive_continuous_categories,
      derive_admission_episode,
      derive_simpler_comorbidities,
      derive_covid_status,
      derive_diagnosis_categories,
      derive_infective_classification, # must be after diagnosis categories
      derive_aLRTD_categories,
      derive_nosocomial_covid_status,
      derive_genomic_variant,
      derive_vaccination_timings,
      derive_completed_vaccination_status,
      derive_effective_vaccination_status,
      derive_vaccine_combinations,
      derive_pneumococcal_high_risk,
      derive_pneumococcal_risk_category,
      derive_WHO_outcome_score,
      derive_severe_disease_outcomes,

      derive_hospital_burden_outcomes,
      ...
    ) %>%
    .wipe_non_consented_data()
}

augment.nhs_extract.pneumococcal = function(df,...) {
  df %>%
    augment_generic(
      derive_pneumo_polyfill,
      derive_phe_pcv_group,
      derive_pandemic_timings(pneumo.test_date,"pneumo"),
      derive_invasive_status,
      derive_continuous_categories_pneumo,
      derive_pneumo_clinical_syndrome,
      derive_survival_times_pneumo,
      derive_survival_censoring,
      derive_quintile_category(survival.length_of_stay),
      derive_simpler_comorbidities,
      # comorbid.interstitial_lung_dx not found
      derive_pneumococcal_high_risk,
      derive_pneumococcal_risk_category,
      derive_reordered_factors(),

      ...)
}

augment.urine_antigens.binax = function(df,...) {
  df %>%
    augment_generic(
      ~ .x # a no-op example
      ,...)
}

augment.urine_antigens.serotype = function(df,...) {
  df %>%
    augment_generic(
      # This has some settings but for the basic recipe we use defaults:
      derive_pcv_groupings,
      derive_pneumo_uad_status,
      ...)
}
