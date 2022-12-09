long_demographic_comparison = dplyr::vars(
  demog.age,
  demog.age_category,
  demog.age_eligible,
  demog.gender,
  demog.ethnicity,
  demog.care_home_resident,
  demog.smoker,
  demog.pcr_positive_by_age,
  vaccination.pneumovax,
  vaccination.influenza_vaccination,
  vaccination.covid_vaccination,
  admission.frailty_score,
  admission.charlson_comorbidity_index,
  admission.pneumonia_severity_index_class,
  admission.curb_65_severity_score,
  comorbid.copd,
  comorbid.asthma,
  comorbid.bronchiectasis,
  comorbid.interstitial_lung_dx,
  comorbid.cystic_fibrosis,
  comorbid.ccf,
  comorbid.ihd,
  comorbid.hypertension,
  comorbid.af,
  comorbid.other_heart_dx,
  admission.pneumococcal_risk_group,
  comorbid.other_pneumococcal_risks,
  admission.on_immunosuppression,
  comorbid.immunodeficiency,
  comorbid.transplant_recipient,
  comorbid.diabetes_type,
  comorbid.ckd,
  comorbid.dementia,
  comorbid.cognitive_impairment,
  comorbid.cva,
  comorbid.tia,
  comorbid.hemiplegia,
  comorbid.neuro_other,
  comorbid.paraplegia,
  comorbid.solid_cancer_present,
  comorbid.leukaemia,
  comorbid.lymphoma
)

short_comorbidities = dplyr::vars(
  comorbid.copd,
  comorbid.asthma,
  comorbid.bronchiectasis,
  comorbid.ihd,
  comorbid.hypertension,
  comorbid.diabetes_type,
  comorbid.ckd
)

short_admission_risk = dplyr::vars(
  admission.curb_65_severity_score,
  admission.charlson_comorbidity_index,
  admission.on_immunosuppression
)

short_demographic_comparison = dplyr::vars(
  demog.age,
  demog.age_category,
  demog.age_eligible,
  demog.gender,
  demog.ethnicity,
  demog.care_home_resident,
  demog.smoker,
  !!!short_admission_risk,
  !!!short_comorbidities
)

short_pneumo_comparison = dplyr::vars(
  demog.age,
  demog.age_eligible,
  demog.gender,
  demog.ethnicity,
  demog.care_home_resident,
  demog.smoker,
  !!!short_admission_risk,
  admission.pneumococcal_risk_classification,
  vaccination.pneumovax,
  !!!short_comorbidities,
  comorbid.other_pneumococcal_risks
)

