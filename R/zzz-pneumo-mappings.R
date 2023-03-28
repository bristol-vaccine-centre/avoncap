
keys_avoncap_pneumococcal  = function(instrument) {
  list(
    "admit" = "{admin.record_number}"
  )
}

#' Normalise the avoncap pneumococcal data
#'
#' `r .document_mapping(map_avoncap_pneumococcal)`
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_pneumococcal = function() list(

  "participant_number" = .normalise_name(admin.record_number),
  "hospital" = .normalise_list(admin.hospital, c("BRI", "Southmead", "RUH")),
  "nhs_number"  = .normalise_ppi(admin.patient_identifier),
  "age_at_admission" = .normalise_double(demog.age, limits=c(0,120)),
  "sex" = .normalise_list(demog.gender, c("Male","Female")),
  # "age_group",
  #"sample_id",
  "test_date" = .normalise_date(pneumo.test_date, limits=as.Date(c("2000-01-01","2030-01-01"))),
  # "year",
  "test" = .normalise_list(pneumo.test_type, c("Blood culture only","Binax only","Blood culture and Binax","CSF PCR","Blood PCR")),
  "serotype" = .normalise_pneumo_serotype(pneumo.phe_serotype),
  # "pcv13_serotype",
  # "pcr_sample",
  "smoker" = .normalise_list(demog.smoker, c("Non-smoker", "Ex-smoker", "Current smoker"), zeroValue = TRUE),
  "resp_disease" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_resp_dx,
    comorbid.copd,
    comorbid.asthma,
    comorbid.bronchiectasis,
    comorbid.pulmonary_fibrosis,
    comorbid.resp_other,
  )),
  "chd" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_heart_dx,
    comorbid.ccf,
    comorbid.ihd,
    comorbid.hypertension,
    comorbid.af,
    comorbid.other_heart_dx,
  )),
  "mi" = .normalise_yesno(comorbid.previous_mi),
  # "spec_other_cardiac",
  "ckd" = .normalise_list(
    comorbid.ckd, c("None", "Mild (CKD 1-3)","Moderate or Severe CKD (CKD 4+)")),
  "liver_disease" = .normalise_list(
    comorbid.liver_disease, c("None","Liver disease without failure","Liver disease with failure")),
  "diabetes" = .normalise_list(
    comorbid.diabetes, c(
      "None","Type 1 - no complications","Type 1 - complications",
      "Type 2 - no complications","Type 2 - complications"
    )),
  "dm_meds" = .normalise_list(
    comorbid.diabetes_medications, c(
      "Oral","Insulin"
    )),
  "dementia" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_dementia,
    comorbid.dementia,
    comorbid.cognitive_impairment
  )),
  "neurological_disease" = .normalise_checkboxes(dplyr::vars(
    comorbid.neuro_other,
    comorbid.cva,
    comorbid.tia,
    comorbid.hemiplegia,
    comorbid.paraplegia,
    comorbid.no_neuro_dx
  )),
  "gastric_ulcers" = .normalise_yesno(
    comorbid.gastric_ulcers),
  "dysphagia" = .normalise_yesno(
    comorbid.dysphagia),
  # "dysphagia_type",
  "pvd" = .normalise_yesno(
    comorbid.periph_vasc_dx),
  "ctd" = .normalise_yesno(
    comorbid.connective_tissue_dx),
  "immunodeficiency" = .normalise_yesno(
    comorbid.immunodeficiency),
  "other_pn_disease" = .normalise_yesno(
    comorbid.other_pneumococcal_risks),
  "hiv" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_HIV,
    comorbid.HIV,
    comorbid.AIDS
  )),
  "cancer" = .normalise_list(
    comorbid.solid_cancer, c(
      "None", "Solid Organ Cancer - no mets", "Solid Organ Cancer - Metastatic Disease"
    )),
  "haem_malig" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_haemotological_cancer,
    comorbid.leukaemia,
    comorbid.lymphoma
  )),
  # "cancer_type",
  "recent_chemo" = .normalise_yesno(comorbid.recent_chemotherapy),
  "recent_radiotherapy" = .normalise_yesno(comorbid.recent_radiotherapy),
  "transplant" = .normalise_yesno(comorbid.transplant_recipient),
  "pregnancy" = .normalise_list(
    comorbid.pregnancy, c("Not pregnant","First Trimester","Second Trimester",
                          "Third Trimester","unsure of trimester","Post-partum")),

  "drugs" = .normalise_checkboxes(dplyr::vars(
    demog.no_drug_abuse,
    demog.alcohol_abuse,
    demog.ivdu_abuse,
    demog.marijuana_abuse,
    demog.other_inhaled_drug_abuse
  )),
  "immsup" = .normalise_yesno(
    admission.on_immunosuppression),
  "weight_problem" = .normalise_list(comorbid.bmi_status, c("Normal", "Low BMI (ie underweight)", "High BMI (ie overweight)")
),
  "concomittant_flu" = .normalise_yesno(comorbid.influenza_infection),
  "hcv" = .normalise_yesno(comorbid.hepatitis_c),
  #"atypical_infection",
  #"spec_other_infection",
  "ppv23" = .normalise_list(vaccination.ppv23_vaccination, c("No", "Under 6 months", "Over 6 months"),zeroValue = TRUE),
  "flu_vaccine" = .normalise_list(vaccination.flu, c("No", "Under 6 months", "Over 6 months"),zeroValue = TRUE),
  # "cci_age_score",
  # "mi_score",
  # "chf_score",
  # "pvd_score",
  # "neuro_score",
  # "dementia_score",
  # "copd_score",
  # "peptic_ulcer_score",
  # "ctd_score",
  # "liver_disease_score",
  # "diabetes_score",
  # "neuro_disease_score",
  # "ckd_score",
  # "cancer_score",
  # "leukemia_score",
  # "lymphoma_score",
  # "hiv_score",
  "cci_total_score" = .normalise_name(admission.charlson_comorbidity_index),
  "los_days" = .normalise_double(outcome.length_of_stay, c(0,Inf)),
  "amts" = .normalise_list(admission.triage_score, c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Confused", "Too distressed to assess", "Not applicable or not assessed")),
  "resp_rate" = .normalise_double(admission.respiratory_rate, c(10,30)),
  "sats_ra" = .normalise_double(admission.saturations_on_room_air),
  "systolic_bp" = .normalise_double(admission.systolic_bp),
  "diastolic_bp" = .normalise_double(admission.diastolic_bp),
  # "age_65_plus",
  #"confusion" ,
  "crb65_score"  = .normalise_double(admission.crb65),
  "curb65_score"  = .normalise_double(admission.curb65),
  "antibiotic_route" = .normalise_list(outcome.antibiotic_route, c("no antibiotic prescribed", "oral (PO)", "intravenous (IV)")),
  # This needs a bit of further processing as is often a combination of Abx.
  # "initial_antibiotic" = .normalise_text_to_factor(admission.first_antibiotic),
  # "rationalised_antibiotic" = .normalise_text_to_factor(admission.final_antibiotic),
  "antibiotic_days"  = .normalise_double(outcome.antibiotic_duration),
  "infection_site" = .normalise_list(admission.infection_site, c("Lung", "Meningitis", "Septic arthritis", "ENT", "Myositis", "Otitis externa", "Abdominal", "Unclear/unknown", "Other")),
  # "site_other",
  # "complications",
  "deranged_lfts" = .normalise_yesno(outcome.abnormal_lft),
  "aki" = .normalise_yesno(outcome.acute_kidney_injury),
  "pleural_effusion" = .normalise_yesno(outcome.pleural_effusion),
  "empyema" = .normalise_yesno(outcome.empyema),
  # "other_complication",
  # "spec_other_complication",
  "discharge_destination" = .normalise_list(outcome.discharge_to, c("Home", "Died as inpatient", "Home but needed increased POC", "Transferred to another hospital", "Rehab", "EOL care")),
  "icu" = .normalise_yesno(outcome.admitted_icu),
  "niv" = .normalise_yesno(outcome.non_invasive_ventilation),
  "intubation" = .normalise_yesno(outcome.intubation),
  "recurrent_pneumonia" = .normalise_yesno(outcome.recurrent_pneumonia),
  "ecmo" = .normalise_yesno(outcome.received_ecmo),
  "inotropes" = .normalise_yesno(outcome.received_ionotropes),
  "trachy" = .normalise_yesno(outcome.tracheostomy),
  "inpatient_death" = .normalise_yesno(outcome.inpatient_death),
  "death_30days" = .normalise_yesno(outcome.death_within_30_days),
  "death_1year" = .normalise_yesno(outcome.death_within_1_year),
  # "death_date",
  "survival_days" = .normalise_name(outcome.survival_duration),

  # effusion details?
  # "loculation",
  # "pus",
  # "effusion_ha",
  # "fibrinolytic",
  # "surgery",
  # "pf_pn_positive",
  # "pf_mulit_organism",
  # "rapid_hai_score",
  # "rapid_pus_score",
  # "rapid_albumin_score",
  # "rapid_urea_score",
  # "rapid_age_score",
  # "rapid_score",
  # "rapid_group",

  "albumin" = .normalise_double(haem.albumin),
  "wcc" = .normalise_double(haem.white_cell_count),
  "hb" = .normalise_double(haem.haemoglobin),
  "pmn" = .normalise_double(haem.neutrophils),
  "lymphocytes" = .normalise_double(haem.lymphocytes),
  "crp" = .normalise_double(haem.crp),
  "na_result" = .normalise_double(haem.sodium),
  "ur_result" = .normalise_double(haem.urea),
  "egfr" = .normalise_double(haem.egfr),

  "creatinine" = .normalise_double(haem.creatinine),

  "cxr_sides" = .normalise_list(radio.cxr_infection, c("No change on CXR", "Unilateral infection", "Bilateral infection", "No CXR performed")),
  "cxr_lobes" = .normalise_list(radio.cxr_lobar_changes, c("No change on CXR", "Unilobar", "Multilobar", "No CXR performed")),
  # "cxr_specify",
  "death_5year" = .normalise_yesno(outcome.death_within_5_years),
  # "death_date_5yr",
  "survival_days_2" = .normalise_name(outcome.5_yr_survival_duration),
  # "imd_rank",
  "imd_decile" = .normalise_name(demog.imd_decile)


)


