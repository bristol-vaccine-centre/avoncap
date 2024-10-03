# These mappings work for the
# avoncap-extract, central
# nhs-extract, deltave
# and with some renaming
# avoncap-extract, uad-controls

# they are a good starting point for the
# nhs_extract, pneumococcal


keys_avoncap_central = function() {list(
  "admit" = "{admin.record_number}",
  "consent" = "{admin.consented_record_number}"
)}

#' Core avoncap consent
#'
#' `r .document_mapping(map_avoncap_consent)`
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_consent = function() {list(
  "consented" = .normalise_list(
    admin.consented,
    c("Not approached","Yes","Declined consent"), zeroValue = TRUE
  ),
  "ppc" = .normalise_list(
    admin.pp_consented,
    c("Not approached","Yes","Declined consent"), zeroValue = TRUE
  ),
  "withdrawal" = .normalise_yesno(
    admin.withdrawal
  ),
  "consent_urine" = .normalise_yesno(admin.consent_for_urine),
  "consent_blood" = .normalise_yesno(admin.consent_for_blood),
  "consent_resp_samples1" = .normalise_yesno(admin.consent_for_respiratory_samples)
)}

#' Core avoncap normalisation
#'
#' `r .document_mapping(map_avoncap_central)`
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_central = function() {list(

  # TODO:
  # 1) "did_the_patient_have_respi" = .normalise_yesno, actually 4 options, not clear what they are
  # 2) change this to deal with explicit NA options in the drop downs otherwise we are missing explicit missing values
  # in the dropdowns.
  "record_number" = .normalise_name(admin.record_number),
  "what_was_the_first_surveil" = .normalise_name(admin.first_record_number),

  "ac_study_number" = .normalise_study_id(admin.consented_record_number),
  "nhs_number" = .normalise_ppi(admin.patient_identifier),
  "duplicate" = .normalise_yesno(admin.duplicate),
  "enrollment_date" = .normalise_date(
    admin.enrollment_date
  ),
  "admission_type" = .normalise_list(admission.admission_route, c("Emergency Department","Other")),
  "study_year" = .normalise_name(admin.study_year),
  "file" = .normalise_name(admin.data_file),
  "week_number" = .normalise_name(admin.week_number),

  "c19_diagnosis" = .normalise_list(
    diagnosis.standard_of_care_COVID_diagnosis, c("Pneumonia","LRTI","HF","other resp symptoms")),
  # "acute_illness" = .normalise_yesno(
  #   diagnosis.acute_illness
  # ), # acute illness will always be 1
  # "covid19" = .normalise_yesno(
  #   diagnosis.community_diagnosed_COVID_admission
  # ),
  # "current"= .normalise_yesno(
  #   diagnosis.current_COVID_episode
  # ), COVID19 and current are slight nonsense fields - that spit out what is, in essence, garbage
  "clinical_radio_diagnosis" = .normalise_yesno(
    diagnosis.clinical_or_radiological_LRTI_or_pneumonia
  ),
  # "c19_adm_swab" = .normalise_checkboxes(dplyr::vars(
  #   diagnosis.COVID_positive,
  #   diagnosis.COVID_negative,
  #   diagnosis.no_COVID_test,
  # )), # So this field changed from checkboxes to a list.

  "c19_adm_swab" = .normalise_list(
    diagnosis.admission_swab,
    values = c("COVID-19 positive","COVID-19 negative","Indeterminate","Known community/recent positive","Not performed")
  ),

  # "c19_adm_status" = .normalise_checkboxes(dplyr::vars(
  #   diagnosis.COVID_laboratory_confirmed,
  #   diagnosis.COVID_patient_reported_test,
  #   diagnosis.COVID_clinical_diagnosis,
  #   diagnosis.not_COVID_negative_test,
  #   diagnosis.not_tested_for_COVID
  # )), # c19_adm_status isnt being completed for this round of case control - as it was done so badly the first time around it was nonsense

  "c19_test_type" = .normalise_list(
    diagnosis.test_type,
    values = c("Lateral Flow Only","PCR Confirmed")
  ),
  "qualifying_symptoms_signs" = .normalise_name(
    diagnosis.qualifying_symptoms_signs),
  "cc_critieria" = .normalise_yesno(
    diagnosis.meets_case_control_criteria
  ),
  "cc_pos_date" = .normalise_date(
    diagnosis.first_COVID_positive_swab_date
  ),
  # "exclusion_criteria" = .normalise_yesno(
  #   diagnosis.meets_exclusion_criteria
  # ), # always 0

  #### Demographics ----
  "gender" = .normalise_list(
    demog.gender, c("Male","Female"), referrent="Female"),
  "age_at_admission" = .normalise_double(
    demog.age, limits=c(0,120)),
  "age_march" = .normalise_double(
    demog.age_in_march_2021, limits=c(0,120)),
  "imd" = .normalise_name(
    demog.imd_decile),
  "gp_practice" = .normalise_name(admin.gp_practice_old),
  "gp_practice_drop_down" = .normalise_list(
    admin.gp_practice,
    c("168 Medical Group",
      "Air Balloon Surgery",
      "Almondsbury Surgery",
      "Bedminster Family Practice",
      "Beechwood Medical Practice",
      "Birchwood Medical Practice",
      "Bishopston Medical Practice",
      "Bradley Stoke Surgery",
      "Bridge View Medical",
      "Broadmead Medical Centre",
      "Cadbury Heath Healthcare",
      "Charlotte Keel Medical Practice",
      "Clarence Park Surgery",
      "Clevedon Medical Centre",
      "Close Farm Surgery",
      "Concord Medical Centre",
      "Coniston Medical Practice",
      "Courtside Surgery",
      "East Trees Health Centre",
      "Emersons Green Medical Centre",
      "Fallodon Way Medical Centre",
      "Fireclay Health",
      "Fishponds Family Practice",
      "Frome Valley Medical Centre",
      "Gloucester Road Medical Centre",
      "Graham Road Surgery",
      "Grange Road Surgery",
      "Greenway Community Practice",
      "Hanham Health",
      "Harbourside Family Practice",
      "Hartwood Healthcare",
      "Helios Medical Centre",
      "Heywood Family Practice",
      "Hillview Family Practice",
      "Horfield Hc",
      "Horizon Health Centre",
      "Kennedy Way Surgery",
      "Kingswood Health Centre",
      "Lawrence Hill Health Centre",
      "Leap Valley Medical Centre",
      "Longton Grove Surgery",
      "Maytrees Medical Practice",
      "Mendip Vale Medical Practice",
      "Monks Park Surgery",
      "Montpelier Health Centre",
      "Nightingale Valley Practice",
      "Northville Family Practice",
      "Orchard Medical Centre",
      "Pembroke Road Surgery",
      "Pilning Surgery",
      "Pioneer Medical Group",
      "Portishead Medical Group",
      "Priory Surgery",
      "Sea Mills Surgery",
      "Shirehampton Group Practice",
      "Southmead & Henbury Family Practice",
      "St Mary Street Surgery",
      "Stafford Medical Group",
      "Stockwood Medical Centre",
      "Stoke Gifford Medical Centre",
      "Streamside Surgery",
      "Student Health Service",
      "The Armada Family Practice",
      "The Cedars Surgery",
      "The Crest Family Practice",
      "The Downend Health Group",
      "The Family Practice",
      "The Lennard Surgery",
      "The Merrywood Practice",
      "The Milton Surgery",
      "The Old School Surgery",
      "The Wellspring Surgery",
      "Thornbury Health Centre - Burney",
      "Three Shires Medical Practice",
      "Tudor Lodge Surgery",
      "Tyntesfield Medical Group",
      "Wellington Road Surgery",
      "Wells Road Surgery",
      "West Walk Surgery",
      "Westbury On Trym Primary Care Centre",
      "Whiteladies Medical Group",
      "Winscombe Surgery",
      "Other")
  ),


  "smoking" = .normalise_list(
    demog.smoker, c("Non-smoker","Current","Ex-smoker","Unknown"),codes = c(3,1,2,4)),
  "ethnicity2" = .normalise_list(
    demog.ethnicity, c(
      "White British",
      "White other",
      "Mixed origin",
      "Black",
      "Asian",
      "Other race",
      "Unknown"
    )),
  # sometimes this column is names ethnicity sometimes ethnicity2
  # "ethnicity" = .normalise_list(
  #   demog.ethnicity, c(
  #     "White British",
  #     "White other",
  #     "Mixed origin",
  #     "Black",
  #     "Asian",
  #     "Other race",
  #     "Unknown"
  #   )),


  "care_home" = .normalise_yesno(
    demog.care_home_resident),
  "hapcovid_screening" = .normalise_yesno(
    admission.non_lrtd_hospital_acquired_covid),
  "hospital_covid" = .normalise_yesno(
    admission.hospital_acquired_covid),
  "drugs" = .normalise_checkboxes(dplyr::vars(
    demog.no_drug_abuse,
    demog.alcohol_abuse,
    demog.ivdu_abuse,
    demog.marijuana_abuse,
    demog.other_inhaled_drug_abuse
  )),
  "vaping" = .normalise_list(
    demog.vaping, c("yes","no","unknown")),
  "alc_units" = .normalise_name(
    demog.units_of_alcohol
  ),

  #### Samples and admin ----
  "np_swab" = .normalise_list(admin.np_swab_taken_1, c("yes","pending","no")),
  "adm_np_type" = .normalise_list(admin.np_swab_site_1, c("nose","throat","midturbinate")),
  "np_date" = .normalise_date(admin.np_swab_date_1),
  "days_adm_npswab" = .normalise_double(admin.np_swab_day_since_admission, limits = c(0,Inf)),

  "np_swab_2" = .normalise_list(admin.np_swab_taken_2, c("yes","pending","no")),
  "adm_np_type_2" = .normalise_list(admin.np_swab_site_2, c("nose","throat","midturbinate")),
  "np_date_2" = .normalise_date(admin.np_swab_date_2),
  # no days since admission

  "np_swab_3" = .normalise_list(admin.np_swab_taken_3, c("yes","pending","no")),
  "adm_np_type_3" = .normalise_list(admin.np_swab_site_3, c("nose","throat","midturbinate")),
  "np_date_3" = .normalise_date(admin.np_swab_date_3),
  # no days since admission

  "saliva" = .normalise_list(admin.saliva_sample_taken, c("yes","pending","no")),
  "saliva_date" = .normalise_date(admin.saliva_sample_date),
  "days_adm_saliva" = .normalise_double(admin.saliva_sample_day_since_admission, limits = c(0,Inf)),

  "sputum" = .normalise_list(admin.sputum_sample_taken, c("yes","pending","no")),
  "sputum_date" = .normalise_date(admin.sputum_sample_date),
  "days_adm_sputum" = .normalise_double(admin.sputum_sample_day_since_admission, limits = c(0,Inf)),

  "pt_ad_ur" = .normalise_yesno(admin.urine_sample_needed),
  "adm_ur_taken" = .normalise_list(admin.urine_sample_taken, c("yes","pending","no")),
  "nourine_reason" = .normalise_list(admin.urine_sample_failure_reason, c("No consent","Incontinent","Anuric","Agitated","Dying patient","Other")),
  "adm_np_type_2" = .normalise_list(admin.urine_sample_site, c("Catheter","Non-catheter")),
  "adm_ur_date" = .normalise_date(admin.urine_sample_date),
  "days_adm_urine" = .normalise_double(admin.urine_sample_day_since_admission, limits = c(0,Inf)),

  "adm_serum_tak" = .normalise_list(admin.serum_sample_taken, c("yes","pending","no")),
  "adm_seru_date" = .normalise_date(admin.serum_sample_date),
  "days_adm_serum" = .normalise_double(admin.serum_sample_day_since_admission, limits = c(0,Inf)),

  #### Vaccination fields: ----
  # TODO: Third dose brand is currently messed up with inconsistently transposed brand and date information.
  "contraindication" = .normalise_yesno(vaccination.covid_vaccine_contraindicated),
  "covid19_vax" = .normalise_list(
    vaccination.covid_vaccination, c("Not received","Received","Unknown"), codes=c(2,1,3)
  ),
  "covidvax_date" = .normalise_date(
    vaccination.first_dose_date),
  "covidvax_dose_2" = .normalise_date(
    vaccination.second_dose_date),
  "covidvax_dose_3" = .normalise_date(
    vaccination.third_dose_date),
  "covidvax_dose_4" = .normalise_date(
    vaccination.fourth_dose_date),
  "covidvax_dose_5" = .normalise_date(
    vaccination.fifth_dose_date),
  "covidvax_dose_6" = .normalise_date(
    vaccination.sixth_dose_date),

  "brand_of_covid19_vaccinati" = .normalise_list(
    vaccination.first_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen","Novovax","Bivalent Pfizer","Bivalent Moderna")),
  "covid19vax_brand_2" = .normalise_list(
    vaccination.second_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen","Novovax","Bivalent Pfizer","Bivalent Moderna")),
  "covid19vax_brand_3" =  .normalise_list(
    vaccination.third_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen","Novovax","Bivalent Pfizer","Bivalent Moderna")),
  "covid19vax_brand_4" =  .normalise_list(
    vaccination.fourth_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen","Novovax","Bivalent Pfizer","Bivalent Moderna")),
  "covid19vax_brand_5" =  .normalise_list(
    vaccination.fifth_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen","Novovax","Bivalent Pfizer","Bivalent Moderna")),
  "covid19vax_brand_6" =  .normalise_list(
    vaccination.sixth_dose_brand, c("Pfizer","AZ","unknown","Moderna","Janssen","Novovax","Bivalent Pfizer","Bivalent Moderna")),

  # Time since vaccination
  "c19vaxd1_adm" = .normalise_name(
    admission.time_since_first_vaccine_dose),
  "c19vaxd2_adm" = .normalise_name(
    admission.time_since_second_vaccine_dose),
  "c19vaxd3_adm" = .normalise_name(
    admission.time_since_third_vaccine_dose),
  "c19vaxd4_adm" = .normalise_name(
    admission.time_since_fourth_vaccine_dose),
  "c19vax5_adm" = .normalise_name(
    admission.time_since_fifth_vaccine_dose),
  "c19vax6_adm" = .normalise_name(
    admission.time_since_sixth_vaccine_dose),

  # Flu vaccination
  "flu_date" = .normalise_date(vaccination.last_flu_dose_date),
  "fluvax_adm_d1" = .normalise_name(admission.time_since_last_flu_vaccine_dose),
  "ppv23_date" = .normalise_date(vaccination.last_pneumococcal_dose_date),
  "ppv23vax_adm_d" = .normalise_name(admission.time_since_last_pneumococcal_vaccine_dose),


  #### Genomics ----

  "c19_variant" = .normalise_variant(
    genomic.variant),

  #### Admission dates: ----
  "year" = .normalise_double(
    admission.year, limits = c(2019,2025)),
  # "week_number" = .normalise_double(admission.week, limits = c(1,53)),
  "study_week" = .normalise_double(
    admission.study_week),
  "admission_date" = .normalise_date(
    admission.date),
  "hospital" = .normalise_text_to_factor(admin.hospital, preprocess = toupper, levels = c("NBT","BRI")),

  #### Day 2-7 ----

  "adm_diagnosis" = .normalise_checkboxes(renameToVars = dplyr::vars(
    admission.presumed_CAP_radiologically_confirmed,
    admission.presumed_CAP_clinically_confirmed,
    admission.presumed_CAP_no_radiology,
    admission.presumed_LRTI,
    admission.presumed_Empyema_or_abscess,
    admission.presumed_exacerbation_COPD,
    admission.presumed_exacerbation_non_COPD,
    admission.presumed_congestive_heart_failure,
    admission.presumed_non_infectious_process,
    admission.presumed_non_LRTI
  )),

  #### Admission symptoms signs: ----
  ## TODO: range and data quality checks
  "ics" = .normalise_yesno(
    admission.on_inhaled_corticosteroids),
  "immsup" = .normalise_yesno(
    admission.on_immunosuppression),
  "psi_class" = .normalise_list(
    admission.pneumonia_severity_index_class, c("Class I","Class II","Class III","Class IV","Class V"),ordered=TRUE),
  "crb_test_mai" = .normalise_list(
    admission.curb_65_severity_score,c("0-Very Low","1-Low","2-Moderate","3-Severe","4-Severe","5-Severe"),zeroValue = TRUE,ordered=TRUE),
  "news_2_total" = .normalise_name(
    admission.news2_score),
  "pulse_ox" = .normalise_name(
    admission.oximetry),
  "rr" = .normalise_name(
    admission.respiratory_rate),
  "fio2" = .normalise_name(
    admission.max_oxygen),
  "systolic_bp" = .normalise_name(
    admission.systolic_bp),
  "diastolic_bp" = .normalise_name(
    admission.diastolic_bp),
  "hr" = .normalise_name(
    admission.heart_rate),
  "temperature" = .normalise_list(
    admission.temperature, c("Normal","Fever (T>38.0\u00B0C)","Hypothermia (T< 35.5\u00B0C)","Not recorded"), codes = c(3,1,2,4)),
  "symptom_days_preadmit" = .normalise_double(
    admission.duration_symptoms),
  "previous_infection" = .normalise_yesno_unknown(
    admission.previous_covid_infection),
  "previousinfection_date" = .normalise_date(
    admission.previous_covid_infection_date),
  "c19d_preadm" = .normalise_name(
    admission.time_since_covid_diagnosis),

  "rockwood" = .normalise_name(
    admission.rockwood_score),
  "cci_total_score" = .normalise_name(
    admission.charlson_comorbidity_index),
  "height" = .normalise_name(
    admission.height),
  "weight" = .normalise_name(
    admission.weight),
  "bmi" = .normalise_double(
    admission.BMI, limits=c(15,45)),
  "first_radio" = .normalise_checkboxes(dplyr::vars(
    admission.cxr_normal,
    admission.cxr_pneumonia,
    admission.cxr_heart_failure,
    admission.cxr_pleural_effusion,
    admission.cxr_covid_changes,
    admission.cxr_other
  )),



  #### Day 7 follow up ----
  "c19_peep" = .normalise_name(
    day_7.max_peep),
  "c19_hospadm" = .normalise_list(
    day_7.length_of_stay, c("0 day","1 day","2 days","3 days","4 days","5 days","6 days","7 days","still inpatient"),ordered=TRUE,zeroValue = TRUE),
  "c17_high" = .normalise_list(
    day_7.max_care_level, c("Routine ward","High care area","ICU/HDU")),
  "c19icuon" = .normalise_yesno(
    day_7.still_on_icu),
  "c19_icudays" = .normalise_list(
    day_7.icu_length_of_stay, c("0 day","1 day","2 days","3 days","4 days","5 days","6 days","7 days"),ordered=TRUE,zeroValue = TRUE),
  "c19_vent" = .normalise_list(
    day_7.max_ventilation_level, c("None","Oxygen","HFNO","NIV (CPAP/BiPAP)","ETT"),ordered=TRUE),
  "c19_ox" = .normalise_list(
    day_7.max_o2_level, c("room air","24-28%","30-35%","40%","50%","60%","80%","Over 80%"),ordered=TRUE),
  "c19_ionotropes" = .normalise_yesno(
    day_7.ionotropes_needed),
  "c19_complication" = .normalise_checkboxes(dplyr::vars(
    day_7.PE,
    day_7.DVT,
    day_7.ARF,
    day_7.NSTEMI,
    day_7.STEMI,
    day_7.cardiac_failure,
    day_7.new_AF,
    day_7.new_other_arrythmia,
    day_7.inpatient_fall,
    day_7.other_complication,
    day_7.no_complication
  )),
  "c19_death7d" = .normalise_yesno(
    day_7.death),
  "c19_meds" = .normalise_checkboxes(dplyr::vars(
    treatment.dexamethasone,
    treatment.remdesevir,
    treatment.tocilizumab,
    treatment.sarilumab,
    treatment.in_drug_trial,
    treatment.no_drug_treatment,
    treatment.sotrovimab
  )),

  #### Long term follow up ----
  "hospital_length_of_stay" = .normalise_integer(
    outcome.length_of_stay, convert_fn=floor),
  "survival_days" = .normalise_integer(
    outcome.survival_duration, convert_fn=round),
  "ip_death" = .normalise_yesno(
    outcome.inpatient_death),
  "days_in_icu" = .normalise_double(
    outcome.icu_duration),
  "did_the_patient_have_respi" = .normalise_yesno(
    outcome.respiratory_support_needed),
  "number_of_days_of_ventilat" = .normalise_double(
    outcome.ventilator_duration),
  "ett_days" = .normalise_double(
    outcome.endotracheal_tube_duration),
  "renal_replacement_therapy" = .normalise_double(
    outcome.renal_support_duration),
  "complications" = .normalise_checkboxes(dplyr::vars(
    outcome.acute_renal_failure,
    outcome.liver_dysfunction,
    outcome.hospital_acquired_infection,
    outcome.acute_respiratory_distress_syndrome,
    outcome.NSTEMI,
    outcome.STEMI,
    outcome.new_AF,
    outcome.new_other_arrhthmia,
    outcome.stroke,
    outcome.DVT,
    outcome.PE,
    outcome.heart_failure,
    outcome.fall_in_hospital,
    outcome.reduced_mobility,
    outcome.increasing_care_requirement,
    outcome.no_complications
  )),
  "ventilatory_support" = .normalise_list(
    outcome.highest_level_ventilatory_support, c("Intubation","BiPAP","CPAP","High-Flow Nasal Cannulae","None"),ordered=TRUE
  ),
  "did_the_patient_receive_ec" = .normalise_yesno(outcome.received_ecmo),
  "inotropic_support_required" = .normalise_yesno_unknown(outcome.received_ionotropes),
  "lrtd_30d_outcome" = .normalise_list(
    outcome.functional_status,c(
      "Deceased",
      "Recovered",
      "Recovered with long term sequelae",
      "Ongoing recovery",
      "Not recovered",
      "Unknown"
    )
  ),
  "survive_1yr" = .normalise_yesno(outcome.one_year_survival),
  "survival_1yr_days" = .normalise_integer(outcome.one_year_survival_duration, limits = c(0,366)),
  "yr_survival_complete" = .normalise_list(outcome.one_year_survival_complete, values = c("Incomplete","Unverified","Complete")),

  #### Symptoms ----
  "fever2" = .normalise_yesno(symptom.abnormal_temperature),
  "pleurtic_cp" = .normalise_yesno(symptom.pleuritic_chest_pain),
  "cough2" = .normalise_yesno(symptom.cough),
  "sput_prod" = .normalise_yesno(symptom.productive_sputum),
  "dyspnoea" = .normalise_yesno(symptom.dyspnoea),
  "tachypnoea2" = .normalise_yesno(symptom.tachypnoea),
  "confusion" = .normalise_yesno(symptom.confusion),

  "anosmia" = .normalise_yesno_unknown(symptom.anosmia),
  "ageusia" = .normalise_yesno_unknown(symptom.ageusia),
  "dysgeusia" = .normalise_yesno_unknown(symptom.dysguesia),
  "fever" = .normalise_yesno_unknown(symptom.fever),
  "hypothermia" = .normalise_yesno_unknown(symptom.hypothermia),
  "chills" = .normalise_yesno_unknown(symptom.chills),
  "headache" = .normalise_yesno_unknown(symptom.headache),
  "malaise" = .normalise_yesno_unknown(symptom.malaise),
  "wheeze" = .normalise_yesno_unknown(symptom.wheeze),
  "myalgia" = .normalise_yesno_unknown(symptom.myalgia),

  "worse_confusion" = .normalise_yesno_unknown(symptom.worsening_confusion),
  "general_det" = .normalise_yesno_unknown(symptom.general_deterioration),
  "ox_on_admission" = .normalise_yesno_unknown(symptom.oxygen_required_on_admission),

  #### Comorbidities ----
  "resp_disease" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_resp_dx,
    comorbid.copd,
    comorbid.asthma,
    comorbid.resp_other,
  )),
  "other_respiratory_disease" = .normalise_checkboxes(dplyr::vars(
    comorbid.bronchiectasis,
    comorbid.interstitial_lung_dx,
    comorbid.cystic_fibrosis,
    comorbid.pulmonary_hypertension,
    comorbid.chronic_pleural_dx,
    comorbid.other_chronic_resp_dx,
  )),
  "chd" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_heart_dx,
    comorbid.ccf,
    comorbid.ihd,
    comorbid.hypertension,
    comorbid.other_heart_dx
  )),
  "mi" = .normalise_yesno(
    comorbid.previous_mi
  ),
  "other_chd" = .normalise_checkboxes(dplyr::vars(
    comorbid.congenital_heart_dx,
    comorbid.af,
    comorbid.other_arrythmia,
    comorbid.pacemaker,
    comorbid.valvular_heart_dx,
    comorbid.other_other_heart_dx
  )),
  "diabetes" = .normalise_list(
    comorbid.diabetes, c(
      "None","Type 1 - no complications","Type 1 - complications",
      "Type 2 - no complications","Type 2 - complications"
    )),
  "dm_meds" = .normalise_list(
    comorbid.diabetes_medications, c(
      "Oral","Insulin"
    )),
  "neurological_disease" = .normalise_checkboxes(dplyr::vars(
    comorbid.neuro_other,
    comorbid.cva,
    comorbid.tia,
    comorbid.hemiplegia,
    comorbid.paraplegia,
    comorbid.no_neuro_dx
  )),
  "dementia" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_dementia,
    comorbid.dementia,
    comorbid.cognitive_impairment
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
  "ckd" = .normalise_list(
    comorbid.ckd, c("None", "Mild (CKD 1-3)","Moderate or Severe CKD (CKD 4+)")),
  "liver_disease" = .normalise_list(
    comorbid.liver_disease, c("None","Liver disease without failure","Liver disease with failure")),
  "gastric_ulcers" = .normalise_yesno(
    comorbid.gastric_ulcers),
  "pvd" = .normalise_yesno(
    comorbid.periph_vasc_dx),
  "ctd" = .normalise_yesno(
    comorbid.connective_tissue_dx),
  "immunodeficiency" = .normalise_yesno(
    comorbid.immunodeficiency),
  "other_pn_disease" = .normalise_yesno(
    comorbid.other_pneumococcal_risks),
  "transplant" = .normalise_yesno(
    comorbid.transplant_recipient),
  "pregnancy" = .normalise_list(
    comorbid.pregnancy, c("Not pregnant","First Trimester","Second Trimester",
                          "Third Trimester","unsure of trimester","Post-partum")),
  "hiv" = .normalise_checkboxes(dplyr::vars(
    comorbid.no_HIV,
    comorbid.HIV,
    comorbid.AIDS
  )),
  ## NHS data set only mappings ----
  "final_soc_lrtd_diagnosis" = .normalise_checkboxes(renameToVars = dplyr::vars(
    diagnosis.SOC_CAP_radiologically_confirmed,
    diagnosis.SOC_CAP_clinically_confirmed,
    diagnosis.SOC_CAP_no_radiology,
    diagnosis.SOC_LRTI,
    diagnosis.SOC_Empyema_or_abscess,
    diagnosis.SOC_exacerbation_COPD,
    diagnosis.SOC_exacerbation_non_COPD,
    diagnosis.SOC_congestive_heart_failure,
    diagnosis.SOC_non_infectious_process,
    diagnosis.SOC_non_LRTI
  )),
  "covid_19_diagnosis" = .normalise_list(
    diagnosis.covid_19_diagnosis,
    c("COVID-19 - laboratory confirmed","COVID-19 - patient reported test", "COVID-19 - clinical diagnosis (but negative test)",
      "COVID-19 - negative test, unlikely COVID-19 disease","No test performed" )
  ),

  # This has AFAIK become irrelevant.
  # "c19_adm_swab" = .normalise_checkboxes_to_list(
  #   diagnosis.admission_swab_old,
  #   values = c("COVID-19 positive","COVID-19 negative","Indeterminate","Known community/recent positive","Not performed")
  # ),

  ## Preadmission therapy ----

  "ppv23" = .normalise_list(
    vaccination.pneumovax,
    c("Not received","PPV23","PCV13","Unknown"), codes=c(2,1,4,3)
  ),

  "flu_vaccine" = .normalise_list(
    vaccination.influenza_vaccination,
    c("Not received","Received","Unknown"), codes=c(2,1,3)
  ),

  "abx_14d_prior" = .normalise_yesno_unknown(admission.pre_admission_antibiotics_given),
  "antibiotic_used" = .normalise_checkboxes_to_nested_list(
    admission.pre_admission_antibiotic,
    c("Amoxicillin",
        "Co-amoxiclav (Augmentin)",
        "Doxycycline",
        "Trimethoprim",
        "Nitrofurantoin",
        "Clarithromycin/Erythromycin",
        "Metronidazole",
        "Septrin (Co-trimoxazole)",
        "Penicillin V",
        "Benzylpenicillin (BenPen)",
        "Tazocin",
        "Meropenem",
        "Other"), nameCol = "antibiotic", valueCol = "given"
    ),

    # TODO: antivirals but they are in different format.
    # TODO: readmissions? - 4 possible blocks?

  "antiplatelets" = .normalise_list(admission.antiplatelet_therapy, c("no","yes","unknown"), referrent = "no", explicitUnavailable = "unknown"),
  "anticoagulants" = .normalise_list(admission.anticoagulant_therapy, c("no","yes","unknown"), referrent = "no", explicitUnavailable = "unknown"),
  "statins" = .normalise_list(admission.cholesterol_lowering_therapy, c("no","yes","unknown"), referrent = "no", explicitUnavailable = "unknown"),
  "hypertensives" = .normalise_list(admission.antihypertensive_therapy, c("no","yes","unknown"), referrent = "no", explicitUnavailable = "unknown"),

  "antiviral_14d_prior" = .normalise_checkboxes_to_nested_list(
    admission.pre_admission_antiviral,
    c(
      "Other",
      "Oseltamivir or Zanamivir",
      "Molnupiravir",
      "Sotrovimab",
      "Paxlovid"
    ), nameCol = "antiviral", valueCol = "given"
  )

)}
