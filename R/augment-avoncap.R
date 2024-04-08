# IMPORTANT:
# any new functions in this file need to
# added to the `dispatch-augment.R` in the appropriate place.

# TODO:
# derive_variant_probability - list variants of interest. calculate probability
# using multinomial (with other category) using COGUK data.

# derive_herd_immunity - probability of vaccination. calculate probability
# using multinomial (with other category) using COGUK data. community_S-antigen_levels

# derive_natural_immunity_probability - N-gene antigen probability from SPI-M data?
# age and time locfit model?

# derive_force_of_infection - growth rates from case data matched to time.

#' Polyfill data
#'
#' Some basic context to allow comparison to ED data.
#'
#' * All of the patients admitted
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_polyfill_central = function(df,v,...) {
  df %>%
    dplyr::mutate(

      outcome.admitted_to_hospital = factor("yes", levels=c("no","yes")),

    )
}


# Admin ----

#' Categorical scores for continuous variables
#'
#' Typically used in regression models with non-linear effects over splines
#'
#' * Age category - UK demographic data ends at 85, and 65 key cut off in 5 year
#' bands, so 10 year bands age categories end at 85 (N.b.) there is a more
#' principled reason here. Boundaries fall approx 0.1, 0.2, 0.4, 0.6, 0.8
#' quantiles. Could merge first two groups but outcomes are usually different.
#' Covid vaccination cohorts were in 5 year age groups, but vaccination
#' prioirity was in these groups approximately.
#' * Age of eligibility for vaccines: 65+ Age of pneumovax eligibility
#' * CCI - 4 bands as defined in original Charleson paper:
#' ** https://pubmed.ncbi.nlm.nih.gov/3558716/
#' ** in https://link.springer.com/article/10.1007/s10654-021-00802-z there is
#' rationale given for not using the charleson score as a continuous value.
#' * Alternate CCI - 0,1,2,3+ is also used as a grouping in the original charleson
#' paper
#' * Rockwood score - Completely independent versus dependent frailty levels.
#' * CURB65 categorisation - As per derivation study (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1746657/): 0-1 consider home treatment; 2 consider
#' admit as inpatient; 3-5 admit, consider ICU.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_continuous_categories = function(df,v,...) {
  df %>%
    dplyr::mutate(
      demog.age_category = cut(demog.age,breaks = c(0,35,50,65,75,85,Inf), labels = c("18-34","35-49","50-64","65-74","75-84","85+"), include.lowest = FALSE, ordered_result = TRUE),
      demog.age_eligible = cut(demog.age,breaks = c(0,65,Inf), labels = c("18-64","65+"),ordered_result = TRUE)
    ) %>%
    dplyr::mutate(
      admission.cci_category =  dplyr::case_when(
        is.na(admission.charlson_comorbidity_index) ~ NA_character_,
        admission.charlson_comorbidity_index == 0 ~ "None (0)",
        admission.charlson_comorbidity_index <= 2 ~ "Mild (1-2)",
        admission.charlson_comorbidity_index <= 4 ~ "Moderate (3-4)",
        admission.charlson_comorbidity_index > 4 ~ "Severe (5+)",
        TRUE ~ NA_character_
      ) %>% ordered(labels=c("None (0)","Mild (1-2)","Moderate (3-4)","Severe (5+)")),
      admission.cci_category_alternate =  dplyr::case_when(
        is.na(admission.charlson_comorbidity_index) ~ NA_character_,
        admission.charlson_comorbidity_index == 0 ~ "0",
        admission.charlson_comorbidity_index == 1 ~ "1",
        admission.charlson_comorbidity_index == 2 ~ "2",
        admission.charlson_comorbidity_index >= 3 ~ "3+",
        TRUE ~ NA_character_
      ) %>% ordered(labels=c("0","1","2","3+")),
      admission.rockwood_category = dplyr::case_when(
        is.na(admission.rockwood_score) ~ NA_character_,
        admission.rockwood_score <= 4 ~ "Independent (0-4)",
        admission.rockwood_score > 4 ~ "Frail (5-9)",
        TRUE ~ NA_character_
      ) %>% ordered(c("Independent (0-4)","Frail (5-9)")),
      admission.curb_65_category = dplyr::case_when(
        admission.curb_65_severity_score == "0-Very Low" ~ "0-1 (Mild)",
        admission.curb_65_severity_score == "1-Low" ~ "0-1 (Mild)",
        admission.curb_65_severity_score == "2-Moderate" ~ "2 (Moderate)",
        admission.curb_65_severity_score == "3-Severe" ~ "3-5 (Severe)",
        admission.curb_65_severity_score == "4-Severe" ~ "3-5 (Severe)",
        admission.curb_65_severity_score == "5-Severe" ~ "3-5 (Severe)"
      ) %>% ordered(c("0-1 (Mild)","2 (Moderate)","3-5 (Severe)"))
    )
}

#' Create a unique patient level id (if it does not already exist)
#'
#' The patient identifier is derived from the record number or the first record
#' number (ensuring it matches) an entry in the record number. This deals with
#' multiple admissions in the data set. In the patient identifiable NHS data
#' this is the NHS number.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_patient_identifier = function(df,v) {
  if ("admin.patient_identifier" %in% colnames(df)) return(df)
  valid = stats::na.omit(intersect(df$admin.first_record_number, df$admin.record_number))
  df %>%
    dplyr::mutate(admin.patient_identifier = ifelse(
      admin.first_record_number %in% valid,
      admin.first_record_number,
      admin.record_number
    ))
}

#' Create a counter in the event of repeated admissions
#'
#' This also will calculate a time interval between admissions.
#' There is also a repeat admission instrument that this does not use.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_admission_episode = function(df,v) {
  df %>%
    dplyr::group_by(admin.patient_identifier) %>%
    dplyr::arrange(admission.date) %>%
    dplyr::mutate(
      admission.episode = dplyr::row_number(),
      admission.interval = as.integer(admission.date-dplyr::lag(admission.date))
    ) %>%
    dplyr::ungroup()
}

#' Identify patients from the GP surgeries in linked primary care study
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_gp_linkage = function(df,v) {
  df %>% dplyr::mutate(
    admin.linked_gp_practice = dplyr::case_when(
      is.na(admin.gp_practice) ~ "Not recorded",
      admin.gp_practice %in%
        c("Concord Medical Centre","Courtside Surgery","Montpelier Health Centre",
          "The Wellspring Surgery","Tyntesfield Medical Group","Pioneer Medical Group")
      ) ~ as.character(admin.gp_practice),
      TRUE ~ "Other specified"
    ) %>% factor(levels = c(
      "Concord Medical Centre","Courtside Surgery","Montpelier Health Centre",
      "The Wellspring Surgery","Tyntesfield Medical Group","Pioneer Medical Group",
      "Other specified","Not recorded"))
}

#' Identify patients who are in the BNSSG ICB based on their GP practice name
#'
#' Names are normalised by removing commonly mixed up components and
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_catchment_status = function(df,v) {
  df %>% dplyr::mutate(
    admin.catchment_status = dplyr::case_when(
      is.na(admin.gp_practice) ~ "No GP details",
      admin.gp_practice == "Other" ~ "Outside Bristol area",
      TRUE ~ "Bristol area"
    ) %>% factor(levels = c("Bristol area","Outside Bristol area","No GP details"))
  )
}

.normalise_gp_name = function(practice_name = avoncap::icb_surgeries$name) {
  practice_name %>% tolower() %>%
    ifelse(. == "the family practice","tfp",.) %>%
    stringr::str_replace_all("[^a-z0-9\\s].*$","") %>%
    stringr::str_remove_all("the|surgery|practice|family|way|road|centre|health|medical|fam|prac|group|hc|community|primary|care") %>%
    stringr::str_replace_all("[^a-z0-9\\s]","") %>%
    stringr::str_replace_all("\\s+"," ") %>%
    trimws()
}

#' Rationalise some of the more detailed comorbidities
#'
#' and generate some summary values
#'
#' * simple DM without insulin dependence
#' * Solid / Haematological / Any cancer present binary indicators
#' * any chronic resp dx: i.e. any of asthma, bronchiectasis, chronic pleural disease, COPD,
#' interstitial lung dx, cyctic fibrosis, other chronic resp dx
#' * any chronic heart disease: pulmonary htn, CCF, IHD, previous MI, congential
#' heart dx, hypertension, AF, other arrythmia, other heart dx, other other heart dx
#' * Stroke or TIA binary
#' * Any immune compromise binary (immunodeficient or on immune suppressants)
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_simpler_comorbidities = function(df,v,...) {
  df %>% dplyr::mutate(
    comorbid.diabetes_type = dplyr::case_when(
      comorbid.diabetes == v$comorbid.diabetes$None ~ "None",
      comorbid.diabetes == v$comorbid.diabetes$`Type 1 - no complications` ~ "Type 1",
      comorbid.diabetes == v$comorbid.diabetes$`Type 1 - complications` ~ "Type 1",
      comorbid.diabetes == v$comorbid.diabetes$`Type 2 - no complications` ~ "Type 2",
      comorbid.diabetes == v$comorbid.diabetes$`Type 2 - complications` ~ "Type 2",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("None","Type 1","Type 2")),
    comorbid.solid_cancer_present = dplyr::case_when(
      comorbid.solid_cancer == v$comorbid.solid_cancer$None ~ "no",
      comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - no mets` ~ "yes",
      comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - Metastatic Disease` ~ "yes",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("no","yes")),
    comorbid.haemotological_cancer_present = dplyr::case_when(
      comorbid.leukaemia == v$comorbid.leukaemia$yes ~ "yes",
      comorbid.lymphoma == v$comorbid.lymphoma$yes ~ "yes",
      comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no ~ "yes",
      TRUE ~ "no"
    ) %>% factor(levels = c("no","yes")),
    comorbid.any_cancer_present = dplyr::case_when(
      comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - no mets` ~ "yes",
      comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - Metastatic Disease` ~ "yes",
      comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no ~ "yes",
      comorbid.leukaemia == v$comorbid.leukaemia$yes ~ "yes",
      comorbid.lymphoma == v$comorbid.lymphoma$yes ~ "yes",
      TRUE ~ "no"
    ) %>% factor(levels = c("no","yes")),
    comorbid.any_chronic_lung_disease = dplyr::case_when(
      comorbid.asthma == v$comorbid.asthma$yes ~ "yes",
      comorbid.bronchiectasis == v$comorbid.bronchiectasis$yes ~ "yes",
      comorbid.chronic_pleural_dx == v$comorbid.chronic_pleural_dx$yes ~ "yes",
      comorbid.copd == v$comorbid.copd$yes ~ "yes",
      comorbid.interstitial_lung_dx == v$comorbid.interstitial_lung_dx$yes ~ "yes",
      comorbid.cystic_fibrosis == v$comorbid.cystic_fibrosis$yes  ~ "yes",
      comorbid.other_chronic_resp_dx == v$comorbid.other_chronic_resp_dx$yes ~ "yes",
      TRUE ~ "no"
    ) %>% factor(c("no","yes")),
    comorbid.any_chronic_heart_disease = dplyr::case_when(
      comorbid.pulmonary_hypertension == v$comorbid.pulmonary_hypertension$yes ~ "yes",
      comorbid.ccf == v$comorbid.ccf$yes ~ "yes",
      comorbid.ihd == v$comorbid.ihd$yes ~ "yes",
      comorbid.previous_mi == v$comorbid.previous_mi$yes ~ "yes",
      comorbid.congenital_heart_dx == v$comorbid.congenital_heart_dx$yes ~ "yes",
      comorbid.hypertension == v$comorbid.hypertension$yes ~ "yes",
      comorbid.af == v$comorbid.af$yes ~ "yes",
      comorbid.other_arrythmia == v$comorbid.other_arrythmia$yes ~ "yes",
      comorbid.other_heart_dx == v$comorbid.other_heart_dx$yes ~ "yes",
      comorbid.other_other_heart_dx == v$comorbid.other_other_heart_dx$yes ~ "yes",
      TRUE ~ "no"
    ) %>% factor(c("no","yes")),
    comorbid.cva_or_tia = dplyr::case_when(
      comorbid.cva == v$comorbid.cva$yes ~ "yes",
      comorbid.tia == v$comorbid.tia$yes ~ "yes",
      TRUE ~ "no"
    ) %>% factor(c("no","yes")),
    comorbid.any_immune_compromise = dplyr::case_when(
      comorbid.immunodeficiency == "yes" ~ "yes",
      admission.on_immunosuppression == "yes" ~ "yes",
      TRUE ~ "no"
    ) %>% factor(c("no","yes"))
  )
}

# Admission ----

#' Create presumed diagnostic categories
#'
#' Pneumonia if one of:
#' * Initial diagnosis of CAP (supported by initial radiology or clinically)
#' * Empyema or abscess
#'
#' Presumed clinical presentation:
#'   * Pneumonia - implies Infective
#'   * NP-LRTI - implies Infective
#'   * No evidence LRTI (include CRDE and HF)
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_presumed_diagnosis_categories = function(df,v) {
  df %>% dplyr::mutate(
    admission.initial_presentation_3_class = dplyr::case_when(
      admission.presumed_CAP_clinically_confirmed == v$admission.presumed_CAP_clinically_confirmed$yes ~ "Pneumonia",
      admission.presumed_CAP_radiologically_confirmed == v$admission.presumed_CAP_radiologically_confirmed$yes ~ "Pneumonia",
      admission.presumed_CAP_no_radiology == v$admission.presumed_CAP_no_radiology$yes ~ "Pneumonia",
      admission.presumed_Empyema_or_abscess == v$admission.presumed_Empyema_or_abscess$yes ~ "Pneumonia",
      # not sure: (!is.na(admission.cxr_pneumonia) & admission.cxr_pneumonia == v$admission.cxr_pneumonia$yes) ~ "Pneumonia",
      admission.presumed_LRTI == v$admission.presumed_LRTI$yes ~ "NP-LRTI",
      admission.presumed_exacerbation_COPD == v$admission.presumed_exacerbation_COPD$yes ~ "No evidence LRTI",
      admission.presumed_exacerbation_non_COPD == v$admission.presumed_exacerbation_non_COPD$yes ~ "No evidence LRTI",
      admission.presumed_congestive_heart_failure == v$admission.presumed_congestive_heart_failure$yes ~ "No evidence LRTI",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Pneumonia","NP-LRTI","No evidence LRTI"))
    #TODO: equivalent to infective classification and HF/CRDE overlap
  )
}


#' Identify patients who were admitted already prior to study entry
#'
#' Hospital acquired COVID is recorded explicitly in 2 places for some patients.
#' A large difference between admission date and enrollment date (<21 days) is
#' suggestive in other cases. The data is probably only collected in COVID cases
#' so shoudl be treated with caution.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_nosocomial_status = function(df,v) {
  df %>% dplyr::mutate(
    admission.hospital_acquired = dplyr::case_when(
      admission.hospital_acquired_covid == "yes" ~ "Probable",
      admission.non_lrtd_hospital_acquired_covid == "yes" ~ "Probable",
      # If neother of the HAP fields is given fall back on the admission date versus enrollment date.
      is.na(admission.hospital_acquired_covid) &
        is.na(admission.non_lrtd_hospital_acquired_covid) &
        admission.date+21 < admin.enrollment_date ~ "Possible",
      TRUE ~ "Unlikely"
    ) %>% factor(levels = c("Unlikely","Possible", "Probable")),
    admission.days_before_enrollment = cut_integer(
      as.integer(admin.enrollment_date-admission.date), c(8,15,22,29), lower_limit = 0
    )
  )
  # normData2 %>% xglimpse(delayed_enrol = admission.date + 28 < admin.enrollment_date, hap = admission.hospital_acquired_covid=="yes" | admission.non_lrtd_hospital_acquired_covid == "yes")
}

# Diagnostic ----

#' Determine if an admission is proven SARS-CoV-2 PCR positive
#'
#' SARS-CoV-2 PCR positive only lab confirmed diagnosis.
#'
#' admission.covid_pcr_result:
#'
#' * based on fields: c19_adm_swab and covid_19_diagnosis
#' * Patient reported, clinical diagnoses are assumed PCR negative (although possible in
#' some cases they may not have been done).
#' * Lateral flows done in hospital are
#' counted as PCR negative.
#' * negative admission swabs are counted as negative
#' * NA signifies test not done.
#'
#' admission.is_covid:
#'
#' * Binary confirmed or no-evidence.
#' * PCR results count as confirmed,
#' * Lateral flow results count as confirmed,
#' * anything else is no evidence (includes negatives and test not done)
#'
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_covid_status = function(df,v,...) {
  # This part works in the UoB data set (lrti incidence)
  df %>% dplyr::mutate(
      admission.covid_pcr_result = dplyr::case_when(

        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - laboratory confirmed` ~ "SARS-CoV-2 PCR positive",

        .safe(diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive` & diagnosis.test_type == v$diagnosis.test_type$`PCR Confirmed`) ~ "SARS-CoV-2 PCR positive",

        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - patient reported test` ~ "SARS-CoV-2 PCR negative",
        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - clinical diagnosis (but negative test)` ~ "SARS-CoV-2 PCR negative",
        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - negative test, unlikely COVID-19 disease` ~ "SARS-CoV-2 PCR negative",
        .safe(diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 negative`) ~ "SARS-CoV-2 PCR negative",

        diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`No test performed` ~ NA_character_,

        TRUE ~ NA_character_
      ),

      admission.is_covid = dplyr::case_when(

        admission.covid_pcr_result == "SARS-CoV-2 PCR positive" ~ "Confirmed SARS-CoV-2",
        # admission swabs field includes LFT results
        .safe(diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive`) ~ "Confirmed SARS-CoV-2",
        TRUE ~ "No evidence SARS-CoV-2"


      ) %>% factor(levels = c("Confirmed SARS-CoV-2","No evidence SARS-CoV-2"))

  )

}




#' Create 4 non exclusive diagnostic categories
#'
#' Pneumonia if one of:
#' * Standard of care diagnosis of CAP (radiologically or clinically)
#' * Empyema or abscess
#' * Admission chest X-ray shows pneumonia
#'
#' NP-LRTI if:
#' * Not pneumonia and Standard of care LTRI diagnosis
#'
#' Exacerbation of CRDE:
#' * Standard of care exacerbation COPD
#' * Standard of care exacerbation Non-COPD
#' * (N.B. may be pneumonia or NP-LRTI)
#'
#' Heart failure:
#' * Standard of care congestive heart failure.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_diagnosis_categories = function(df,v) {
  df %>% dplyr::mutate(
    diagnosis.pneumonia = dplyr::case_when(
      diagnosis.SOC_CAP_clinically_confirmed == v$diagnosis.SOC_CAP_clinically_confirmed$yes ~ "yes",
      diagnosis.SOC_CAP_radiologically_confirmed == v$diagnosis.SOC_CAP_radiologically_confirmed$yes ~ "yes",
      diagnosis.SOC_CAP_no_radiology == v$diagnosis.SOC_CAP_no_radiology$yes ~ "yes",
      diagnosis.SOC_Empyema_or_abscess == v$diagnosis.SOC_Empyema_or_abscess$yes ~ "yes",
      (!is.na(admission.cxr_pneumonia) & admission.cxr_pneumonia == v$admission.cxr_pneumonia$yes) ~ "yes",
      TRUE ~ "no"
    ) %>% factor(levels = c("no","yes")),
    diagnosis.LRTI = dplyr::case_when(
      # preventing overlap between pneumonia and LRTD is desirable here
      # TODO: we should have a test that looks for this in the source data
      diagnosis.pneumonia == "yes" ~ "no",
      diagnosis.SOC_LRTI == v$diagnosis.SOC_LRTI$yes ~ "yes",
      TRUE ~ "no"
    ) %>% factor(levels = c("no","yes")),
    diagnosis.exacerbation_of_chronic_respiratory_disease = dplyr::case_when(
      diagnosis.SOC_exacerbation_COPD == v$diagnosis.SOC_exacerbation_COPD$yes ~ "yes",
      diagnosis.SOC_exacerbation_non_COPD == v$diagnosis.SOC_exacerbation_non_COPD$yes ~ "yes",
      TRUE ~ "no"
    ) %>% factor(levels = c("no","yes")),
    diagnosis.heart_failure = dplyr::case_when(
      diagnosis.SOC_congestive_heart_failure == v$diagnosis.SOC_congestive_heart_failure$yes ~ "yes",
      TRUE ~ "no"
    ) %>% factor(levels = c("no","yes"))
  )
}

#' Determine if an admission is due to an infective cause
#'
#' Infective admissions are defined as any of:
#' * pneumonias
#' * NP-LRTI
#' * laboratory confirmed COVID diagnosis
#' * admission swab COVID positive
#'
#' Infective admissions are excluded if:
#' * Standard of care states non-infectious process
#' * SOC non-LRTI (and none of the other categories above)
#'
#' Any unknowns are defined as non-Infective
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_infective_classification = function(df,v) {
  df %>% dplyr::mutate(
    admission.infective_cause = dplyr::case_when(
      diagnosis.pneumonia == "yes" ~ "Infective",
      diagnosis.LRTI == "yes" ~ "Infective",
      admission.is_covid == "Confirmed SARS-CoV-2" ~ "Infective",

      diagnosis.SOC_non_infectious_process == v$diagnosis.SOC_non_infectious_process$yes ~ "Non-infective",
      diagnosis.SOC_non_LRTI == v$diagnosis.SOC_non_LRTI$yes ~ "Non-infective",
      TRUE ~ "Non-infective"
    ) %>% factor(levels = c("Non-infective","Infective"))
  )
}

#' The aLRTD incidence paper classifications
#'
#' The 3 category classifications
#'
#' * aetiological:
#'   * Confirmed SARS-CoV-2 - implies Infective
#'   * No evidence SARS-CoV-2 - implies Infective but not confirmed as SARS-CoV-2
#'   * Non-infective - presumed non infective
#'
#' * clinical presentation:
#'   * Pneumonia - implies Infective
#'   * NP-LRTI - implies Infective
#'   * No evidence LRTI (include CRDE and HF)
#'
#' Some cases do not get a clinical presentation in this. Typically they are people
#' who have an infective cause, but LRTI and pneumonia have been excluded. These
#' could be URTI and or incidental COVID cases.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_aLRTD_categories = function(df,v,...) {
  df %>% dplyr::mutate(

      admission.category = dplyr::case_when(
        admission.is_covid == "Confirmed SARS-CoV-2" ~ "Confirmed SARS-CoV-2",
        admission.infective_cause == "Infective" ~ "No evidence SARS-CoV-2",
        admission.infective_cause == "Non-infective" ~ "Non-infective",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Confirmed SARS-CoV-2","No evidence SARS-CoV-2","Non-infective")),

      admission.presentation_3_class = dplyr::case_when(
          diagnosis.pneumonia == "yes" ~ "Pneumonia",
          diagnosis.LRTI == "yes" ~ "NP-LRTI",
          admission.infective_cause == "Non-infective" ~ "No evidence LRTI",
          TRUE ~ "No evidence LRTI"
        ) %>% factor(levels = c("Pneumonia","NP-LRTI","No evidence LRTI")),
      )
}




#' Did the patient catch COVID in hospital
#'
#' Only relevant to SARS-CoV-2 PCR positive patient.
#' Timing of positive test compared to admission:
#' This relies on knowing dates and hence only works on the identifiable data
#' sets,
#'
#' Logic is:
#'
#' * Community if PCR result predates admission
#' * Probably commuinity if PCR result within 7 days of admission
#' * Probably nosocomial if 7-28 days after admission
#' * Otherwise is it undefined.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_nosocomial_covid_status = function(df, v, ...) {
  df %>% dplyr::mutate(
    diagnosis.infection_context = #.maybe(
      dplyr::case_when(
        is.na(admission.covid_pcr_result) ~ NA_character_,
        admission.covid_pcr_result == "SARS-CoV-2 PCR negative" ~ NA_character_,
        .safe(admission.hospital_acquired_covid == "yes") ~ "Possible nosocomial",
        .safe(admission.non_lrtd_hospital_acquired_covid == "yes") ~ "Possible nosocomial",
        .safe(is.na(diagnosis.first_COVID_positive_swab_date)) ~ "Unknown",
        .safe(diagnosis.first_COVID_positive_swab_date < admission.date) ~ "Community",
        .safe(diagnosis.first_COVID_positive_swab_date < admission.date+7) ~ "Probable community",
        .safe(diagnosis.first_COVID_positive_swab_date < admission.date+28) ~ "Possible nosocomial",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Community","Probable community","Possible nosocomial","Unknown"))
    # )
  )
}


# COVID ----

#' Give a inferred Alpha, Delta or Omicron status based on time alone.
#'
#' This relies on date period during which we are very confidence that the only
#' variants circulating are of a given type. These are quite conservative estimates
#' based on the frequency of sequenced cases in the bristol area (according to the
#' Sanger centre and to cases identified in the hospital testing)
#'
#' [Sanger centre data](https://covid19.sanger.ac.uk/lineages/raw?date=2021-07-24&area=E06000023&lineageView=1&lineages=A%2CB%2CB.1.1.7%2CB.1.617.2%2CB.1.1.529&colours=7%2C3%2C1%2C6%2C2)
#'
#' * Pre-alpha before `r .fdmy(key_dates$min_alpha)`
#' * Alpha between `r .fdmy(key_dates$max_wuhan)` and `r .fdmy(key_dates$min_delta)`
#' * Delta between `r .fdmy(key_dates$max_alpha)` and `r .fdmy(key_dates$min_omicron)`
#' * Omicron from `r .fdmy(key_dates$max_delta)` to present
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_genomic_variant = function(df,v,...) {
  # Bristol specific data is here

  # message("Inferring Delta for cases before ",minOmicron," and inferring Omicron for cases after ",maxDelta)
  df %>% dplyr::mutate(
    genomic.variant_inferred = dplyr::case_when(
      is.na(admission.covid_pcr_result) ~ NA_character_,
      admission.covid_pcr_result == "SARS-CoV-2 PCR negative" ~ NA_character_,
      !is.na(genomic.variant) & genomic.variant != "unknown" ~ as.character(genomic.variant),
      admission.date < key_dates$min_alpha ~ "Pre-alpha",
      admission.date > key_dates$max_wuhan & admission.date < key_dates$min_delta ~ "Alpha",
      admission.date > key_dates$max_alpha & admission.date < key_dates$min_omicron ~ "Delta",
      admission.date > key_dates$max_delta ~ "Omicron",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Pre-alpha","Alpha","Delta","Omicron"))
  )

    # .fdmy = function(date) format(date,"%d %b %Y")
    # dtrackr::comment(.headline = "Infering genomic variant",.messages = c(
    #   "If sequencing not available, we assume:",
    #   "Pre-alpha before {.fdmy(minAlpha)}",
    #   "Alpha between {.fdmy(maxWuhan)} and {.fdmy(minDelta)}",
    #   "Delta between {.fdmy(maxAlpha)} and {.fdmy(minOmicron)}",
    #   "Omicron from {.fdmy(maxDelta)} to present"
    # ), .tag = "variants-inference")

}

#' Derive times from vaccination to symptom onset
#'
#' If symptom duration is not given it is assumed to be zero.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_vaccination_timings = function(df,v,...) {
    # Use symptom onset dates rather than admission dates
    # TODO: time since admission fields could be used for avoncap_export dataset
    df %>%
      dplyr::mutate(
        symptom_onset.date_of_symptoms = admission.date - .na.default(admission.duration_symptoms, 0),
        symptom_onset.time_of_symptoms_since_first_vaccine_dose = admission.time_since_first_vaccine_dose - admission.duration_symptoms,
        symptom_onset.time_of_symptoms_since_second_vaccine_dose = admission.time_since_second_vaccine_dose - admission.duration_symptoms,
        symptom_onset.time_of_symptoms_since_third_vaccine_dose = admission.time_since_third_vaccine_dose - admission.duration_symptoms,
        symptom_onset.time_of_symptoms_since_fourth_vaccine_dose = admission.time_since_fourth_vaccine_dose - admission.duration_symptoms
        # TODO: 5th & 6th dose? change to last dose?
      ) %>%
      dplyr::mutate(
        vaccination.dose_interval = admission.time_since_first_vaccine_dose - admission.time_since_second_vaccine_dose,
        vaccination.booster_interval = admission.time_since_second_vaccine_dose - admission.time_since_third_vaccine_dose,
        vaccination.second_booster_interval = admission.time_since_third_vaccine_dose - admission.time_since_fourth_vaccine_dose
        # TODO: additional dose? change to last dose?
      )
}

#' Derive detailed vaccination status on admission
#'
#' Vaccination is deemed to have had effect if given > 14 days before admission
#' for 1st dose or >7 days before admission for subsequent doses.
#' This does not account for previous infection which is not in the data set.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_completed_vaccination_status = function(df,v,...) {
  df %>% dplyr::mutate(
      vaccination.protection = #.maybe(
        dplyr::case_when(
          # vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
          #   admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) &
          #   admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Vacc + Recovered",
          # TODO: time since 5th and sixth doses?
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_fourth_vaccine_dose >= 7 + .na.default(admission.duration_symptoms,0) ~ "4th dose 7d+",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_third_vaccine_dose >= 7 + .na.default(admission.duration_symptoms,0) ~ "3rd dose 7d+",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_second_vaccine_dose >= 7 + .na.default(admission.duration_symptoms,0) ~ "2nd dose 7d+",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_first_vaccine_dose >= 14 + .na.default(admission.duration_symptoms,0) ~ "1st dose 14d+",
          admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Unvaccinated with prior COVID",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_first_vaccine_dose < 0 + .na.default(admission.duration_symptoms,0) ~ "Before 1st dose",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_first_vaccine_dose < 14 + .na.default(admission.duration_symptoms,0) ~ "1st dose 0-13d",
          TRUE ~ "Unvaccinated"
        ) %>% ordered(c("Unvaccinated","Unvaccinated with prior COVID","Before 1st dose","1st dose 0-13d","1st dose 14d+","2nd dose 7d+","3rd dose 7d+","4th dose 7d+"))
      # )
    )
}

#' A simple vaccination status on admission as an ordered number of doses
#'
#' This does not account for previous infection which is not in the data set.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_effective_vaccination_status = function(df,v,...) {
  df %>% dplyr::mutate(
      vaccination.vaccination = #.maybe(
        dplyr::case_when(
          # vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
          #   symptom_onset.time_of_symptoms_since_second_vaccine_dose >= 7 & admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Vacc + Recovered",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_sixth_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "6 doses",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_fifth_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "5 doses",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_fourth_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "4 doses",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_third_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "3 doses",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "2 doses",
          vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
            admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) ~ "1 dose",
          TRUE ~ "Unvaccinated"
        ) %>% ordered(c("Unvaccinated","1 dose","2 doses","3 doses","4 doses","5 doses","6 doses"))
      #)
    )
}

#' Deprecated - Vaccine combinations are less relevant now
#'
#' There are too many potential combinations with 4th, 5th and sixth dose to make
#' this useful.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_vaccine_combinations = function(df,v,...) {
  # Vaccine brand combinations
  df %>% dplyr::mutate(
    vaccination.brand_combination = paste(
      dplyr::case_when(
        vaccination.first_dose_brand == v$vaccination.first_dose_brand$Pfizer ~ "Pf",
        vaccination.first_dose_brand == v$vaccination.first_dose_brand$AZ ~ "AZ",
        vaccination.first_dose_brand == v$vaccination.first_dose_brand$unknown ~ "??",
        vaccination.first_dose_brand == v$vaccination.first_dose_brand$Moderna ~ "Mo",
        vaccination.first_dose_brand == v$vaccination.first_dose_brand$Janssen ~ "Ja",
        TRUE ~ "xx"
      ),
      dplyr::case_when(
        vaccination.second_dose_brand == v$vaccination.second_dose_brand$Pfizer ~ "Pf",
        vaccination.second_dose_brand == v$vaccination.second_dose_brand$AZ ~ "AZ",
        vaccination.second_dose_brand == v$vaccination.second_dose_brand$unknown ~ "??",
        vaccination.second_dose_brand == v$vaccination.second_dose_brand$Moderna ~ "Mo",
        vaccination.second_dose_brand == v$vaccination.second_dose_brand$Janssen ~ "Ja",
        TRUE ~ "xx"
      ),
      dplyr::case_when(
        vaccination.third_dose_brand == v$vaccination.third_dose_brand$Pfizer ~ "Pf",
        vaccination.third_dose_brand == v$vaccination.third_dose_brand$AZ ~ "AZ",
        vaccination.third_dose_brand == v$vaccination.third_dose_brand$unknown ~ "??",
        vaccination.third_dose_brand == v$vaccination.third_dose_brand$Moderna ~ "Mo",
        vaccination.third_dose_brand == v$vaccination.third_dose_brand$Janssen ~ "Ja",
        TRUE ~ "xx"
      ),
      # dplyr::case_when(
      #   vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$Pfizer ~ "Pf",
      #   vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$AZ ~ "AZ",
      #   vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$unknown ~ "??",
      #   vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$Moderna ~ "Mo",
      #   vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$Janssen ~ "Ja",
      #   TRUE ~ "xx"
      # ),
      sep="-"
    ) %>% forcats::as_factor()
  )
}



# TODO:
# covid shielding groups
# https://webarchive.nationalarchives.gov.uk/ukgwa/20220610000336/https://digital.nhs.uk/coronavirus/shielded-patient-list/methodology/rule-logic?key=

# Pneumococcal ----

#' The pneumococcal incidence diagnostic classifications
#'
#' The 4 category disjoint classification.
#'
#' * pneumo.presentation_class:
#'   * CAP+/RAD+ - radiologically proved pneumonia
#'   * CAP+/RAD- - pneumonia without x-ray confirmation
#'   * NP-LRTI - non-pneumonic lower respiratory tract infection
#'   * No evidence LRTI - believed to be non-infective at admission, this last
#'   group is usually discarded from analysis, however it only really describes
#'   people without a clinical diagnosis of LRTI on admission. There could
#'   still be undiagnosed infection there, and some of these patients have
#'   COVID (possibly without lower respiratory symptoms?).
#'
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_pneumococcal_categories = function(df,v,...) {

  df %>% dplyr::mutate(

    pneumo.presentation_CAP = admission.presentation_3_class == "Pneumonia",
    pneumo.presentation_RAD = diagnosis.SOC_CAP_radiologically_confirmed == "yes",

    pneumo.presentation_class = dplyr::case_when(
      pneumo.presentation_CAP & pneumo.presentation_RAD ~ "CAP+/RAD+",
      pneumo.presentation_CAP & !pneumo.presentation_RAD ~ "CAP+/RAD-",
      admission.presentation_3_class == "NP-LRTI" ~ "NP-LRTI",
      TRUE ~ "No evidence LRTI"
    ) %>% factor(c("No evidence LRTI","NP-LRTI","CAP+/RAD-","CAP+/RAD+"))
  )
}

#' Determine if patient is in a high pneumococcal risk group
#'
#' High pneumococcal risk defined if any of the following:
#' * over 65 years old
#' * other pneumococcal risks
#' * comorbid copd
#' * interstitial lung disease
#' * cystic fibrosis
#' * hypertension
#' * CCF
#' * ischaemic heart disease
#' * chronic kidney disease
#' * chronic liver disease
#' * diabetes
#' * asthmatic with immunodeficiency
#' * on immunosupression
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_pneumococcal_high_risk = function(df,v,...) {
  df %>% dplyr::mutate(
      # admission.frailty_score = cut(admission.rockwood_score, breaks=c(0,5,Inf), labels=c("0-4","5-9"),ordered_result = TRUE),
      admission.pneumococcal_high_risk = ifelse(
        demog.age >= 65 |
          comorbid.other_pneumococcal_risks == v$comorbid.other_pneumococcal_risks$yes |
          comorbid.copd == v$comorbid.copd$yes |
          comorbid.interstitial_lung_dx == v$comorbid.interstitial_lung_dx$yes |
          comorbid.cystic_fibrosis == v$comorbid.cystic_fibrosis$yes |
          comorbid.hypertension == v$comorbid.hypertension$yes |
          comorbid.ccf == v$comorbid.ccf$yes |
          comorbid.ihd == v$comorbid.ihd$yes |
          comorbid.ckd == v$comorbid.ckd$`Moderate or Severe CKD (CKD 4+)` |
          comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease without failure` |
          comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease with failure` |
          comorbid.diabetes == v$comorbid.diabetes$`Type 1 - no complications` |
          comorbid.diabetes == v$comorbid.diabetes$`Type 1 - complications` |
          comorbid.diabetes == v$comorbid.diabetes$`Type 2 - no complications` |
          comorbid.diabetes == v$comorbid.diabetes$`Type 2 - complications` |
          comorbid.immunodeficiency == v$comorbid.immunodeficiency$yes & comorbid.asthma == v$comorbid.asthma$yes |
          admission.on_immunosuppression == v$admission.on_immunosuppression$yes
        ,
        "yes","no") %>% factor(levels=c("no","yes"))
    )
}

#' Determine pneumococcal risk group
#'
#' Original algorithm from B1851202 SAP defines a 3 class risk group:
#'
#' High-risk (immunocompromised)
#'
#' * Asplenia - not supported
#' * Cancer/Malignancy, Hematologic - OK
#' * Cancer/Malignancy, Solid Tumor - OK
#' * Chronic Kidney Disease - OK
#' * Human Immunodeficiency Virus (HIV) – AIDS - OK
#' * Human Immunodeficiency Virus (HIV) – No AIDS - OK
#' * Immunodeficiency - OK
#' * Immunosuppressant Drug Therapy - OK
#' * Organ Transplantation - OK
#' * Multiple Myeloma - not supported
#'
#' At Risk (immunocompetent)
#'
#' * Asthma - OK
#' * Alcoholism - OK
#' * Celiac Disease - not supported
#' * Chronic Liver Disease without Hepatic Failure - OK
#' * Chronic Liver Disease with Hepatic Failure - OK
#' * Chronic Obstructive Pulmonary Disease - OK
#' * Cochlear Implant - not supported
#' * Congestive Heart Failure - OK
#' * Coronary Artery Disease (CAD) - OK
#' * Chronic Neurologic Diseases - OK
#' * Coagulation factor replacement therapy - not supported
#' * CSF Leak - not supported
#' * Diabetes Treated with Medication - OK
#' * Down syndrome - OK
#' * Institutionalized in nursing home or LTC facility (Nursing home or long-term care
#'   facility for those with disability or dependency on subject characteristics/risk
#'   determinants eCRF page) - OK
#' * Occupational risk with exposure to metal fumes - OK
#' * Other Chronic Heart Disease - OK
#' * Other Chronic Lung Disease - OK
#' * Other pneumococcal disease risk factors - OK
#' * Previous Invasive Pneumococcal Disease - not supported
#' * Tobacco smoking (Tobacco/E-Cigarettes) - OK
#'
#' Anything else is low risk
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_pneumococcal_risk_category = function(df,v,...) {

 df %>% dplyr::mutate(
      admission.pneumococcal_risk_classification = dplyr::case_when(

        # HIGH RISK Conditions
        comorbid.cva == v$comorbid.cva$yes ~ "High risk",
        comorbid.immunodeficiency == v$comorbid.immunodeficiency$yes ~ "High risk",
        comorbid.leukaemia == v$comorbid.leukaemia$yes ~ "High risk",
        comorbid.lymphoma == v$comorbid.lymphoma$yes ~ "High risk",
        # difficult double negative here:
        comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no ~ "High risk",
        admission.on_immunosuppression == v$admission.on_immunosuppression$yes ~ "High risk",
        comorbid.transplant_recipient == v$comorbid.transplant_recipient$yes ~ "High risk",
        # Bone marrow transplant ~ "High risk"
        comorbid.ckd == v$comorbid.ckd$`Moderate or Severe CKD (CKD 4+)` ~"High risk",
        comorbid.HIV == v$comorbid.HIV$yes ~ "High risk",
        comorbid.solid_cancer != v$comorbid.solid_cancer$None ~ "High risk",
        # Asplenia ~ "High risk"

        # AT RISK Conditions
        demog.care_home_resident == v$demog.care_home_resident$yes ~ "At risk",
        demog.smoker == v$demog.smoker$Current ~ "At risk",
        demog.age >= 60 ~ "At risk",
        demog.alcohol_abuse == v$demog.alcohol_abuse$yes ~ "At risk",
        # Diabetic on medication
        comorbid.diabetes != v$comorbid.diabetes$None & !is.na(comorbid.diabetes_medications) ~ "At risk",
        # Celiac disease - not recorded ~ "At risk",
        # Down syndrome - not recorded ~ "At risk",
        # Coagulation factor replacement therapy ~ "At risk",
        # Prior invasive pneumococcal dx ~ "At risk"
        comorbid.paraplegia == v$comorbid.paraplegia$yes ~ "At risk",
        # cochlear implant ~ "At risk"
        # CSF leak ~ "At risk"
        # chronic lung diease:
        comorbid.copd == v$comorbid.copd$yes ~ "At risk",
        comorbid.interstitial_lung_dx == v$comorbid.interstitial_lung_dx$yes ~ "At risk",
        comorbid.cystic_fibrosis == v$comorbid.cystic_fibrosis$yes ~ "At risk",
        comorbid.asthma == v$comorbid.asthma$yes ~ "At risk",
        # chronic liver diease:
        comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease without failure` ~ "At risk",
        comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease with failure` ~ "At risk",
        # chronic heart disease:
        comorbid.ccf == v$comorbid.ccf$yes ~ "At risk",
        comorbid.ihd == v$comorbid.ihd$yes ~ "At risk",
        # Occ explose to metal fumes ~ "At risk"
        comorbid.other_pneumococcal_risks == v$comorbid.other_pneumococcal_risks$yes ~ "At risk",
        TRUE ~ "Low risk"
      ) %>% factor(levels=c("Low risk","At risk","High risk"))
    )
}




# Outcomes ----

#' determine WHO outcome score
#'
#' Scores 0-3 are for community cases.
#'
#' We generally can't tell the difference between 7 and 8.
#'
#' * 4: Hospitalised; no oxygen therapy
#' * 5: Hospitalised; oxygen by mask or nasal prongs
#' * 6: Hospitalised; oxygen by NIV or high flow
#' * 7: Intubation and mechanical ventilation, pO2/FiO2 >= 150 or SpO2/FiO2 >= 200
#' * 8: Mechanical ventilation pO2/FIO2 <150 (SpO2/FiO2 <200) or vasopressors
#' * 9: Mechanical ventilation pO2/FiO2 <150 and vasopressors, dialysis, or ECMO
#' * 10: Dead
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_WHO_outcome_score = function(df, v, ...) {

  df %>% dplyr::mutate(
      day_7.WHO_clinical_progression = dplyr::case_when(
        day_7.death == v$day_7.death$yes ~ "score 10",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$ETT & day_7.ionotropes_needed == v$day_7.ionotropes_needed$yes   ~ "score 9",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$ETT ~ "score 7-8",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$`NIV (CPAP/BiPAP)` ~ "score 6",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$HFNO ~ "score 6",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$Oxygen ~ "score 5",
        day_7.max_o2_level == v$day_7.max_o2_level$`24-28%` ~ "score 5",
        day_7.max_o2_level == v$day_7.max_o2_level$`room air` ~ "score 4",
        day_7.max_ventilation_level == v$day_7.max_ventilation_level$None ~ "score 4",
        TRUE ~ NA_character_

      ) %>% ordered(levels = c("score 4","score 5","score 6","score 7-8","score 9","score 10"))
    ) %>% dplyr::mutate(

      # Data is slightly different at the 30 day time point

      outcome.WHO_clinical_progression = dplyr::case_when(
        !is.na(outcome.survival_duration) & outcome.survival_duration <= 30 ~ "score 10",
        outcome.functional_status == v$outcome.functional_status$Deceased ~ "score 10",
        outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$Intubation & outcome.received_ionotropes == v$outcome.received_ionotropes$yes   ~ "score 9",
        outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$Intubation ~ "score 7-8",
        outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$BiPAP ~ "score 6",
        outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$CPAP ~ "score 6",
        outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$`High-Flow Nasal Cannulae` ~ "score 6",
        outcome.respiratory_support_needed == v$outcome.respiratory_support_needed$yes ~ "score 5",
        outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$None ~ "score 4",
        # if the outcome was bad at day 7 then the max outcome score bad at day 30.
        !is.na(day_7.max_o2_level) & day_7.max_o2_level == v$day_7.max_o2_level$`24-28%` ~ "score 5",
        !is.na(day_7.max_o2_level) & day_7.max_o2_level == v$day_7.max_o2_level$`room air` ~ "score 4",
        !is.na(day_7.max_ventilation_level) & day_7.max_ventilation_level == v$day_7.max_ventilation_level$None ~ "score 4",
        TRUE ~ NA_character_
      ) %>% ordered(levels = c("score 4","score 5","score 6","score 7-8","score 9","score 10"))
    )


}

#' Binary outcomes for severe disease
#'
#' * Confirmed death within 30 days (subject to potential censoring)
#' * Confirmed death within 1 year (subject to potential censoring). The
#'   date of censoring depends on when the mortality data was updated. Currently
#'   this is `r .fdmy(key_dates$mortality_updated)`
#' * Confirmed death (any length follow up)
#' * Any ICU admission
#'
#' described in aLRTD paper.
#' These outcomes are
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_severe_disease_outcomes = function(df,v,...) {
  df %>%
      dplyr::mutate(
        # outcome.length_of_stay
        # outcome.icu_duration
        outcome.icu_admission = dplyr::case_when(
          outcome.icu_duration > 0 ~ "confirmed",
          TRUE ~ "not recorded"
        ) %>% factor(levels=c("confirmed","not recorded")),
        outcome.death_within_30_days = dplyr::case_when(
          day_7.death == v$day_7.death$yes ~ "confirmed",
          outcome.survival_duration <= 30 ~ "confirmed",
          TRUE ~ "not recorded"
        ) %>% factor(levels=c("confirmed","not recorded")),
        outcome.death_within_1_year = dplyr::case_when(
          !is.na(outcome.survival_duration) & outcome.survival_duration <= 30 ~ "confirmed",
          !is.na(outcome.one_year_survival_duration) & outcome.one_year_survival_duration < 365 ~ "confirmed",
          outcome.one_year_survival_complete != "Complete" ~ "not recorded",
          TRUE ~ "not recorded"
        ) %>% factor(levels=c("confirmed","not recorded")),
        # TODO: proper survival censoring
        outcome.death_during_follow_up = dplyr::case_when(
          !is.na(outcome.survival_duration) ~ "confirmed",
          outcome.inpatient_death == v$outcome.inpatient_death$yes ~ "confirmed",
          outcome.functional_status == v$outcome.functional_status$Deceased ~ "confirmed",
          TRUE ~ "not recorded"
        ) %>% factor(levels=c("confirmed","not recorded"))
      )
}


#' Survival analysis times
#'
#' Fixes a data issue with length of stay and survival duration being filled in
#' across 2 columns. and missing last observation dates so that we can calculate
#' survival censoring consistently in other data sets.
#'
#' Calculates:
#'
#' * A consistent length of stay - shortest of length of stay and 30 day and 1 yr survival duration
#' * A consistent uncensored time to death - shortest of 30 day and 1 yr survival duration
#' * A consistent time to last observation
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_survival_times_avoncap = function(df,v,...) {
  # any given record should be updated at 30 days post enrollment unless
  # this has not happened yet, in which case the date of the file is used as a
  # latest possible date.
  last_updated = min(c(
    max(df$admin.enrollment_date)+30,
    attributes(df)$date
  ))
  df %>%
    dplyr::mutate(

      # death status is recorded if the patient is in hospital.
      # A NA LOS and NA time to death is current up to last updates

      # If the patient is discharged we lose sight of them until the mortality
      # data has been updated (intermittently)
      # A non NA LOS and NA time to death is censored at the latest of discharge
      # date and mortality update date.

      # length of stay in hospital until death or discharge.
      # NA implies not discharged (or potentially record not updated)
      # N.B. pmax and pmin return NA if all the parallel elements are NA even for na.rm = TRUE.
      survival.length_of_stay = pmin(
        outcome.length_of_stay, outcome.survival_duration, outcome.one_year_survival_duration,na.rm = TRUE),

      # time to death.
      # NA implies not died or not yet recorded as having died.
      survival.uncensored_time_to_death = pmin(
        outcome.survival_duration, outcome.one_year_survival_duration, na.rm = TRUE),

      # last time of update for los / time to death
      # death status of NA means alive up to this date, after this censoring
      # is active.
      survival.last_observed_event = pmax(
        # in hospital up to end of data
        ifelse(is.na(survival.uncensored_time_to_death) & is.na(survival.length_of_stay),
               last_updated - admission.date, NA),
        # discharged or died at this point
        survival.length_of_stay,
        # discharged and not died but mortality checked, alive can be inferred
        # This is negative if admission is after mortality updated
        # in this case it is not an observation.
        (key_dates$mortality_updated - admission.date) %>% ifelse(. < 0, NA, .),
        na.rm = TRUE
      )
    )
}




#' Binary outcomes for hospital burden
#'
#' These outcomes were tested in the Delta vs Omicron severity paper and
#' sensitivity analysis. These are only defined for COVID cases.
#'
#' * O2 requirement within 7 days (various cut-offs)
#' * Any respiratory support in 7 days (various cut-offs)
#' * LOS > X days in first 7 days (various cut-offs)
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_hospital_burden_outcomes = function(df,v,...) {
  # omicron severe disease outcomes
  df %>%
      dplyr::mutate(
        # MAX O2 within first 7 days
        day_7.max_o2_gt_28 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`24-28%`,"28% and under","over 28%") %>% ordered(c("28% and under","over 28%")),
        day_7.max_o2_gt_35 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`30-35%`,"35% and under","over 35%") %>% ordered(c("35% and under","over 35%")),
        day_7.max_o2_gt_50 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`50%`,"50% and under","over 50%") %>% ordered(c("50% and under","over 50%")),
        # WHO outcome score within first 7 days
        day_7.WHO_score_gt_6 = ifelse(day_7.WHO_clinical_progression > "score 6","WHO score 7-10","WHO score 4-6") %>% ordered(c("WHO score 4-6","WHO score 7-10")),
        day_7.WHO_score_gt_5 = ifelse(day_7.WHO_clinical_progression > "score 5","WHO score 6-10","WHO score 4-5") %>% ordered(c("WHO score 4-5","WHO score 6-10")),
        # day_7.any_ICU = ifelse(day_7.icu_length_of_stay > v$day_7.icu_length_of_stay$`0 day`,"ICU admission","No ICU admission") %>% ordered(c("No ICU admission","ICU admission")),
        # LOS within first 7 days
        day_7.los_gt_3 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`3 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 3),"LOS>3 days","LOS<=3 days") %>% ordered(c("LOS<=3 days","LOS>3 days")),
        day_7.los_gt_5 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`5 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 5),"LOS>5 days","LOS<=5 days") %>% ordered(c("LOS<=5 days","LOS>5 days")),
        day_7.los_gt_7 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`7 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 7),"LOS>7 days","LOS<=7 days") %>% ordered(c("LOS<=7 days","LOS>7 days")),
      )
}

# Haematology ----

#' Binary outcomes for haematology data
#'
#' * Elevated troponin : > 18:  18ng/L is simply the 99th percentile value Beckman
#' assay we use as quoted by the IFCC. We elected to not use sex-specific 99th
#' percentile values although they are also quoted here and you could
#' incorporate into your analysis. I am sure you are aware of the 4th Universal
#' definition of MI that requires a rise or fall above the 99th percentile etc.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_haematology_categories= function(df,v,...) {
  # omicron severe disease outcomes
  df %>%
    dplyr::mutate(
      haem.troponin_level = dplyr::case_when(
        is.na(haem.troponin) ~ "Unknown",
        haem.troponin > 18 ~ ">18",
        TRUE ~ "\u226418" # \u2264 is &lte;
      ) %>% factor(c("\u226418",">18","Unknown")),
      haem.crp_level = dplyr::case_when(
        is.na(haem.crp) ~ "Unknown",
        haem.crp > 50 ~ ">50",
        haem.crp >= 10 ~ "10-50",
        TRUE ~ "<10"
      ) %>% factor(c("<10","10-50",">50","Unknown")),
      haem.white_cell_count_level = dplyr::case_when(
        is.na(haem.white_cell_count) ~ "Unknown",
        haem.white_cell_count > 10 ~ ">10",
        TRUE ~ "\u226410"
      ) %>% factor(c("\u226410",">10","Unknown")),
      haem.d_dimer_level = dplyr::case_when(
        is.na(haem.d_dimer) ~ "Unknown",
        haem.d_dimer > 0.5 ~ ">0.5",
        TRUE ~ "\u22640.5"
      ) %>% factor(c("\u22640.5",">0.5","Unknown")),
      haem.pro_bnp_level = dplyr::case_when(
        is.na(haem.pro_bnp) ~ "Unknown",
        haem.pro_bnp > 125 ~ ">125",
        TRUE ~ "\u2264125"
      ) %>% factor(c("\u2264125",">125","Unknown"))

    )
}

# Deprecated - this was for vaccination plus previous infection
#
# It was always felt that the data was not good enough to trust previous
# covid infection field.
# TODO:
# derive_immune_protection_status = function(df,v,...) {
#    df %>% dplyr::mutate(
#     vaccination.immune_exposure =
#       dplyr::case_when(
#         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#           admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) &
#           admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "3+",
#         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#           admission.time_since_third_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "3+",
#         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#           admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) &
#           admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "2",
#         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#           admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "2",
#         admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "1",
#         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#           admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) ~ "1",
#         TRUE ~ "None"
#       ) %>% ordered(c("None","1","2","3+"))
#     )
# }


# Augment dataset with derived data items
#
# @param avoncap_original the data set as output from `avoncap::normalise_data()`
#
# @return a data set with extended derived fields
# @export
# augment_data = function(avoncap_original) {
#
#   tmp2 = avoncap_original
#   v = tmp2 %>% get_value_sets()
#
#   # # pick up older format for admission swab
#   # if("diagnosis.admission_swab_old" %in% colnames(tmp2)) {
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     diagnosis.admission_swab = ifelse(is.na(diagnosis.admission_swab), diagnosis.admission_swab_old, diagnosis.admission_swab)
#   #   )
#   # }
#
#   # tmp2 = suppressMessages(
#   #   tmp2 %>% dtrackr::pause() %>%
#   #   dplyr::group_by(admin.record_number, admission.date) %>%
#   #   dplyr::mutate(admin.duplicate = ifelse(dplyr::row_number()>1, "yes", "no") %>% factor(levels = c("no","yes"))) %>%
#   #   dplyr::ungroup() %>%
#   #   dtrackr::resume()
#   # )
#
#   # # SOC 4 presentation categories
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     diagnosis.pneumonia = ifelse(
#   #       diagnosis.SOC_CAP_clinically_confirmed == v$diagnosis.SOC_CAP_clinically_confirmed$yes |
#   #         diagnosis.SOC_CAP_radiologically_confirmed == v$diagnosis.SOC_CAP_radiologically_confirmed$yes |
#   #         diagnosis.SOC_CAP_no_radiology == v$diagnosis.SOC_CAP_no_radiology$yes |
#   #         diagnosis.SOC_Empyema_or_abscess == v$diagnosis.SOC_Empyema_or_abscess$yes |
#   #         (!is.na(admission.cxr_pneumonia) & admission.cxr_pneumonia == v$admission.cxr_pneumonia$yes),
#   #       "yes","no") %>% factor(levels = c("no","yes")),
#   #     diagnosis.LRTI = ifelse(
#   #       #TODO: clarify whether preventing overlap between pneumonia and LRTD is desirable here or should be explicit exclusion later (and tracked for data quality).
#   #       diagnosis.pneumonia == "no" &
#   #         diagnosis.SOC_LRTI == v$diagnosis.SOC_LRTI$yes,
#   #       "yes","no") %>% factor(levels = c("no","yes")),
#   #     diagnosis.exacerbation_of_chronic_respiratory_disease = ifelse(
#   #       diagnosis.SOC_exacerbation_COPD == v$diagnosis.SOC_exacerbation_COPD$yes |
#   #         diagnosis.SOC_exacerbation_non_COPD == v$diagnosis.SOC_exacerbation_non_COPD$yes,
#   #       "yes","no") %>% factor(levels = c("no","yes")),
#   #     diagnosis.heart_failure = ifelse(
#   #       diagnosis.SOC_congestive_heart_failure == v$diagnosis.SOC_congestive_heart_failure$yes,
#   #       "yes","no") %>% factor(levels = c("no","yes"))
#   #   )
#   # }, error = function(e) message("could not compute SOC diagnoses: This is normal for the NHS AvonCap extract: ", e$message))
#   # tmp2 %>% augment(derive_diagnosis_categories)
#
#   # Infective cause & covid status
#   # tmp2 = tryCatch({
#   #   tmp2 %>% dplyr::mutate(
#   #     admission.infective_cause = dplyr::case_when(
#   #       diagnosis.pneumonia == "yes" ~ "Infective",
#   #       diagnosis.LRTI == "yes" ~ "Infective",
#   #       .safe(diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - laboratory confirmed`) ~ "Infective",
#   #       diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive` ~ "Infective",
#   #       diagnosis.SOC_non_infectious_process == v$diagnosis.SOC_non_infectious_process$yes ~ "Non-infective",
#   #       diagnosis.SOC_non_LRTI == v$diagnosis.SOC_non_LRTI$yes ~ "Non-infective",
#   #       TRUE ~ "Non-infective"
#   #     ) %>% factor(levels = c("Non-infective","Infective"))
#   #   )
#   #
#   # }, error = function(e) {
#   #   message("falling back to diagnosis.standard_of_care_COVID_diagnosis")
#   #   tmp2 %>% dplyr::mutate(
#   #     admission.infective_cause = dplyr::case_when(
#   #       diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive` ~ "Infective",
#   #       .safe(diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - laboratory confirmed`) ~ "Infective",
#   #       diagnosis.standard_of_care_COVID_diagnosis == v$diagnosis.standard_of_care_COVID_diagnosis$Pneumonia ~ "Infective",
#   #       diagnosis.standard_of_care_COVID_diagnosis == v$diagnosis.standard_of_care_COVID_diagnosis$LRTI ~ "Infective",
#   #
#   #       is.na(diagnosis.standard_of_care_COVID_diagnosis) & is.na(diagnosis.admission_swab) ~ NA_character_,
#   #       TRUE ~ "Non-infective"
#   #     ) %>% factor(levels = c("Non-infective","Infective"))
#   #   )
#   # })
#
#
#   # tmp2 = tryCatch({
#   #   # This part works in the UoB data set (lrti incidence)
#   #   tmp2 %>% dplyr::mutate(
#   #     admission.covid_pcr_result = dplyr::case_when(
#   #       diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - laboratory confirmed` ~ "SARS-CoV-2 PCR positive",
#   #       diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - patient reported test` ~ "SARS-CoV-2 PCR negative",
#   #       diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - clinical diagnosis (but negative test)` ~ "SARS-CoV-2 PCR negative",
#   #       diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`COVID-19 - negative test, unlikely COVID-19 disease` ~ "SARS-CoV-2 PCR negative",
#   #       diagnosis.covid_19_diagnosis == v$diagnosis.covid_19_diagnosis$`No test performed` ~ NA_character_,
#   #       TRUE ~ NA_character_
#   #     ) %>% factor(levels = c("SARS-CoV-2 PCR positive","SARS-CoV-2 PCR negative")),
#   #   )
#   # }, error = function(e) {
#   #   message("falling back to admission_swab data point for covid status")
#   #   tmp2 %>% dplyr::mutate(
#   #     admission.covid_pcr_result = dplyr::case_when(
#   #       diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive` ~ "SARS-CoV-2 PCR positive",
#   #       diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 negative` ~ "SARS-CoV-2 PCR negative",
#   #       TRUE ~ NA_character_) %>% factor(levels = c("SARS-CoV-2 PCR positive","SARS-CoV-2 PCR negative")),
#   #   )
#   # })
#
#   # tryCatch({
#   #   # Consent given
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     admin.consent_withheld = dplyr::case_when(
#   #       admin.consented == v$admin.consented$`Declined consent` ~ "yes",
#   #       admin.pp_consented == v$admin.pp_consented$`Declined consent` ~ "yes",
#   #       admin.withdrawal == v$admin.withdrawal$yes ~ "yes",
#   #       TRUE ~ "no") %>% factor(levels = c("no","yes"))
#   #   ) %>%
#   #     dplyr::mutate(
#   #       admission.infective_cause = ifelse(admin.consent_withheld == "yes", NA, as.character(admission.infective_cause)) %>% factor(levels=levels(admission.infective_cause)),
#   #       admission.covid_pcr_result = ifelse(admin.consent_withheld == "yes", NA, as.character(admission.covid_pcr_result)) %>% factor(levels=levels(admission.covid_pcr_result))
#   #     )
#   # }, error = function(e) message("could not compute consent: This is normal for the NHS AvonCap extract: ", e$message))
#
#
#
#
#   # determine covid patients versus controls
#   # tmp2 = tmp2 %>%
#   #   dplyr::mutate(
#   #     cohort = dplyr::if_else(
#   #       # Original logic here depends on items that are not in current data dump
#   #       # e.g. c19_adm_swab
#   #       # (
#   #       #   diagnosis.COVID_positive == v$diagnosis.COVID_positive$yes &
#   #       #   diagnosis.COVID_negative == v$diagnosis.COVID_negative$no &
#   #       #   diagnosis.no_COVID_test == v$no_COVID_test$no
#   #       # )
#   #       # OR c19_adm_status
#   #       # (
#   #       #   (
#   #       #     diagnosis.COVID_laboratory_confirmed == v$diagnosis.COVID_laboratory_confirmed$yes |
#   #       #     diagnosis.COVID_patient_reported_test == v$diagnosis.COVID_patient_reported_test$yes |
#   #       #     diagnosis.COVID_clinical_diagnosis == v$diagnosis.COVID_clinical_diagnosis$yes
#   #       #   ) &
#   #       #   diagnosis.not_COVID_negative_test !=  v$diagnosis.not_COVID_negative_test$yes &
#   #       #   diagnosis.not_tested_for_COVID != v$diagnosis.not_tested_for_COVID$yes
#   #       # )
#   #       # I don't think this is an adequate alternative:
#   #       # is.na(diagnosis.clinical_or_radiological_LRTI_or_pneumonia),
#   #       !is.na(diagnosis.meets_case_control_criteria) &
#   #         diagnosis.meets_case_control_criteria == v$diagnosis.meets_case_control_criteria$yes &
#   #         diagnosis.admission_swab == v$diagnosis.admission_swab$`COVID-19 positive`,
#   #       "case","control") %>% factor(levels = c("case","control"))
#   #   )
#
#   # determine if infection was possibly nosocomial
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     diagnosis.infection_context = #.maybe(
#   #       dplyr::case_when(
#   #         cohort == "control" ~ NA_character_,
#   #         is.na(diagnosis.first_COVID_positive_swab_date) ~ "Unknown",
#   #         diagnosis.first_COVID_positive_swab_date < admission.date ~ "Community",
#   #         diagnosis.first_COVID_positive_swab_date < admission.date+7 ~ "Probable community",
#   #         diagnosis.first_COVID_positive_swab_date < admission.date+28 ~ "Possible nosocomial",
#   #         TRUE ~ NA_character_
#   #       ) %>% factor(levels = c("Community","Probable community","Possible nosocomial","Unknown"))
#   #     # )
#   #   )
#   # }, error = function(e) message("could not compute infection context: ", e$message))
#
#   # infer variant status
#   # tryCatch({
#   #
#   #   # Bristol specific data is here
#   #   # https://covid19.sanger.ac.uk/lineages/raw?date=2021-07-24&area=E06000023&lineageView=1&lineages=A%2CB%2CB.1.1.7%2CB.1.617.2%2CB.1.1.529&colours=7%2C3%2C1%2C6%2C2
#   #   # https://covid19.sanger.ac.uk/ee0c813f-1706-4fee-a69f-0d642aa4c5a7
#   #
#   #   minAlpha = as.Date("2020-12-05")
#   #
#   #   maxWuhan = as.Date("2021-02-13")
#   #
#   #   minDelta = as.Date("2021-05-15")
#   #
#   #   # maxAlpha = as.Date("2021-06-26") # officially according to sanger but there were very low levels of Alpha
#   #   maxAlpha = as.Date("2021-06-01") # unofficially this cutoff was used in the Delta Omicron paper.
#   #
#   #   # minOmicron = as.Date("2021-11-27") # according to sanger
#   #   minOmicron = as.Date("2021-11-07") # according to in hospital data
#   #   # i.e. tmp2 %>% dplyr::filter(genomic.variant == "Omicron") %>% dplyr::summarise(min = min(admission.date)) %>% dplyr::pull(min)
#   #
#   #   # maxDelta = as.Date("2022-01-15") # according to sanger
#   #   maxDelta = as.Date("2022-02-07") # according to in hospital results
#   #   # i.e. tmp2 %>% dplyr::filter(genomic.variant == "Delta") %>% dplyr::summarise(max = max(admission.date)) %>% dplyr::pull(max)
#   #
#   #   .fdmy = function(date) format(date,"%d %b %Y")
#   #
#   #   # message("Inferring Delta for cases before ",minOmicron," and inferring Omicron for cases after ",maxDelta)
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     genomic.variant_inferred = dplyr::case_when(
#   #       #cohort == "control" ~ NA_character_,
#   #       #!is.na(genomic.variant) ~ as.character(genomic.variant),
#   #       !is.na(genomic.variant) & genomic.variant != "unknown" ~ as.character(genomic.variant),
#   #       admission.date < minAlpha ~ "Pre-alpha",
#   #       admission.date > maxWuhan & admission.date < minDelta ~ "Alpha",
#   #       admission.date > maxAlpha & admission.date < minOmicron ~ "Delta",
#   #       admission.date > maxDelta ~ "Omicron",
#   #       TRUE ~ NA_character_
#   #     ) %>% factor(levels = c("Pre-alpha","Alpha","Delta","Omicron"))
#   #   ) %>% dtrackr::comment(.headline = "Infering genomic variant",.messages = c(
#   #     "If sequencing not available, we assume:",
#   #     "Pre-alpha before {.fdmy(minAlpha)}",
#   #     "Alpha between {.fdmy(maxWuhan)} and {.fdmy(minDelta)}",
#   #     "Delta between {.fdmy(maxAlpha)} and {.fdmy(minOmicron)}",
#   #     "Omicron from {.fdmy(maxDelta)} to present"
#   #   ), .tag = "variants-inference")
#   # }, error = function(e) message("could not infer variant status from dates: ", e$message))
#
#   # Compute symptom onset dates
#   #TODO NHS Redcap only
#   # tryCatch({
#   #   # Use symptom onset dates rather than admission dates
#   #   tmp2 = tmp2 %>%
#   #     dplyr::mutate(
#   #       symptom_onset.date_of_symptoms = admission.date - admission.duration_symptoms,
#   #       symptom_onset.time_of_symptoms_since_first_vaccine_dose = admission.time_since_first_vaccine_dose - admission.duration_symptoms,
#   #       symptom_onset.time_of_symptoms_since_second_vaccine_dose = admission.time_since_second_vaccine_dose - admission.duration_symptoms,
#   #       symptom_onset.time_of_symptoms_since_third_vaccine_dose = admission.time_since_third_vaccine_dose - admission.duration_symptoms,
#   #       symptom_onset.time_of_symptoms_since_fourth_vaccine_dose = admission.time_since_fourth_vaccine_dose - admission.duration_symptoms
#   #     )
#   # }, error = function(e) message("could not compute symptom onset dates: ", e$message))
#   #
#   # # calculate various time intervals
#   # tryCatch({
#   #   #TODO NHS Redcap only
#   #   tmp2 = tmp2 %>%
#   #     dplyr::mutate(
#   #       vaccination.dose_interval = admission.time_since_first_vaccine_dose - admission.time_since_second_vaccine_dose,
#   #       vaccination.booster_interval = admission.time_since_second_vaccine_dose - admission.time_since_third_vaccine_dose,
#   #       vaccination.second_booster_interval = admission.time_since_third_vaccine_dose - admission.time_since_fourth_vaccine_dose
#   #     )
#   # }, error = function(e) message("could not compute admission intervals: ", e$message))
#
#   # # TODO:
#   # tryCatch({
#   #   # # establish vaccine protection / immune status on admission
#   #   # # TODO: do we have any clear defined categories for these?
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     vaccination.protection = #.maybe(
#   #       dplyr::case_when(
#   #         # vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #         #   admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) &
#   #         #   admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Vacc + Recovered",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_fourth_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "4th dose 7d+",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_third_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "3rd dose 7d+",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "2nd dose 7d+",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) ~ "1st dose 14d+",
#   #         admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Unvaccinated with prior COVID",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_first_vaccine_dose < 0+.na.default(admission.duration_symptoms,0) ~ "Before 1st dose",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_first_vaccine_dose < 14+.na.default(admission.duration_symptoms,0) ~ "1st dose 0-13d",
#   #         TRUE ~ "Unvaccinated"
#   #       ) %>% ordered(c("Unvaccinated","Unvaccinated with prior COVID","Before 1st dose","1st dose 0-13d","1st dose 14d+","2nd dose 7d+","3rd dose 7d+","4th dose 7d+"))
#   #     # )
#   #   )
#   # }, error = function(e) message("could not compute vaccination protection status: ", e$message))
#   #
#   # tmp2 = tryCatch({
#   #   tmp2 %>% dplyr::mutate(
#   #     vaccination.vaccination = #.maybe(
#   #       dplyr::case_when(
#   #         # vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #         #   symptom_onset.time_of_symptoms_since_second_vaccine_dose >= 7 & admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "Vacc + Recovered",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_fourth_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "4 doses",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_third_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "3 doses",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "2 doses",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) ~ "1 dose",
#   #         TRUE ~ "Unvaccinated"
#   #       ) %>% ordered(c("Unvaccinated","1 dose","2 doses","3 doses","4 doses"))
#   #     #)
#   #   )
#   # }, error = function(e) {
#   #   message("could not compute simple covid vaccination status using dates, falling back to brand combinations")
#   #   tmp2 %>% dplyr::mutate(
#   #     vaccination.vaccination = #.maybe(
#   #       dplyr::case_when(
#   #         !is.na(vaccination.fourth_dose_brand) ~ "4 doses",
#   #         !is.na(vaccination.third_dose_brand) ~ "3 doses",
#   #         !is.na(vaccination.second_dose_brand) ~ "2 doses",
#   #         !is.na(vaccination.first_dose_brand) ~ "1 dose",
#   #         TRUE ~ "Unvaccinated"
#   #       ) %>% ordered(c("Unvaccinated","1 dose","2 doses","3 doses","4 doses"))
#   #   )
#   # })
#
#   # tryCatch({
#   #   #TODO NHS Redcap only
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     vaccination.immune_exposure = #.maybe(
#   #       dplyr::case_when(
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) &
#   #           admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "3+",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_third_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "3+",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) &
#   #           admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "2",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_second_vaccine_dose >= 7+.na.default(admission.duration_symptoms,0) ~ "2",
#   #         admission.previous_covid_infection == v$admission.previous_covid_infection$yes ~ "1",
#   #         vaccination.covid_vaccination == v$vaccination.covid_vaccination$Received &
#   #           admission.time_since_first_vaccine_dose >= 14+.na.default(admission.duration_symptoms,0) ~ "1",
#   #         TRUE ~ "None"
#   #       ) %>% ordered(c("None","1","2","3+"))
#   #     #)
#   #   )
#   # }, error = function(e) message("could not compute immune exposure status: ", e$message))
#
#   # Vaccine brand combinations
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     vaccination.brand_combination = paste(
#   #       dplyr::case_when(
#   #         vaccination.first_dose_brand == v$vaccination.first_dose_brand$Pfizer ~ "Pf",
#   #         vaccination.first_dose_brand == v$vaccination.first_dose_brand$AZ ~ "AZ",
#   #         vaccination.first_dose_brand == v$vaccination.first_dose_brand$unknown ~ "??",
#   #         vaccination.first_dose_brand == v$vaccination.first_dose_brand$Moderna ~ "Mo",
#   #         vaccination.first_dose_brand == v$vaccination.first_dose_brand$Janssen ~ "Ja",
#   #         TRUE ~ "xx"
#   #       ),
#   #       dplyr::case_when(
#   #         vaccination.second_dose_brand == v$vaccination.second_dose_brand$Pfizer ~ "Pf",
#   #         vaccination.second_dose_brand == v$vaccination.second_dose_brand$AZ ~ "AZ",
#   #         vaccination.second_dose_brand == v$vaccination.second_dose_brand$unknown ~ "??",
#   #         vaccination.second_dose_brand == v$vaccination.second_dose_brand$Moderna ~ "Mo",
#   #         vaccination.second_dose_brand == v$vaccination.second_dose_brand$Janssen ~ "Ja",
#   #         TRUE ~ "xx"
#   #       ),
#   #       dplyr::case_when(
#   #         vaccination.third_dose_brand == v$vaccination.third_dose_brand$Pfizer ~ "Pf",
#   #         vaccination.third_dose_brand == v$vaccination.third_dose_brand$AZ ~ "AZ",
#   #         vaccination.third_dose_brand == v$vaccination.third_dose_brand$unknown ~ "??",
#   #         vaccination.third_dose_brand == v$vaccination.third_dose_brand$Moderna ~ "Mo",
#   #         vaccination.third_dose_brand == v$vaccination.third_dose_brand$Janssen ~ "Ja",
#   #         TRUE ~ "xx"
#   #       ),
#   #       dplyr::case_when(
#   #         vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$Pfizer ~ "Pf",
#   #         vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$AZ ~ "AZ",
#   #         vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$unknown ~ "??",
#   #         vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$Moderna ~ "Mo",
#   #         vaccination.fourth_dose_brand == v$vaccination.third_dose_brand$Janssen ~ "Ja",
#   #         TRUE ~ "xx"
#   #       ),
#   #       sep="-"
#   #     ) %>% forcats::as_factor()
#   #   )
#   # }, error = function(e) message("could not compute vaccination dosing regimen status: ", e$message))
#
#   # rationalise comorbidities
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     comorbid.diabetes_type = dplyr::case_when(
#   #       comorbid.diabetes == v$comorbid.diabetes$None ~ "None",
#   #       comorbid.diabetes == v$comorbid.diabetes$`Type 1 - no complications` ~ "Type 1",
#   #       comorbid.diabetes == v$comorbid.diabetes$`Type 1 - complications` ~ "Type 1",
#   #       comorbid.diabetes == v$comorbid.diabetes$`Type 2 - no complications` ~ "Type 2",
#   #       comorbid.diabetes == v$comorbid.diabetes$`Type 2 - complications` ~ "Type 2",
#   #       TRUE ~ NA_character_
#   #     ) %>% factor(levels = c("None","Type 1","Type 2")),
#   #     comorbid.solid_cancer_present = dplyr::case_when(
#   #       comorbid.solid_cancer == v$comorbid.solid_cancer$None ~ "no",
#   #       comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - no mets` ~ "yes",
#   #       comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - Metastatic Disease` ~ "yes",
#   #       TRUE ~ NA_character_
#   #     ) %>% factor(levels = c("no","yes")),
#   #     comorbid.haemotological_cancer_present = dplyr::case_when(
#   #       comorbid.leukaemia == v$comorbid.leukaemia$yes ~ "yes",
#   #       comorbid.lymphoma == v$comorbid.lymphoma$yes ~ "yes",
#   #       comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no ~ "yes",
#   #       TRUE ~ "no"
#   #     ) %>% factor(levels = c("no","yes")),
#   #     comorbid.any_cancer_present = dplyr::case_when(
#   #       comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - no mets` ~ "yes",
#   #       comorbid.solid_cancer == v$comorbid.solid_cancer$`Solid Organ Cancer - Metastatic Disease` ~ "yes",
#   #       comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no ~ "yes",
#   #       comorbid.leukaemia == v$comorbid.leukaemia$yes ~ "yes",
#   #       comorbid.lymphoma == v$comorbid.lymphoma$yes ~ "yes",
#   #       TRUE ~ "no"
#   #     ) %>% factor(levels = c("no","yes"))
#   #   )
#   # }, error = function(e) message("could not rationalise comorbidities: ", e$message))
#
#   # # determine high pneumococcal risk group
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     admission.frailty_score = cut(admission.rockwood_score, breaks=c(0,5,Inf), labels=c("0-4","5-9"),ordered_result = TRUE),
#   #     admission.pneumococcal_risk_group = ifelse(
#   #       demog.age >= 65 |
#   #         comorbid.other_pneumococcal_risks == v$comorbid.other_pneumococcal_risks$yes |
#   #         comorbid.copd == v$comorbid.copd$yes |
#   #         comorbid.interstitial_lung_dx == v$comorbid.interstitial_lung_dx$yes |
#   #         comorbid.cystic_fibrosis == v$comorbid.cystic_fibrosis$yes |
#   #         comorbid.hypertension == v$comorbid.hypertension$yes |
#   #         comorbid.ccf == v$comorbid.ccf$yes |
#   #         comorbid.ihd == v$comorbid.ihd$yes |
#   #         comorbid.ckd == v$comorbid.ckd$`Moderate or Severe CKD (CKD 4+)` |
#   #         comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease without failure` |
#   #         comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease with failure` |
#   #         comorbid.diabetes == v$comorbid.diabetes$`Type 1 - no complications` |
#   #         comorbid.diabetes == v$comorbid.diabetes$`Type 1 - complications` |
#   #         comorbid.diabetes == v$comorbid.diabetes$`Type 2 - no complications` |
#   #         comorbid.diabetes == v$comorbid.diabetes$`Type 2 - complications` |
#   #         comorbid.immunodeficiency == v$comorbid.immunodeficiency$yes & comorbid.asthma == v$comorbid.asthma$yes |
#   #         admission.on_immunosuppression == v$admission.on_immunosuppression$yes
#   #       ,
#   #       "yes","no") %>% factor(levels=c("no","yes"))
#   #   )
#   # }, error=function(e) message("Could not calculate pneumococcal risk group", e$message))
#
#   # determine pneumococcal risk group from B1851202 SAP
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     admission.pneumococcal_risk_classification = dplyr::case_when(
#   #
#   #       # HIGH RISK Conditions
#   #       comorbid.cva == v$comorbid.cva$yes ~ "High risk",
#   #       comorbid.immunodeficiency == v$comorbid.immunodeficiency$yes ~ "High risk",
#   #       comorbid.leukaemia == v$comorbid.leukaemia$yes ~ "High risk",
#   #       comorbid.lymphoma == v$comorbid.lymphoma$yes ~ "High risk",
#   #       # difficult double negative here:
#   #       comorbid.no_haemotological_cancer == v$comorbid.no_haemotological_cancer$no ~ "High risk",
#   #       admission.on_immunosuppression == v$admission.on_immunosuppression$yes ~ "High risk",
#   #       comorbid.transplant_recipient == v$comorbid.transplant_recipient$yes ~ "High risk",
#   #       # Bone marrow transplant ~ "High risk"
#   #       comorbid.ckd == v$comorbid.ckd$`Moderate or Severe CKD (CKD 4+)` ~"High risk",
#   #       comorbid.HIV == v$comorbid.HIV$yes ~ "High risk",
#   #       comorbid.solid_cancer != v$comorbid.solid_cancer$None ~ "High risk",
#   #       # Asplenia ~ "High risk"
#   #
#   #       # AT RISK Conditions
#   #       demog.care_home_resident == v$demog.care_home_resident$yes ~ "At risk",
#   #       demog.smoker == v$demog.smoker$Current ~ "At risk",
#   #       demog.age >= 60 ~ "At risk",
#   #       demog.alcohol_abuse == v$demog.alcohol_abuse$yes ~ "At risk",
#   #       # Diabetic on medication
#   #       comorbid.diabetes != v$comorbid.diabetes$None & !is.na(comorbid.diabetes_medications) ~ "At risk",
#   #       # Celiac disease - not recorded ~ "At risk",
#   #       # Down syndrome - not recorded ~ "At risk",
#   #       # Coagulation factor replacement therapy ~ "At risk",
#   #       # Prior invasive pneumococcal dx ~ "At risk"
#   #       comorbid.paraplegia == v$comorbid.paraplegia$yes ~ "At risk",
#   #       # cochlear implant ~ "At risk"
#   #       # CSF leak ~ "At risk"
#   #       # chronic lung diease:
#   #       comorbid.copd == v$comorbid.copd$yes ~ "At risk",
#   #       comorbid.interstitial_lung_dx == v$comorbid.interstitial_lung_dx$yes ~ "At risk",
#   #       comorbid.cystic_fibrosis == v$comorbid.cystic_fibrosis$yes ~ "At risk",
#   #       comorbid.asthma == v$comorbid.asthma$yes ~ "At risk",
#   #       # chronic liver diease:
#   #       comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease without failure` ~ "At risk",
#   #       comorbid.liver_disease == v$comorbid.liver_disease$`Liver disease with failure` ~ "At risk",
#   #       # chronic heart disease:
#   #       comorbid.ccf == v$comorbid.ccf$yes ~ "At risk",
#   #       comorbid.ihd == v$comorbid.ihd$yes ~ "At risk",
#   #       # Occ explose to metal fumes ~ "At risk"
#   #       comorbid.other_pneumococcal_risks == v$comorbid.other_pneumococcal_risks$yes ~ "At risk",
#   #       TRUE ~ "Low risk"
#   #       ) %>% factor(levels=c("Low risk","At risk","High risk"))
#   #   )
#   # }, error=function(e) message("Could not calculate pneumococcal risk group", e$message))
#
#   # # determine WHO outcome
#   # # 4: Hospitalised; no oxygen therapy*
#   # # 5: Hospitalised; oxygen by mask or nasal prongs
#   # # 6: Hospitalised; oxygen by NIV or high flow
#   # # 7: Intubation and mechanical ventilation, pO2/FiO2 ≥150 or SpO2/FiO2 ≥200
#   # # 8: Mechanical ventilation pO2/FIO2 <150 (SpO2/FiO2 <200) or vasopressors
#   # # 9: Mechanical ventilation pO2/FiO2 <150 and vasopressors, dialysis, or ECMO
#   # # 10: Dead
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     day_7.WHO_clinical_progression = dplyr::case_when(
#   #       cohort == "control" ~ "score 0",
#   #       day_7.death == v$day_7.death$yes ~ "score 10",
#   #       day_7.max_ventilation_level == v$day_7.max_ventilation_level$ETT & day_7.ionotropes_needed == v$day_7.ionotropes_needed$yes   ~ "score 9",
#   #       day_7.max_ventilation_level == v$day_7.max_ventilation_level$ETT ~ "score 7-8",
#   #       day_7.max_ventilation_level == v$day_7.max_ventilation_level$`NIV (CPAP/BiPAP)` ~ "score 6",
#   #       day_7.max_ventilation_level == v$day_7.max_ventilation_level$HFNO ~ "score 6",
#   #       day_7.max_ventilation_level == v$day_7.max_ventilation_level$Oxygen ~ "score 5",
#   #       day_7.max_o2_level == v$day_7.max_o2_level$`24-28%` ~ "score 5",
#   #       day_7.max_o2_level == v$day_7.max_o2_level$`room air` ~ "score 4",
#   #       day_7.max_ventilation_level == v$day_7.max_ventilation_level$None ~ "score 4",
#   #       TRUE ~ NA_character_
#   #
#   #     ) %>% ordered(levels = c("score 0","score 4","score 5","score 6","score 7-8","score 9","score 10"))
#   #   )
#   # }, error=function(e) message("Could not calculate WHO outcome group at day 7", e$message))
#   #
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     outcome.WHO_clinical_progression = dplyr::case_when(
#   #       cohort == "control" ~ "score 0",
#   #       !is.na(outcome.survival_duration) & outcome.survival_duration <= 30 ~ "score 10",
#   #       outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$Intubation & outcome.recieved_ionotropes == v$outcome.recieved_ionotropes$yes   ~ "score 9",
#   #       outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$Intubation ~ "score 7-8",
#   #       outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$BiPAP ~ "score 6",
#   #       outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$CPAP ~ "score 6",
#   #       outcome.highest_level_ventilatory_support == v$outcome.highest_level_ventilatory_support$`High-Flow Nasal Cannulae` ~ "score 6",
#   #       outcome.respiratory_support_needed == v$outcome.respiratory_support_needed$yes ~ "score 5",
#   #       day_7.max_o2_level == v$day_7.max_o2_level$`24-28%` ~ "score 5",
#   #       day_7.max_o2_level == v$day_7.max_o2_level$`room air` ~ "score 4",
#   #       day_7.max_ventilation_level == v$day_7.max_ventilation_level$None ~ "score 4",
#   #       TRUE ~ NA_character_
#   #     ) %>% ordered(levels = c("score 0","score 4","score 5","score 6","score 7-8","score 9","score 10"))
#   #   )
#   # }, error=function(e) message("Could not calculate WHO outcome group at day 30", e$message))
#
#   # determine salami slice categories
#   # tryCatch({
#   #   tmp2 = tmp2 %>% dplyr::mutate(
#   #     admission.is_covid = dplyr::case_when(
#   #       admission.covid_pcr_result == "SARS-CoV-2 PCR positive" ~ "Confirmed SARS-CoV-2",
#   #       TRUE ~ "No evidence SARS-CoV-2"
#   #     ) %>% factor(levels = c("Confirmed SARS-CoV-2","No evidence SARS-CoV-2")),
#   #     admission.category = dplyr::case_when(
#   #       admission.is_covid == "Confirmed SARS-CoV-2" ~ "Confirmed SARS-CoV-2",
#   #       admission.is_covid == "No evidence SARS-CoV-2" & admission.infective_cause == "Infective" ~ "No evidence SARS-CoV-2",
#   #       admission.infective_cause == "Non-infective" ~ "Non-infective",
#   #       TRUE ~ NA_character_
#   #     ) %>% factor(levels = c("Confirmed SARS-CoV-2","No evidence SARS-CoV-2","Non-infective"))
#   #   )
#   # }, error=function(e) message("Could not calculate main salami slice categories", e$message))
#   #
#   # tmp2 = tryCatch({
#   #   tmp2  %>%
#   #     dplyr::mutate(
#   #       admission.presentation_3_class = dplyr::case_when(
#   #         diagnosis.pneumonia == "yes" ~ "Pneumonia",
#   #         diagnosis.LRTI == "yes" ~ "NP-LRTI",
#   #         admission.infective_cause == "Non-infective" ~ "Non-infective",
#   #         TRUE ~ NA_character_
#   #       ) %>% factor(levels = c("Pneumonia","NP-LRTI","Non-infective")),
#   #     )
#   # }, error=function(e) {
#   #   message("falling back to diagnosis.standard_of_care_COVID_diagnosis")
#   #   tmp2 %>%
#   #     dplyr::mutate(
#   #       admission.presentation_3_class = dplyr::case_when(
#   #         diagnosis.standard_of_care_COVID_diagnosis == v$diagnosis.standard_of_care_COVID_diagnosis$Pneumonia ~ "Pneumonia",
#   #         diagnosis.standard_of_care_COVID_diagnosis == v$diagnosis.standard_of_care_COVID_diagnosis$LRTI ~ "NP-LRTI",
#   #         is.na(diagnosis.standard_of_care_COVID_diagnosis) ~ NA_character_,
#   #         TRUE ~ "Non-infective"
#   #       ) %>% factor(levels = c("Pneumonia","NP-LRTI","Non-infective"))
#   #     )
#   # })
#
#   # demographics categorisation (defaults may be overridden in a specific analysis)
#   # tmp2 = tmp2 %>%
#   #   dplyr::mutate(
#   #     demog.age_category = cut(demog.age,breaks = c(0,35,50,65,75,85,Inf), labels = c("18-34","35-49","50-64","65-74","75-84","85+"), include.lowest = FALSE, ordered_result = TRUE),
#   #     demog.age_eligible = cut(demog.age,breaks = c(0,65,Inf), labels = c("18-64","65+"),ordered_result = TRUE)
#   #   )
#   #
#   #
#   # tmp2 = tmp2 %>%
#   #   dplyr::mutate(
#   #     admission.cci_category = cut(admission.charlson_comorbidity_index, breaks = c(-Inf,0,2,4,Inf), labels=c("none (0)","mild (1-2)","moderate (3-4)","severe (5+)"), include.lowest = FALSE, ordered_result = TRUE)
#   #   )
#
#
#   # # determine salami slice outcomes
#   # tryCatch({
#   #   tmp2 = tmp2 %>%
#   #     dplyr::mutate(
#   #       # outcome.length_of_stay
#   #       # outcome.icu_duration
#   #       outcome.icu_admission = dplyr::case_when(
#   #         outcome.icu_duration > 0 ~ "confirmed",
#   #         TRUE ~ "not recorded"
#   #       ) %>% factor(levels=c("confirmed","not recorded")),
#   #       outcome.death_during_follow_up = dplyr::case_when(
#   #         outcome.inpatient_death == v$outcome.inpatient_death$yes ~ "confirmed",
#   #         TRUE ~ "not recorded"
#   #       ) %>% factor(levels=c("confirmed","not recorded"))
#   #     )
#   # }, error=function(e) message("Could not calculate aLTRD (salami slice) outcomes: ", e$message))
#   #
#   # # omicron severe disease outcomes
#   # tryCatch({
#   #   tmp2 = tmp2 %>%
#   #     dplyr::mutate(
#   #       # MAX O2 within first 7 days
#   #       day_7.max_o2_gt_28 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`24-28%`,"28% and under","over 28%") %>% ordered(c("28% and under","over 28%")),
#   #       day_7.max_o2_gt_35 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`30-35%`,"35% and under","over 35%") %>% ordered(c("35% and under","over 35%")),
#   #       day_7.max_o2_gt_50 = ifelse(day_7.max_o2_level <= v$day_7.max_o2_level$`50%`,"50% and under","over 50%") %>% ordered(c("50% and under","over 50%")),
#   #       # WHO outcome score within first 7 days
#   #       day_7.WHO_score_gt_6 = ifelse(day_7.WHO_clinical_progression > "score 6","WHO score 7-10","WHO score 4-6") %>% ordered(c("WHO score 4-6","WHO score 7-10")),
#   #       day_7.WHO_score_gt_5 = ifelse(day_7.WHO_clinical_progression > "score 5","WHO score 6-10","WHO score 4-5") %>% ordered(c("WHO score 4-5","WHO score 6-10")),
#   #       # day_7.any_ICU = ifelse(day_7.icu_length_of_stay > v$day_7.icu_length_of_stay$`0 day`,"ICU admission","No ICU admission") %>% ordered(c("No ICU admission","ICU admission")),
#   #       # LOS within first 7 days
#   #       day_7.los_gt_3 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`3 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 3),"LOS>3 days","LOS<=3 days") %>% ordered(c("LOS<=3 days","LOS>3 days")),
#   #       day_7.los_gt_5 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`5 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 5),"LOS>5 days","LOS<=5 days") %>% ordered(c("LOS<=5 days","LOS>5 days")),
#   #       day_7.los_gt_7 = ifelse(day_7.length_of_stay > v$day_7.length_of_stay$`7 days` & (is.na(outcome.survival_duration) | outcome.survival_duration > 7),"LOS>7 days","LOS<=7 days") %>% ordered(c("LOS<=7 days","LOS>7 days"))
#   #     )
#   # }, error=function(e) stop("Could not calculate omicron severity outcomes", e$message))
#
#   return(tmp2)
# }
#


