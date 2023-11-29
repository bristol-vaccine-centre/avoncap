# augment.nhs_extract.pneumococcal


#' Make pneumo data compatible with AvonCAP
#'
#' Needed for:
#' * `derive_simpler_comorbidities`
#' * `derive_pneumococcal_high_risk`
#' * `derive_pneumococcal_risk_category`
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_pneumo_polyfill = function(df, ...) {
  df %>% dplyr::mutate(
    comorbid.chronic_pleural_dx = NA %>% factor(c("no","yes")),
    comorbid.interstitial_lung_dx = NA %>% factor(c("no","yes")),
    comorbid.cystic_fibrosis = NA %>% factor(c("no","yes")),
    comorbid.other_chronic_resp_dx = NA %>% factor(c("no","yes")),
    comorbid.pulmonary_hypertension = NA %>% factor(c("no","yes")),
    comorbid.congenital_heart_dx = NA %>% factor(c("no","yes")),
    comorbid.other_arrythmia = NA %>% factor(c("no","yes")),
    comorbid.other_other_heart_dx = NA %>% factor(c("no","yes")),
    demog.care_home_resident = NA %>% factor(c("no","yes"))
  )
}

#' Get vaccine coverage group for known serotype
#'
#' For the longitudinal oneumocococcal data, a range of useful serotype groups
#' is defined in the list `avoncap::serotype_data`. The `avoncap::serotype_pcv_map` gives a set of
#' mappings to (multiple) group headings that gives the overall serotype distribution by
#' vaccine.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_phe_pcv_group = function(df, v, ...) {

  # col_name = rlang::ensym(col_name)

  # all detected and stated in mapping
  detected = unique(c(avoncap::serotype_data$map$serotype, levels(df$pneumo.phe_serotype)))

  # groupings for all serotypes including default for non defined ones
  serotype_groups = avoncap::serotype_data$map %>%
    dplyr::transmute(
      pneumo.phe_serotype=serotype,
      pneumo.pcv_group = dplyr::case_when(
        PCV7 ~ "PCV7",
        PCV13 ~ "PCV13-7",
        PCV15 ~ "PCV15-13",
        PCV20 ~  "PCV20-15",
        TRUE ~ "Non-PCV serotype"
      )
    ) %>%
    tidyr::complete(pneumo.phe_serotype = detected, fill=list(pneumo.pcv_group = "Non-PCV serotype")) %>%
    dplyr::mutate(
      pneumo.pcv_group = factor(pneumo.pcv_group,c("PCV7","PCV13-7","PCV15-13","PCV20-15","Non-PCV serotype")),
      pneumo.phe_serotype = factor(pneumo.phe_serotype, detected)
    )

  df %>%
    dplyr::nest_join(
      avoncap::serotype_pcv_map,
      by=c("pneumo.phe_serotype"="serotype"),
      name="pneumo.vaccine_group",
      na_matches = "never"
    ) %>%
    dplyr::mutate(
      pneumo.phe_serotype = factor(as.character(pneumo.phe_serotype), detected),
      pneumo.serotype_status = ifelse(!is.na(pneumo.phe_serotype),
        "Serotype identified",
        "No serotype"
      ) %>% factor(c("Serotype identified", "No serotype")),

    ) %>% dplyr::left_join(
      serotype_groups, by="pneumo.phe_serotype"
    )


    # dplyr::mutate(group = forcats::fct_drop(forcats::fct_na_value_to_level( group, level=not_matched))) %>%
    # dplyr::rename(!!col_name := group)
}

#' Pneumococcal invasive status and binary test category
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_invasive_status = function(df, ...) {

  df %>%
    dplyr::mutate(
      pneumo.invasive_status = dplyr::case_when(
        admission.infection_site == "Meningitis" ~ "Invasive disease",
        admission.infection_site == "Septic arthritis" ~ "Invasive disease",
        pneumo.test_type != "Binax only" ~ "Invasive disease",
        TRUE ~ "Non-invasive disease"
      ) %>% factor(c("Non-invasive disease", "Invasive disease")),

      pneumo.test_category = ifelse(
        pneumo.test_type %in% c("Blood culture only", "Blood culture and Binax"),
        "Blood culture","Other"
      ) %>% factor(c("Blood culture","Other"))
    )
}

#' Add in clinical syndrome indicator
#'
#' A list of presentations based on site which
#' * LRTI
#' * Meningitis
#' * Effusion/Empyema
#' * Septic arthritis
#' * URTI
#' * Other
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_pneumo_clinical_syndrome = function(df,v,...) {
  df %>% dplyr::mutate(
    pneumo.clinical_syndrome = dplyr::case_when(
      admission.infection_site == "Meningitis" ~ "Meningitis",
      outcome.pleural_effusion == "yes" | outcome.empyema == "yes" ~ "Effusion/Empyema",
      admission.infection_site == "Lung"  ~ "LRTI",
      admission.infection_site == "Septic arthritis" ~ "Septic arthritis",
      admission.infection_site == "ENT"  ~ "URTI",
      admission.infection_site == "Otitis externa"  ~ "URTI",
      TRUE ~ "Other"
    ) %>% factor(c("LRTI","Meningitis","Effusion/Empyema","Septic arthritis","URTI","Other"))
  )
}


#' Age and CURB score categories
#'
#' This should be consistent with AvonCAP age / CURB cateories.
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_continuous_categories_pneumo = function(df,v,...) {
  df %>%
    dplyr::mutate(
  ) %>% dplyr::mutate(
    demog.age_category = cut(demog.age,breaks = c(0,35,50,65,75,85,Inf), labels = c("18-34","35-49","50-64","65-74","75-84","85+"), include.lowest = FALSE, ordered_result = TRUE),
    demog.age_eligible = cut(demog.age,breaks = c(0,65,Inf), labels = c("18-64","65+"),ordered_result = TRUE),
    admission.cci_category = cut(admission.charlson_comorbidity_index, breaks = c(-Inf,0,2,4,Inf), labels=c("None (0)","Mild (1-2)","Moderate (3-4)","Severe (5+)"), include.lowest = FALSE, ordered_result = TRUE),
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
derive_survival_times_pneumo = function(df,v,...) {
  last_updated = min(c(
    max(df$pneumo.test_date),
    attributes(df)$date
  ))
  df %>%
    dplyr::mutate(

      # length of stay in hospital until death or discharge.
      # NA implies not discharged (or potentially record not updated)
      # N.B. pmax and pmin return NA if all the parallel elements are NA even for na.rm = TRUE.
      survival.length_of_stay = pmin(
        outcome.length_of_stay, outcome.survival_duration, outcome.5_yr_survival_duration,na.rm = TRUE),

      # time to death.
      # NA implies not died or not yet recorded as having died.
      survival.uncensored_time_to_death = pmin(
        outcome.survival_duration, outcome.5_yr_survival_duration, na.rm = TRUE),

      # deaths observed yearly after discharge, data set is not updated.
      survival.last_observed_event =
        (last_updated - pneumo.test_date) %>% ifelse(. < 0, NA, .),

    )
}



