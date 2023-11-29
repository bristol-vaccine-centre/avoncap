#' Polyfill ED data
#'
#' The ED data has some different fields from the main avoncap data.
#'
#' * It is missing an admission cxr summary field needed to calculate pneumonia
#' * It has a fixed admission route of "A&E" (i.e. ED to non UK people)
#' * None of the patients admitted
#' * Hospital admission length of stay is zero
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_polyfill_ed = function(df,v,...) {
  df %>%
    dplyr::mutate(

      admission.cxr_pneumonia = dplyr::case_when(
        radio.consistent_with_pneumonia_1 == "yes" ~ "yes",
        radio.consistent_with_pneumonia_2 == "yes" ~ "yes",
        radio.consistent_with_pneumonia_1 == "no" ~ "no",
        radio.consistent_with_pneumonia_2 == "no" ~ "no",
        TRUE ~ NA_character_
      ) %>% factor(levels=c("no","yes")),

      admission.admission_route = factor("Emergency Department", levels=c("Emergency Department","Other")),

      outcome.admitted_to_hospital = factor("no", levels=c("no","yes")),

      outcome.death_within_30_days =
        ifelse(!(outcome.functional_status != "Deceased" | outcome.survival_duration > 30), "confirmed", "not recorded") %>% factor(levels=c("confirmed", "not recorded"))


    )
}
