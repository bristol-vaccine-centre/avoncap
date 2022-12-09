

## Study dates and weeks ----

#' Convert a date to a study week
#'
#' @param dates a list of date objects
#'
#' @return an integer number of weeks since 2019-12-30
#' @export
study_week = function(dates) {
  return(as.integer(as.Date(dates)-as.Date("2019-12-30")) %/% 7)
}


#' Convert a study week back into a date
#'
#' This is poorly named as only give the start date is the input is an integer
#'
#' @param study_week does accept decimals and returns the nearest whole date to the value
#'
#' @return a vector of sudy_week numbers
#' @export
start_date_of_week = function(study_week) {
  return(as.Date("2019-12-30")+floor(study_week*7))
}

## Exclusions ----

standard_exclusions = function(avoncap_df, censoring=7) {
  tmp3 = avoncap_df
  v = tmp3 %>% get_value_sets()
  reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date()))

  # Self documenting exclusions
  tmp3 = tmp3 %>%
    dtrackr::exclude_all(
      diagnosis.clinical_or_radiological_LRTI_or_pneumonia==v$diagnosis.clinical_or_radiological_LRTI_or_pneumonia$no & diagnosis.qualifying_symptoms_signs < 2 ~ "{.excluded} with fewer than 2 symptoms and not proven LRTI / pneumonia",
      demog.age<18 ~ "{.excluded} under 18 on admission",
      vaccination.first_dose_brand == v$vaccination.first_dose_brand$Pfizer & vaccination.first_dose_date < "2020-12-08" ~ "{.excluded} with first pfizer before 8/12/2020",
      vaccination.first_dose_brand == v$vaccination.first_dose_brand$AZ & vaccination.first_dose_date < "2021-01-04" ~ "{.excluded} with first AZ before 4/1/2021",
      admission.duration_symptoms > 10 ~ "{.excluded} symptomatic >10 days before admission",
      admission.date > reproduce_at-censoring ~ "{.excluded} with admission after {format(reproduce_at-censoring, '%d/%m/%Y')}",
      admission.episode > 1 ~ "{.excluded} repeat admissions",
      .stage = "standard exclusions"
    )
  return(tmp3)
}




