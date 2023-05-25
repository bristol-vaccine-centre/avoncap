

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



#' Cut and label an integer valued quantity
#'
#' Deals with some annoying issues classifying integer data sets, such as ages, into groups. where you want to
#' specify just the change over points as integers and clearly label the resulting ordered factor.
#'
#' @param x a vector of integer valued numbers, e.g. ages, counts
#' @param cut_points a vector of integer valued cut points which define the lower boundaries of conditions
#' @param glue a glue spec that may be used to generate a label. It can use {low}, {high}, {next_low}, or {label} as values.
#' @param lower_limit the minimum value we should include (this is inclusive for the bottom category) (default -Inf)
#' @param upper_limit the maximum value we should include (this is also inclusive for the top category) (default Inf)
#' @param ... not used
#'
#' @return an ordered factor of the integer
#' @export
#'
#' @examples
#' cut_integer(rbinom(20,20,0.5), c(5,10,15))
#' cut_integer(floor(runif(100,-10,10)), cut_points = c(2,3,4,6), lower_limit=0, upper_limit=10)
cut_integer = function(x, cut_points, glue = "{label}", lower_limit = -Inf, upper_limit = Inf, ...) {

  next_low = NULL # remove global binding note

  if (!all(as.integer(x)==x,na.rm = TRUE)) warning("input to cut_integer(...) has been coerced to integer values")
  x = floor(x)
  if (!all(as.integer(cut_points)==cut_points)) stop("cut_points must be integer valued, and define the lower end of each category.")
  if (any(cut_points <= lower_limit | cut_points >= upper_limit)) warning("cut_point values should be between lower_limit (",lower_limit,") and upper_limit (",upper_limit,").")
  # make sure the limits are not included.
  cut_points = cut_points[cut_points > lower_limit & cut_points < upper_limit]
  # sort and uniquify
  #
  breaks = unique(sort(c(lower_limit,cut_points,upper_limit+1)))
  labels = tibble::tibble(
    low = utils::head(breaks,-1),
    next_low = utils::head(dplyr::lead(breaks,n = 1),-1),
    high = ifelse(next_low != upper_limit, next_low-1, upper_limit)
  ) %>% dplyr::mutate(
    label = dplyr::case_when(
      low == high ~ sprintf("%1.0f",low),
      low == -Inf ~ sprintf("<%1.0f",next_low),
      high == Inf ~ sprintf("\u2265%1.0f",low),
      TRUE ~ sprintf("%1.0f\u2012%1.0f",low, high)
    )
  ) %>% dplyr::mutate(
    label2 = glue::glue(glue)
  )

  return(
    cut(x,include.lowest = TRUE,breaks = breaks, labels=labels$label2, ordered_result = TRUE, right=FALSE)
  )
}
