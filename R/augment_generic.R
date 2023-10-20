
## Categories ---

#' Split a continuous variable into quintiles
#'
#' @param col the continuous data column that is to be categorised by quintile.
#' @param labels the category labels
#'
#' @return a `derive_...` style function that augments a data set with col `xxx`
#'   with col `xxx_quintile` containing the quintiles
#' @export
derive_quintile_category = function(col, labels = c("1-short","2","3","4","5-long")) {
  col = rlang::ensym(col)
  outcol = as.symbol(sprintf("%s_quintile",rlang::as_label(col)))
  n = length(labels)
  function(df, v, ...) {
    df %>% dplyr::mutate(
        !!outcol := cut(
          !!col,
          quantile(!!col, seq(0,1,by=1/n),na.rm=TRUE),
          labels=labels
        ) %>% ordered(levels = labels)
    )
  }
}

## Fix referent categories ----

derive_reordered_factors = function(referents = list(demog.gender = "Female")) {
  function(df,v,...) {
    for (i in seq_along(referents)) {
      col = as.symbol(names(referents)[[i]])
      level = referents[[i]]
      df = df %>% dplyr::mutate(!!col := forcats::fct_relevel(!!col, level))
    }
    return(df)
  }
}

## Dates ---


#' Date columns
#'
#' @param date_col the date column
#' @param prefix a prefix for the columns to be added
#'
#' @importFrom rlang `:=`
#' @return a `derive_...` style function to augment a data set containing `date_col`
#'   with a set of columns describing the timing.
#' @export
derive_pandemic_timings = function(date_col, prefix) {
  date_col = rlang::ensym(date_col)
  .p = function(s) {
    as.symbol(sprintf("%s.%s", prefix, rlang::as_label(ensym(s))))
  }
  return(function(df, ...) {
    df %>% dplyr::mutate(
      !!.p(pre_covid) := factor(ifelse(!!date_col<"2020-03-01","Pre-COVID","Post-COVID"),c("Pre-COVID","Post-COVID")),
      !!.p(pandemic_period) := case_when(
        is.na(!!date_col) ~ NA,
        !!date_col<"2018-03-01" ~ "Pre Mar 18",
        !!date_col<"2018-09-01" ~ "Mar 18-Aug 18",
        !!date_col<"2019-03-01" ~ "Sep 18-Feb 19",
        !!date_col<"2019-09-01" ~ "Mar 19-Aug 19",
        !!date_col<"2020-03-01" ~ "Sep 19-Feb 20",
        !!date_col<"2020-09-01" ~ "Mar 20-Aug 20",
        !!date_col<"2021-03-01" ~ "Sep 20-Feb 21",
        !!date_col<"2021-09-01" ~ "Mar 21-Aug 21",
        !!date_col<"2022-03-01" ~ "Sep 21-Feb 22",
        !!date_col<"2022-09-01" ~ "Mar 22-Aug 22",
        TRUE ~ "Post Aug 22"
      ) %>% factor(c("Pre Mar 18","Mar 18-Aug 18","Sep 18-Feb 19","Mar 19-Aug 19","Sep 19-Feb 20","Mar 20-Aug 20","Sep 20-Feb 21","Mar 21-Aug 21","Sep 21-Feb 22","Mar 22-Aug 22","Post Aug 22")),
      !!.p(pcv_vaccine_period) := case_when(
        is.na(!!date_col) ~ NA,
        !!date_col<"2010-01-01" ~ "2006-2009",
        !!date_col<"2016-01-01" ~ "2010-2015",
        !!date_col<"2020-01-01" ~ "2016-2019",
        !!date_col<"2021-01-01" ~ "2020 (pandemic)",
        !!date_col<"2023-01-01" ~ "2021-2022",
        TRUE ~ "2023 onwards"
      ) %>% factor(c("2006-2009","2010-2015","2016-2019","2020 (pandemic)","2021-2022","2023 onwards"))
    )
  })
}

## Survival ----

#' Survival outcomes
#'
#' Expects as days since admission:
#' * `survival.length_of_stay` - length of stay until discharge or death (NA if still in hosptial),
#' * `survival.uncensored_time_to_death` - time until death (NA if alive at last obs),
#' * `survival.last_observed_event` - last time patient observed alive.
#'
#' Calculates
#' * a 30 day survival duration and censoring status for survfit
#' * a 1 year survival duration and censoring status for survfit
#' * Hospital length of stay and censoring status for survfit
#' * Categorical length of stay and 30 day survival 0-3, 4-6, 7-13, 14-29, gte 30
#'
#' Survival data will be of the form:
#'
#' `survival.30_day_death_xxx`, `survival.1_yr_death_xxx`, `survival.30_day_discharge`
#'
#' `xxx_time`: for this is the follow up time to event in days (max 30 or 365).
#'
#' `xxx_event`: The event type indicator
#' * 0 = alive at event (censored),
#' * 1 = dead.
#'
#' or for length of stay:
#' * 0 = still inpatient / died (censored),
#' * 1 = discharged from hospital
#'
#' A survival model will be of the form:
#'
#' `survival::Surv(time = xxx_time, event=xxx_event) ~ ...`
#'
#' @inherit derive_template
#' @concept derived
#' @export
derive_survival_censoring = function(df,v,...) {
  # last updated date assumed to be the earlier of latest enrollment+30
  # the date the data was downloaded from redcap.
  df %>%
    dplyr::mutate(

      survival.length_of_stay_category =
        avoncap::cut_integer(survival.length_of_stay, c(4,7,14,30), lower_limit = 0),

      survival.30_day_death_time = dplyr::case_when(
        survival.uncensored_time_to_death <= 30 ~ survival.uncensored_time_to_death,  # confirmed dead
        survival.last_observed_event > 30 ~ 30, # censored
        TRUE ~ survival.last_observed_event, # censored
      ),
      survival.30_day_death_event = dplyr::case_when(
        survival.uncensored_time_to_death <= 30 ~ 1,  # confirmed dead
        survival.last_observed_event > 30 ~ 0, # censored
        TRUE ~ 0, # censored
      ),
      survival.1_yr_death_time = dplyr::case_when(
        survival.uncensored_time_to_death <= 365 ~ survival.uncensored_time_to_death,  # confirmed dead
        survival.last_observed_event > 365 ~ 365, # censored
        TRUE ~ survival.last_observed_event, # censored
      ),
      survival.1_yr_death_event = dplyr::case_when(
        survival.uncensored_time_to_death <= 365 ~ 1,  # confirmed dead
        survival.last_observed_event > 365 ~ 0, # censored
        TRUE ~ 0, # censored
      ),
      survival.30_day_discharge_time = dplyr::case_when(
        survival.length_of_stay <= 30 ~ survival.length_of_stay, # discharged or died
        survival.last_observed_event > 30 ~ 30, # censored
        TRUE ~ survival.last_observed_event # maybe died or short follow up / censored
      ),
      survival.30_day_discharge_event = dplyr::case_when(
        survival.length_of_stay <= 30 &
          survival.length_of_stay == survival.uncensored_time_to_death ~ 0, # died / censored
        survival.length_of_stay <= 30 ~ 1, # discharged
        survival.last_observed_event > 30 ~ 0, # did not die / censored
        TRUE ~ 0 # maybe died or short follow up / censored
      )
    )
}
