## Standard data manipulation post load ----

# TODO: use this more?
.opt = function(...) {
  tryCatch(rlang::eval_tidy(...), error=function(e) rlang::eval_tidy(FALSE))
}

# Apply an augmentation to the dataframe

#' Applies a set of functions to the whole dataframe
#'
#' This sequences, catches errors and allows parameters to be passed by name
#'
#' @param df a data frame
#' @param ... unnamed parameters are a list of functions, named parameters
#'   are passed to those functions (if they match formal arguments).
#'
#' @return the altered df
#' @export
#'
#' @examples
#' fn1 = function(df,v) {df %>% dplyr::filter(cut=="Fair") %>% dplyr::mutate(x_col = color)}
#' fn2 = function(df,v) {df %>% dplyr::filter(color==v$color$J)}
#' df = diamonds %>% augment_generic(fn1, fn2)
augment_generic = function(df, ...) {
  dots = rlang::list2(...)
  if (!is.null(names(dots))) {
    fns = dots[names(dots) == ""]
    params = dots[names(dots) != ""]
  } else {
    fns = dots
    params = list()
  }
  #TODO: store error messages in an attribute and retrieve later.
  .cached({
    tmp = df
    for (fn in fns) {
      fn = purrr::as_mapper(fn)
      used_cols = all.vars(body(fn)) %>% intersect(colnames(df))
      if ("..." %in% names(formals(fn))) {
        used_params = names(params)
      } else {
        used_params = names(formals(fn)) %>% intersect(names(params))
      }
      tmp_params = params[names(params) %in% used_params]
      v = avoncap::get_value_sets(df)

      tmp = tryCatch({




        df2 = tmp %>% dtrackr::pause()
        df2 = rlang::exec(fn, df2, v, !!!tmp_params)
        df2 = df2 %>% dtrackr::resume()

        # copy old attributes to new data
        df2 = df2 %>% .restore_attributes(tmp)

        new_cols = colnames(df2) %>% setdiff(colnames(tmp))
        # TODO: look into dtrackr for this.
        df2 = df2 %>%
          dplyr::mutate(dplyr::across(tidyselect::any_of(new_cols), .fns = ~ .x %>% magrittr::set_attr("depends", used_cols)))
        message("Created ",paste0(new_cols,collapse=", ")," using: ",paste0(used_cols,collapse=", "))
        df2
      }, error = function(e) {
        message("Skipping function due to error: ", e$message)
        tmp
      })
    }
    tmp
  }, df, deparse1(fns), params, .prefix = "augment") %>% return()
}

#' Derived data function template
#'
#' @param df the dataframe.
#' @param v the value set. usually precomputed by the augment framework the value
#' set can be explicitly supplied with `v = get_value_sets(df)`
#' @param ... ignored
#'
#' @return a dataframe
#' @export
derive_template = function(df,v,...) {
  df
}

## Specific dataset dispatch ----

#' Sanitise AvonCap data columns
#'
#' AvonCap data has lots of columns which are named in a difficult to remember
#' fashion, composed of data items that have enumerated values with no
#' semantics. This makes displaying them difficult and any filtering done on the
#' raw data inscrutable. Depending on the source of the data some different
#' columns may be present due to differences in the NHS and UoB data sets. The
#' redcap database has some options that may be checklists and some that are
#' radio buttons, both of these end up with mysterious names in the data.
#'
#' This function maps the data into a tidy dataframe with consistently named
#' columns, and named factors where appropriate. If not present in the data the
#' ethnicity
#'
#' files Most of the sanitisation code is held in the
#' `zzz-avoncap-mappings.R` file.
#'
#' @param x - the raw data from `load_data()`
#' @inheritDotParams augment_generic
#'
#' @return a tracked dataframe with
#' @export
augment_data = function(
    x,
    ...
) {

  type = attr(x,"type")
  subtype = attr(x,"subtype")
  instrument = attr(x,"instrument")
  norm_fn = c("augment",type,subtype,instrument) %>% stringr::str_replace("-","_") %>% paste0(collapse=".")
  message("Augmenting data using: ",norm_fn)
  norm_fn = tryCatch(
    # utils::getFromNamespace(norm_fn,"avoncap"),
    get(norm_fn),
    error = function(e) {
      message("No data augmentation defined for ",norm_fn)
      return(function(x, ...) x)
    }
  )
  # dispatch the call to the specific subtype of the call
  # or to a noop function if there is no specific method
  norm_fn(x, ...)

}


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
      derive_simpler_comorbidities,
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
      derive_hospital_burden_outcomes,
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
      ~ .x # a no-op example
      ,...)
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
      derive_simpler_comorbidities,
      # comorbid.interstitial_lung_dx not found
      # derive_pneumococcal_high_risk,
      # derive_pneumococcal_risk_category,
      derive_phe_pcv_group,
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
