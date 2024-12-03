.ifnull = function(x,default) {
  if (is.null(x)) default else x
}

# Dataset specific normalisation recipes ----

# # the generic
# normalise = function(rawData, ...) {
#   return(rawData)
# }

normalise.avoncap_export.uad_controls = function(rawData, ...) {
  # Controls database has some naming inconsistencies:
  warning("This hasn't been updated since the change to admission_dates reconstruction")
  # TODO: we have admission dates as data now so we should probably use those wherever possible
  rawData %>%
    .reconstruct_admission_date(...) %>%
    .reconstruct_hospital(...) %>%
    normalise_generic(
      mappings=map_avoncap_central() %>% .rename_mapping(
        "ac_study_number" = "aconvap_number",
        "record_number" = "study_number",
        "ethnicity2" = "ethnicity"
      ), ...) %>%
    create_keys(keys_avoncap_central())
}

## ED ----

normalise.avoncap_export.ed = function(rawData, ...) {
  # Controls database has some naming inconsistencies:
  warning("This hasn't been updated since the change to admission_dates reconstruction")
  # TODO: we have admission dates as data now so we should probably use those wherever possible
  rawData %>%
    .reconstruct_admission_date(...) %>%
    .reconstruct_hospital(...) %>%
    normalise_generic(mappings=
      c(
        map_avoncap_ed_consent(),
        map_avoncap_central(), # %>% .remove_mapping(c("hospital_length_of_stay")),
        map_avoncap_ed()
      ),
      ...
    ) %>%
    create_keys(keys_avoncap_ed()) %>%
    .derive_consent_flag_ed() %>%
    .wipe_non_consented_data()
}

normalise.avoncap_export.ed.haem = function(rawData, ...) {
  rawData %>%
    normalise_generic(mappings = c(
      map_avoncap_ed_consent(),
      map_avoncap_haem()
    ),...) %>%
    create_keys(keys_avoncap_ed()) %>%
    .derive_consent_flag_ed() %>%
    .exclude_non_consented_patients()
}

normalise.avoncap_export.ed.micro = function(rawData, ...) {
  .cached({
    tmp = tibble::tibble()
    for (i in .repeat_instrument(rawData, "micro")) {
      tmp2 = rawData %>%
        normalise_generic(mappings=c(
          map_avoncap_ed_consent(),
          map_avoncap_micro(i)
        ), ...) %>%
        create_keys(keys_avoncap_micro(i))
      # dplyr::mutate(micro.test_id = paste0(admin.record_number,"_micro_",i))
      tmp = tmp %>% dplyr::bind_rows(tmp2)
    }
    tmp %>%
      create_keys(keys_avoncap_ed()) %>%
      .derive_consent_flag_ed() %>%
      .exclude_non_consented_patients()
  },rawData,..., .prefix="norm")
}

normalise.avoncap_export.ed.virol = function(rawData, ...) {
  .cached({
    tmp = tibble::tibble()
    for (i in .repeat_instrument(rawData, "virol")) {
      tmp2 = rawData %>%
        normalise_generic(mappings=c(
          map_avoncap_ed_consent(),
          map_avoncap_virol(i)
        ), ...) %>%
        create_keys(keys_avoncap_virol(i))
      # dplyr::mutate(virol.test_id = paste0(admin.record_number,"_virol_",i))
      tmp = tmp %>% dplyr::bind_rows(tmp2)
    }
    tmp %>% create_keys(keys_avoncap_ed()) %>%
      .derive_consent_flag_ed() %>%
      .exclude_non_consented_patients()
  },rawData,..., .prefix="norm")
}

normalise.avoncap_export.ed.radio = function(rawData, ...) {
  .cached({
    tmp = tibble::tibble()
    for (i in .repeat_instrument(rawData, "radio")) {
      tmp2 = rawData %>%
        normalise_generic(mappings=c(
          map_avoncap_ed_consent(),
          map_avoncap_radio(i)
        ), ...) %>%
        create_keys(keys_avoncap_radio(i))
      # TODO:
      # dplyr::mutate(radio.test_id = paste0(admin.record_number,"_radio_",i))
      tmp = tmp %>% dplyr::bind_rows(tmp2)
    }
    tmp %>% create_keys(keys_avoncap_ed()) %>%
      .derive_consent_flag_ed() %>%
      .exclude_non_consented_patients()
  },rawData,..., .prefix="norm")
}

## Central ----

normalise.avoncap_export.central = function(rawData, ...) {
  rawData %>%
    .merge_ethnicity(...) %>%
    .merge_admission_dates(...) %>%
    .reconstruct_admission_date(...) %>%
    normalise_generic(mappings=c(
        map_avoncap_consent(),
        map_avoncap_central()
      ), ...) %>%
    create_keys(keys_avoncap_central()) %>%
    .derive_consent_flag() %>%
    .wipe_non_consented_data()
}



normalise.avoncap_export.central.micro = function(rawData, ...) {
  .cached({
    tmp = tibble::tibble()
    for (i in .repeat_instrument(rawData, "micro")) {
      tmp2 = rawData %>%
        normalise_generic(mappings=c(
          map_avoncap_consent(),
          map_avoncap_micro(i)
        ), ...) %>%
        create_keys(keys_avoncap_micro(i))
        # dplyr::mutate(micro.test_id = paste0(admin.record_number,"_micro_",i))
      tmp = tmp %>% dplyr::bind_rows(tmp2)
    }
    tmp %>% create_keys(keys_avoncap_central()) %>%
      .derive_consent_flag() %>%
      .exclude_non_consented_patients()
  },rawData,..., .prefix="norm")
}

normalise.avoncap_export.central.virol = function(rawData, ...) {
  .cached({
    tmp = tibble::tibble()
    for (i in .repeat_instrument(rawData, "virol")) {
      tmp2 = rawData %>%
        normalise_generic(mappings=c(
          map_avoncap_consent(),
          map_avoncap_virol(i)
        ), ...) %>%
        create_keys(keys_avoncap_virol(i))
        # dplyr::mutate(virol.test_id = paste0(admin.record_number,"_virol_",i))
      tmp = tmp %>% dplyr::bind_rows(tmp2)
    }
    tmp %>% create_keys(keys_avoncap_central()) %>%
      .derive_consent_flag() %>%
      .exclude_non_consented_patients()
  },rawData,..., .prefix="norm")
}

normalise.avoncap_export.central.radio = function(rawData, ...) {
  .cached({
    tmp = tibble::tibble()
    for (i in .repeat_instrument(rawData, "radio")) {
      tmp2 = rawData %>%
        normalise_generic(mappings=c(
          map_avoncap_consent(),
          map_avoncap_radio(i)
        ), ...) %>%
        create_keys(keys_avoncap_radio(i))
        # TODO:
        # dplyr::mutate(radio.test_id = paste0(admin.record_number,"_radio_",i))
      tmp = tmp %>% dplyr::bind_rows(tmp2)
    }
    tmp %>% create_keys(keys_avoncap_central()) %>%
      .derive_consent_flag() %>%
      .exclude_non_consented_patients()
  },rawData,..., .prefix="norm")
}

normalise.avoncap_export.central.haem = function(rawData, ...) {
  rawData %>%
    normalise_generic(mappings = c(
      map_avoncap_consent(),
      map_avoncap_haem()
    ),...) %>%
    create_keys(keys_avoncap_central()) %>%
    .derive_consent_flag() %>%
    .exclude_non_consented_patients()
}

normalise.nhs_extract.deltave = function(rawData, ...) {
  rawData %>%
    .merge_ethnicity(...) %>%
    .reconstruct_admission_times(...) %>%
    normalise_generic(mappings = c(
      map_avoncap_consent(),
      map_avoncap_central()
    ), ...) %>%
    create_keys(keys_avoncap_central()) %>%
    .derive_consent_flag() %>%
    .wipe_non_consented_data()
}

normalise.nhs_extract.pneumococcal = function(rawData, ...) {
  rawData %>%
    normalise_generic(mappings = map_avoncap_pneumococcal(), ...) %>%
    create_keys(keys_avoncap_pneumococcal())
}

normalise.urine_antigens.serotype = function(rawData, ...) {
  rawData %>%
    dplyr::filter(ANALYSIS != "BINAX") %>%
    normalise_generic(mappings=map_urine_antigens(), ...) %>%
    tidyr::complete(
      pneumo.urine_antigen_test,
      tidyr::nesting(
        admin.consented_record_number,
        pneumo.test_date
      ),
      fill = list(pneumo.urine_antigen_result="Unknown")
    ) %>%
    dplyr::mutate(pneumo.urine_antigen_result = factor(pneumo.urine_antigen_result, levels = c("Negative","Positive","Other","Unknown"))) %>%
    create_keys(keys_urine_antigens_serotype()) %>%
    dplyr::rename(sample_id = pneumo.urine_antigen_sample_id, test = pneumo.urine_antigen_test, result=pneumo.urine_antigen_result) %>%
    tidyr::nest(pneumo.urine_antigen = c(sample_id,test,result))
}

normalise.urine_antigens.binax = function(rawData, ...) {
  rawData %>% dplyr::filter(ANALYSIS == "BINAX") %>%
    normalise_generic(mappings=map_urine_binax(), ...) %>%
    dplyr::mutate(pneumo.binax_result = factor(pneumo.binax_result, levels = c("Negative","Positive","Other","Unknown"))) %>%
    create_keys(keys_urine_antigens_binax())
}




# Helper preprocessing steps ----
# These are reusable bits of logic to fix specific deficiencies in the
# different avoncap extracts

.merge_ethnicity = function(rawData, ethn = try(avoncap::load_data("ethnicity",reproduce_at = Sys.Date()),silent = TRUE), ...) {

  if (!"record_number" %in% colnames(rawData)) {
    message("Aborting attempt to assign ethnicity to a data set without a `record_number` identifier")
    return(rawData)
  }
  # some data sets have ethnicity in them already, ethnicity2 is the name of the column
  if (!"ethnicity2" %in% colnames(rawData)) {

    if ("try-error" %in% class(ethn)) {
      ethn = NULL
      message("No ethnicity data was found, please supply a value to the ethn parameter if needed.")
    }

    if(!is.null(ethn)) {

      # make sure unique in case we loaded data more than once.
      ethn = ethn %>% dplyr::group_by(record_number) %>% dplyr::filter(dplyr::row_number()==1) %>% dplyr::ungroup()

      if (!"ethnicity2" %in% colnames(ethn)) ethn = ethn %>% dplyr::rename(ethnicity2 = ethnicity)
      rawData = rawData %>%
        dplyr::left_join(ethn %>% dplyr::select(record_number, ethnicity2) , by="record_number", suffix = c("",".ethn"))
    }
  }
  return(rawData)
}


.merge_admission_dates = function(rawData, dates = try(avoncap::load_data("admission-dates",reproduce_at = Sys.Date()),silent = TRUE), ...) {

  if (!"record_number" %in% colnames(rawData)) {
    message("Aborting attempt to assign ethnicity to a data set without a `record_number` identifier")
    return(rawData)
  }
  # some data sets have ethnicity in them already, ethnicity2 is the name of the column
  if (!"admission_date" %in% colnames(rawData)) {

    if ("try-error" %in% class(dates)) {
      dates = NULL
      message("No admission-date data was found, please supply a value to the dates parameter if needed.")
    }

    if(!is.null(dates)) {

      # make sure unique in case we loaded data more than once.
      dates = dates %>%
        dplyr::group_by(record_number) %>%
        dplyr::filter(dplyr::row_number()==1) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(
          record_number,
          admission_date = as.Date(admission_date,"%d/%m/%Y"))

      rawData = rawData %>%
        dplyr::left_join(dates , by="record_number", suffix = c("",".dates"))
    }
  }
  return(rawData)
}

# TODO: convert this to a derive_ function
# as long as there are done first then we should be OK.
.reconstruct_admission_times = function(rawData, ...) {
  # The dates vary between the different sources
  # This makes sure everything has
  # a numeric year,
  # an admission_date (which may be start of week),
  # a week_number (epiweek),
  # a study_week (weeks since 2019-12-30)
  # a study_year (years since 2019)
  tmp = rawData
  if ("admission_date" %in% colnames(tmp)) {
    # the NHS data set has and admission date
    # bit not week_number / study_week / study_year
    tmp = tmp %>%
      dplyr::mutate(
        year = lubridate::year(admission_date),
        week_number = lubridate::epiweek(admission_date),
        study_week = study_week(admission_date),
        study_year = year-2019+ifelse(week_number>31,0,1)
      )
  } else {

  }
}

# TODO: convert this to a derive_ function
# as long as there are done first then we should be OK.
.reconstruct_admission_times_legacy = function(rawData, ...) {
  # The dates vary between the different sources
  # This makes sure everything has
  # a numeric year,
  # an admission_date (which may be start of week),
  # a week_number (epiweek),
  # a study_week (weeks since 2019-12-30)
  # a study_year (years since 2019)
  tmp = rawData
  if ("admission_date" %in% colnames(tmp)) {
    # the NHS data set has and admission date
    # bit not week_number / study_week / study_year
    tmp = tmp %>%
      dplyr::mutate(
        year = lubridate::year(admission_date),
        week_number = lubridate::epiweek(admission_date),
        study_week = study_week_legacy(admission_date),
        study_year = year-2019+ifelse(week_number>30,0,1)
      )
  } else {

  }
}

.reconstruct_admission_date_legacy = function(rawData,...) {
  tmp = rawData
  if (all(c("study_year","week_number") %in% colnames(tmp))) {
    tmp = tmp %>%
      # the Bristol data set has week_number which is a week number from start of the study in that year.
      # The study starts on week 31. Therefore for the 20-21 database (i.e. study_year 1) weeks 31-52 are in 2020 and 0-30 are in 2021
      # we get the year from the file-name itself (which assumes it was correctly named)
      dplyr::mutate(
        year = dplyr::case_when(
          !is.na(year) & is.numeric(year) & year >= 2020 & year < 2025 ~ year,
          study_year == 1 & week_number>30 ~ 2020,
          study_year == 1 & week_number<=30 ~ 2021,
          study_year == 2 & week_number>30 ~ 2021,
          study_year == 2 & week_number<=30 ~ 2022,
          study_year == 3 & week_number>30 ~ 2022,
          study_year == 3 & week_number<=30 ~ 2023,
          #study_year == 3 & week_number>31 ~ 2022,
          #study_year == 3 & week_number<=31 ~ 2023,
          study_year == 4 & week_number>30 ~ 2023,
          study_year == 4 & week_number<=30 ~ 2024,
          TRUE ~ NA_real_
        )) %>%
      dplyr::mutate(
        study_week = (year-2020)*52+week_number-1,
        # study_week = (year-2020)*52+week_number-1+ifelse(year>2022,1,0),
        admission_date = start_date_of_week_legacy(study_week)
        # week_number = lubridate::epiweek(admission_date)
      )
    message("Admission date is derived from study week and hence approximate.")
  } else {
    if ("enrollment_date" %in% colnames(tmp)) {
      tmp = tmp %>% dplyr::mutate(admission_date = enrollment_date)
      warning("Using enrollment_date as an approximate admission date.")
    } else {
      warning("The loaded data has neither an admission_date column, nor both of study_year, and week_number")
      warning("and no enrollment_date either.")
      warning("Was rawData loaded with avoncap::load_data(...)? If not the study_year number may be missing.")
      stop("Aborted.")
    }
  }
  return(tmp)
}


.reconstruct_admission_date = function(rawData,...) {
  tmp = rawData
  if (!"admission_date" %in% colnames(tmp)) {
    tmp = tmp %>% dplyr::mutate(admission_date=NA)
    message("Admission date is derived from study week and hence approximate.")
  }
  if (all(c("study_year","week_number") %in% colnames(tmp))) {
    tmp = tmp %>%
      # the Bristol data set has week_number which is a week number from start of the study in that year.
      # The study starts on week 31 but annoyingly this is a short week.
      # Therefore for the 20-21 database (i.e. study_year 1) weeks 32-52 are in 2020 and most of 0-31 are in 2021
      # we get the year from the file-name itself (which assumes it was correctly named)
      # We only use this heuristic when the year is not given and the admission date is not known.
      # This is mostly un-consented patients who may get an inferred date that is wrong
      dplyr::mutate(
        year = dplyr::case_when(
          !is.na(year) & is.numeric(year) & year >= 2020 & year < 2025 ~ year,
          study_year == 1 & week_number>31 ~ 2020,
          study_year == 1 & week_number<=31 ~ 2021, # This is wrong for the first 2 days data but not used
          study_year == 2 & week_number>31 ~ 2021,
          study_year == 2 & week_number<=31 ~ 2022,
          study_year == 3 & week_number>31 ~ 2022,
          study_year == 3 & week_number<=31 ~ 2023,
          study_year == 4 & week_number>31 ~ 2023,
          study_year == 4 & week_number<=31 ~ 2024,
          TRUE ~ NA_real_
        )) %>%
      dplyr::left_join(
        avoncap::year_week_number_lookup, by=c("year","week_number"), suffix=c("",".inferred")
      ) %>%
      dplyr::mutate(
        admission_date = as.Date(ifelse(is.na(admission_date), start_of_week, admission_date)),
        study_week = ifelse(is.na(study_week), study_week(admission_date), study_week)
      )

  } else {
    if ("enrollment_date" %in% colnames(tmp)) {
      tmp = tmp %>% dplyr::mutate(admission_date = enrollment_date)
      warning("Using enrollment_date as a VERY approximate admission date.")
    } else {
      warning("The loaded data has neither an admission_date column, nor both of study_year, and week_number")
      warning("and no enrollment_date either.")
      warning("Was rawData loaded with avoncap::load_data(...)? If not the study_year number may be missing.")
      stop("Aborted.")
    }
  }
  return(tmp)
}


.reconstruct_hospital = function(rawData, ...) {
  tmp = rawData
  if (!"hospital" %in% colnames(tmp)) {
    if (!"record_number" %in% colnames(rawData)) {
      message("Aborting attempt to infer hospital from a data set without a `record_number` identifier")
      return(rawData)
    }
    # The hospital may be known from the file the data came from. If not we can work it out from
    # the record number
    tmp = tmp %>% dplyr::mutate(hospital = dplyr::case_when(
      # in the controls database the hospital numbers are different, NC for:
      # NBT, startswith numeric for BRI
      tolower(substr(record_number,1,2)) == "nc" ~ "NBT",
      substr(record_number,1,1) %in% as.character(0:9) ~ "BRI",
      # in the main database an "A" is an early BRI patient, then the moved to "B":
      tolower(substr(record_number,1,1)) == "a" ~ "BRI",
      tolower(substr(record_number,1,1)) == "b" ~ "BRI",
      tolower(substr(record_number,1,1)) == "n" ~ "NBT",
      TRUE ~ NA_character_
    ))
  }
  return(tmp)
}

# Consent postprocessing steps (avoncap) ----

## Admin ----

.derive_consent_flag_ed = function(df) {
  df = df %>% dplyr::mutate(
    admin.consent_withheld = dplyr::case_when(
      is.na(admin.opted_out) ~ "yes",
      admin.opted_out == "yes" ~ "yes",
      TRUE ~ "no"
    ) %>% factor(levels = c("no","yes")),

    admin.consent_type = dplyr::case_when(
      is.na(admin.opted_out) ~ "Missing",
      admin.opted_out == "yes" ~ "Declined",
      TRUE ~ "Section 251"
    ) %>% factor(levels = c("Explicit","Section 251","Declined","Withdrew","Missing")),

    admin.consent_samples = factor("None", levels = c("All samples", "Urine & resp", "Urine only", "Resp only", "None"))
  )

  return(df)
}

.derive_consent_flag = function(df) {
  df = df %>% dplyr::mutate(
    admin.consent_withheld = dplyr::case_when(
      is.na(admin.consented) & is.na(admin.pp_consented) & is.na(admin.withdrawal) ~ "yes",
      admin.consented == "Declined consent" ~ "yes",
      admin.pp_consented == "Declined consent" ~ "yes",
      admin.withdrawal == "yes" ~ "yes",
      TRUE ~ "no"
    ) %>% factor(levels = c("no","yes")),

    admin.consent_type = dplyr::case_when(
      is.na(admin.consented) & is.na(admin.pp_consented) & is.na(admin.withdrawal) ~ "Missing",
      admin.withdrawal == "yes" ~ "Withdrew",
      admin.consented == "Declined consent" ~ "Declined",
      admin.pp_consented == "Declined consent" ~ "Declined",
      admin.consented == "Yes"  ~ "Explicit",
      admin.pp_consented == "Yes" ~ "Explicit",
      TRUE ~ "Section 251"
    ) %>% factor(levels = c("Explicit","Section 251","Declined","Withdrew","Missing")),
  )

  df = tryCatch(
    df %>% dplyr::mutate(
      admin.consent_samples = dplyr::case_when(
        admin.consent_for_urine == "yes" & admin.consent_for_blood  == "yes" &
          admin.consent_for_respiratory_samples  == "yes" ~ "All samples",
        admin.consent_for_urine == "yes" &
          admin.consent_for_respiratory_samples  == "yes" ~ "Urine & resp",
        admin.consent_for_urine == "yes" ~ "Urine only",
        admin.consent_for_respiratory_samples == "yes" ~ "Resp only",
        TRUE ~ "None"
      ) %>% factor(levels = c("All samples", "Urine & resp", "Urine only", "Resp only", "None"))
    ),
    error = function(e) df
  )

  return(df)

}

## Non consented patients ----
#
# # fct = factor(c("a","b","c","a")) %>% magrittr::set_attr("test",1)
# # .ifelsefct(fct=="a",NA,fct)
# .ifelsefct = function(pred, t, f) {
#
#   if (is.factor(f)) {
#     x = dplyr::if_else(pred, t, as.character(levels(f))[f])
#     x = factor(x, levels = levels(f), ordered = is.ordered(f))
#     return(x)
#   } else {
#     return(dplyr::if_else(pred, t, f))
#   }
#
# }

.wipe_non_consented_data = function(df) {
  # chacnge
  df %>% dplyr::mutate(
    dplyr::across(
      .cols = c(-dplyr::starts_with("admin"),-dplyr::starts_with("key"),-dplyr::any_of(c("admission.date","admission.study_week", "admission.year"))),
      .fns = ~ dplyr::if_else(admin.consent_withheld == "yes", NA, .x)
    )
  )
}

.exclude_non_consented_patients = function(df) {
  df %>% dplyr::filter(admin.consent_withheld == "no")
}

#
#   if(remove_mapped) {
#     tmp = tmp %>% dplyr::select(-tidyselect::starts_with(".."))
#   } else {
#     message("renamed original columns as: ",tmp %>% dplyr::select(tidyselect::starts_with("..")) %>% colnames() %>% paste0(collapse="; "))
#   }
#
#   unmappedCols = tmp %>% dplyr::select(c(tidyselect::starts_with("."), -tidyselect::starts_with(".."))) %>% colnames()
#   message("Did not map ", unmappedCols %>% length()," columns")
#
#   # detect multiple admission episodes based on nhs_number, if present
#
