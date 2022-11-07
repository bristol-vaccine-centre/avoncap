## Data normalisation infrastructure ----


#' Sanitise AvonCap data columns
#'
#' AvonCap data has lots of columns which are named in a difficult to remember fashion, composed of
#' data items that have enumerated values with no semantics. This makes displaying them difficult and
#' any filtering done on the raw data inscrutable. Depending on the source of the data some different columns
#' may be present due to differences in the NHS and UoB data sets. The redcap database has some options that may be
#' checklists and some that are radio buttons, both of these end up with mysterious names in the data.
#'
#' This function maps the data into a tidy dataframe with consistently named columns, and named factors where appropriate.
#' If not present in the data the ethnicity files
#' Most of the sanitisation code is held in the `zzz-avoncap-mappings.R` file.
#'
#' @param rawData - the raw data from `load_data()`
#' @param ethn the supplementary ethnicity files. but default will use the call `avoncap::most_recent_files("ethnicity")` to find these files and will- can be set to "NULL" if ethnicity files are not available.
#' @param remove_mapped gets rid of original columns for which we have a mapping (leaving the new versions)
#' @param remove_unmapped gets rid of columns for which we do not have a mapping
#' @param mappings a set of mappings (see `zzz-avoncap-mappings.R`)
#' @param messages a set of `dtrackr` glue specs that populate the first box fo the flow chart. (can use `{files}`, `{reproduce_at}`, `{date}`, `{.total}`)
#' @param reproduce_at the date at which to reproduce the analysis to (`options(reproduce.at = "...")`)
#' @param data_source_info - if not null a filename, and the function will write out a file with the details of the input files used.
#' @param ... passed onto `.cached(...)`. e.g. `nocache = TRUE` can be used to defeat caching.
#'
#' @return a tracked dataframe with
#' @export
normalise_data = function(
    rawData,
    ethn = try(avoncap::load_data("ethnicity",reproduce_at = Sys.Date()),silent = TRUE),
    remove_mapped = TRUE,
    remove_unmapped = TRUE,
    mappings=.mappings,
    messages = c("files: {files}","{.total} rows","analysis date: {reproduce_at}","files from: {date}"),
    reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date())),
    data_source_info = NULL,
    ...
) {

  files = attr(rawData,"file")
  date = attr(rawData,"date")

  if(!is.null(data_source_info)) save_data_source_info(rawData, ethn, .file = data_source_info)

  # Controls database has come naming inconsistencies:
  if ("aconvap_number" %in% colnames(rawData)) {
    rawData = rawData %>% rename(
      ac_study_number = aconvap_number,
      record_number = study_number,
      ethnicity2 = ethnicity
    ) %>% mutate(uad_control = TRUE)
  } else {
    rawData = rawData %>% mutate(uad_control = FALSE)
  }

  # some data sets have ethnicity in them already, ethnicity2 is the name of the column
  if (!"ethnicity2" %in% colnames(rawData)) {

    if ("try-error" %in% class(ethn)) {
      ethn = NULL
      message("No ethnicity data was found, please supply a value to the ethn parameter if needed.")
    }

    if(!is.null(ethn)) {
      if (!"ethnicity2" %in% colnames(ethn)) ethn = ethn %>% dplyr::rename(ethnicity2 = ethnicity)
      rawData = rawData %>%
        dplyr::left_join(ethn %>% dplyr::select(record_number, ethnicity2) , by="record_number", suffix = c("",".ethn"))
    }

  }



  tomap = names(.mappings)
  missingCols = tomap[unlist(purrr::map(tomap, ~ !any(colnames(rawData) %>% stringr::str_starts(.x))))]

  if (length(missingCols)>0) message("Some columns expected in the mappings were not present: ",paste0(missingCols,collapse=", "))

  .cached({

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
      tmp = tmp %>%
        dplyr::mutate(
          year = lubridate::year(admission_date),
          week_number = lubridate::epiweek(admission_date),
          study_week = study_week(admission_date),
          study_year = year-2019+ifelse(week_number>30,0,1)
        )
    } else if (all(c("study_year","week_number") %in% colnames(tmp))) {
      tmp = tmp %>%
        # the Bristol data set has week_number which is a week number from start of the study in that year.
        # The study starts on week 31. Therefore for the 20-21 database (i.e. study_year 1) weeks 31-52 are in 2020 and 0-30 are in 2021
        # we get the year from the file-name itself (which assumes it was correctly named)
        dplyr::mutate(
          year = dplyr::case_when(
            is.numeric(year) & !is.na(year) ~ year,
            study_year == 1 & week_number>30 ~ 2020,
            study_year == 1 & week_number<=30 ~ 2021,
            study_year == 2 & week_number>30 ~ 2021,
            study_year == 2 & week_number<=30 ~ 2022,
            study_year == 3 & week_number>30 ~ 2022,
            study_year == 3 & week_number<=30 ~ 2023,
            study_year == 4 & week_number>30 ~ 2023,
            study_year == 4 & week_number<=30 ~ 2024,
            TRUE ~ NA_real_
          )) %>%
        dplyr::mutate(
          study_week = (year-2020)*52+week_number-1,
          admission_date = start_date_of_week(study_week)
          # week_number = lubridate::epiweek(admission_date)
        )
      message("Admission date is derived from study week and hence approximate.")
    } else {
      warning("The loaded data has neither an admission_date column, nor both of study_year, and week_number")
      if ("enrollment_date" %in% colnames(tmp)) {
        tmp = tmp %>% mutate(admission_date = enrollment_date)
        warning("Using enrollment date as an admission date.")
      } else {
        warning("No enrollment date either.")
        warning("Was rawData loaded with avoncap::load_data(...)? If not the study_year number may be missing.")
        stop("Aborted.")
      }


    }

    if (!"hospital" %in% colnames(tmp)) {
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

    # If this a UAD control?
    tmp = tmp %>% dplyr::mutate(uad_control = dplyr::case_when(
      # in the controls database the hospital numbers are different, NC for:
      # NBT, startswith numeric for BRI
      tolower(substr(record_number,1,2)) == "nc" ~ TRUE,
      substr(record_number,1,1) %in% as.character(0:9) ~ TRUE,
      # in the main database an "A" is an early BRI patient, then the moved to "B":
      tolower(substr(record_number,1,1)) == "a" ~ FALSE,
      tolower(substr(record_number,1,1)) == "b" ~ FALSE,
      tolower(substr(record_number,1,1)) == "n" ~ FALSE,
      TRUE ~ NA
    ))

    originalColnames = colnames(tmp)
    # prefix all the original columns with a single "."
    tmp = tmp %>% dplyr::rename_with(.fn= ~ paste0(".",.x))

    # mutate field values into new values
    for(i in 1:length(mappings)) {
      mappingName = names(mappings)[[i]]

      .fn = mappings[[i]]
      # add in the "."
      tomap = paste0(".",mappingName)
      if (any(stringr::str_starts(colnames(tmp),tomap))) {
        tryCatch({
          tmp = tmp %>% .fn(tomap)
        }, error = function(e) {
          message("could not process column or column set ",mappingName," due to ",e$message)
        })
      } else {
        message("the input data set does not have a ",mappingName," column (or column set)")
      }

    }

    fileName = files

    # Identify admission count if patient identifier available
    tryCatch({
      tmp = tmp %>% dplyr::group_by(admin.patient_identifier) %>% dplyr::arrange(admission.date) %>%
        dplyr::mutate(
          admission.episode = dplyr::row_number(),
          admission.interval = as.integer(admission.date-dplyr::lag(admission.date))
        ) %>% dplyr::ungroup()
    }, error = function(e) message("could not identify duplicates by NHS number"))
    if (!"admission.episode" %in% colnames(tmp)) tmp = tmp %>% dplyr::mutate(admission.episode = NA_real_)

    mappedCols = tmp %>% dplyr::select(tidyselect::starts_with("..")) %>% colnames()
    message("Mapped ",mappedCols %>% length()," columns")

    if(remove_mapped) {
      tmp = tmp %>% dplyr::select(-tidyselect::starts_with(".."))
    } else {
      message("renamed original columns as: ",tmp %>% dplyr::select(tidyselect::starts_with("..")) %>% colnames() %>% paste0(collapse="; "))
    }

    unmappedCols = tmp %>% dplyr::select(c(tidyselect::starts_with("."), -tidyselect::starts_with(".."))) %>% colnames()
    message("Did not map ", unmappedCols %>% length()," columns")

    # detect multiple admission episodes based on nhs_number, if present




    if(remove_unmapped) {
      tmp = tmp %>% dplyr::select(!(c(tidyselect::starts_with("."), -tidyselect::starts_with(".."))))
    } else {
      message("renamed unmapped columns as: ",unmappedCols %>% paste0(collapse="; "))
    }



    tmp %>% dtrackr::track(.messages = messages) %>% dtrackr::capture_exclusions() %>%
      magrittr::set_attr("unmapped", unmappedCols) %>%
      magrittr::set_attr("mapped", mappedCols)

  }, rawData, mappings, remove_mapped, remove_unmapped, .cache = input("cache"), ...) %>%
    magrittr::set_attr("missing", missingCols)
  # return(tmp2)
}

# Allows a
.normalise_variant = function(renameTo) {
  renameTo = rlang::ensym(renameTo)
  return(function(df, valueCol) {
    valueCol = as.symbol(valueCol)
    message("mapping ",valueCol," to ",renameTo)
    df %>% dplyr::mutate(!!renameTo := dplyr::case_when(
      !!valueCol %>% stringr::str_detect("(P|p).*(R|r)") ~ "Delta",
      !!valueCol %>% stringr::str_detect("(K|k).*(N|n)") ~ "Omicron",
      is.na(!!valueCol) ~ NA_character_,
      TRUE ~ "unknown"
    ) %>% factor(levels = c("unknown","Delta","Omicron"))) %>%
      dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol)) %>%
      dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

# TODO: use labelled:: for column names

.normalise_list = function(renameTo, values, ordered = FALSE, zeroValue=FALSE, codes=(1:length(values))-zeroValue) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    valueCol = as.symbol(valueCol)
    message("mapping ",valueCol," to ",renameTo)
    df %>%
      dplyr::mutate(!!renameTo := !!valueCol %>% factor(levels=codes, labels=values, ordered = ordered)) %>%
      dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol)) %>%
      dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_yesno = function(renameTo) {
  #TODO: error checking
  .normalise_list({{renameTo}}, c("no","yes"), zeroValue=TRUE)
}

.normalise_yesno_unknown = function(renameTo) {
  #TODO: error checking
  .normalise_list({{renameTo}}, c("yes","no","unknown"))
}

.normalise_ppi = function(renameTo) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo := sapply(!!valueCol, digest::digest)) %>%
      dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol)) %>%
      dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_name = function(renameTo) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo := !!valueCol) %>%
      dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol)) %>%
      dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_double = function(renameTo, limits=c(-Inf,Inf)) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo :=  suppressWarnings(as.numeric(!!valueCol))) %>%
      dplyr::mutate(!!renameTo :=  dplyr::if_else(!!renameTo < limits[1] | !!renameTo > limits[2],NA_real_,!!renameTo)) %>%
      dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol)) %>%
      dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_text_to_factor = function(renameTo, levels) {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo := suppressWarnings(factor(tolower(!!valueCol), levels=tolower(levels), labels=levels))) %>%
      dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol)) %>%
      dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_date = function(renameTo, limits=as.Date(c("2020-01-01","2030-01-01")), tryFormats="%d/%m/%Y") {
  renameTo=rlang::ensym(renameTo)
  return(function(df, valueCol) {
    message("mapping ",valueCol," to ",renameTo)
    #TODO: error checking
    valueCol = as.symbol(valueCol)
    df %>%
      dplyr::mutate(!!renameTo :=  suppressWarnings(as.Date(!!valueCol, tryFormats = tryFormats))) %>%
      dplyr::mutate(!!renameTo :=  dplyr::if_else(!!renameTo < limits[1] | !!renameTo > limits[2],as.Date(NA),!!renameTo)) %>%
      dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol)) %>%
      dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  })
}

.normalise_checkboxes = function(renameToVars) {
  return(function(df, valueColPrefix) {
    i=1
    naCol = as.symbol(paste0(valueColPrefix,"___na"))
    hasNa = rlang::as_label(naCol) %in% colnames(df)
    for(renameTo in renameToVars) {
      # figure out the name in the data of the column
      valueCol = as.symbol(paste0(valueColPrefix,"___",i))
      message("mapping ",valueCol," to ",renameTo)
      # rename original ___1, ___2, etc to something meaningful and convert to ordered factor
      df = df %>% dplyr::mutate(!!renameTo := !!valueCol)
      if(hasNa) {
        # deal with ___na columns etc, by making the renamed checkbox variables have an NA in them for the values where NA has been checked
        df = df %>% dplyr::mutate(!!renameTo := dplyr::if_else(!!naCol == 1, NA_real_, !!renameTo))
      }
      df = df %>%
        dplyr::mutate(!!renameTo := !!renameTo %>% factor(levels=c(0,1), labels=c("no","yes"))) %>%
        dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueCol))
      # hide original ___1, ___2, etc columns
      df = df %>% dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
      #TODO: validation checking?
      i=i+1
    }
    # hide original ___na columns
    if(hasNa) df = df %>% dplyr::rename(!!(paste0(".",naCol)) := (!!naCol))

    return(df)


  })
}

.normalise_checkboxes_to_list = function(renameTo, values, ordered = FALSE, zeroValue=FALSE, codes=(1:length(values))-zeroValue) {
  renameTo=rlang::ensym(renameTo)
  # return(function(df, valueCol) {
  #   valueCol = as.symbol(valueCol)
  #   message("mapping ",valueCol," to ",renameTo)
  #   df %>%
  #     dplyr::mutate(!!renameTo := !!valueCol %>% factor(levels=codes, labels=values, ordered = ordered)) %>%
  #     dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
  # })

  return(function(df, valueColPrefix) {

    naCol = as.symbol(paste0(valueColPrefix,"___na"))
    hasNa = rlang::as_label(naCol) %in% colnames(df)

    df = df %>% dplyr::mutate(!!renameTo := methods::as(NA,class(values)))

    for(i in 1:length(values)) {
      value = values[i]
      code = codes[i]
      # figure out the name in the data of the column
      valueCol = as.symbol(paste0(valueColPrefix,"___",code))
      message("mapping ",valueCol," to ",renameTo,", value ",value)
      df = df %>% dplyr::mutate(!!renameTo := ifelse(!!valueCol == 1 & is.na(!!renameTo),value,!!renameTo))
      # no need to deal with ___na columns etc as columns start with NA. no way to tell missing from present but NA.
      # hide original ___1, ___2, etc columns
      df = df %>% dplyr::rename(!!(paste0(".",valueCol)) := (!!valueCol))
      #TODO: validation checking?
    }

    # hide original ___na columns
    if(hasNa) df = df %>% dplyr::rename(!!(paste0(".",naCol)) := (!!naCol))
    df = df %>% dplyr::mutate(!!renameTo := !!renameTo %>% magrittr::set_attr("src",valueColPrefix))
    return(df)

  })
}
