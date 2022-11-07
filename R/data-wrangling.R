
# detecting errors when merging dataframes ----

# extract parse issues
.detect_parse_issues = function(listOfDf) {
  suppressWarnings(purrr::map(listOfDf, ~ readr::problems(.x)))
}

# parse the list of dataframes for columns and emptiness
# (list(iris,mtcars)) %>% .detect_structure()
.detect_structure =  function(listOfDf) {
  purrr::map(listOfDf, ~ dplyr::inner_join(
    .x %>% lapply(class) %>% unlist() %>% tibble::enframe() %>% dplyr::rename(type = value),
    .x %>% lapply(function(c) all(is.na(c))) %>% unlist() %>% tibble::enframe() %>% dplyr::rename(empty = value),
    by = "name"
  ))
}

# figure out which columns are empty
# (list(iris,mtcars %>% dplyr::mutate(carb=NA))) %>% .detect_structure() %>% .detect_empty()
.detect_empty = function(structure) {
  structure %>% purrr::map(~ .x %>% dplyr::filter(empty) %>% dplyr::pull(name))
}

# (list(iris %>% dplyr::mutate(mpg="wrong type"),mtcars %>% dplyr::mutate(carb=NA))) %>% .detect_structure() %>% .detect_mismatch()
.detect_mismatch = function(structure) {
  data = tibble::tibble(index = 1:length(structure), structure = structure) %>% tidyr::unnest(structure)
  # majority = data %>% dplyr::filter(!empty) %>% dplyr::group_by(name,type) %>% dplyr::count() %>% dplyr::group_by(name) %>% dplyr::filter(n == dplyr::n())
  minority = data %>%
    # ignore empty columns as they are generally incorrectly typed as a logical(NA)
    dplyr::filter(!empty) %>%
    dplyr::group_by(name,type) %>% dplyr::mutate(n=dplyr::n()) %>%
    dplyr::group_by(name) %>% dplyr::mutate(N = dplyr::n()) %>%
    # Check that there is only one type for each of the column names.
    dplyr::filter(n!=N)
  structure %>% purrr::map(~ .x %>% dplyr::filter(!empty) %>% dplyr::semi_join(minority, by=c("name","type")) %>% dplyr::pull(name))
}


#' Load data and check structure
#'
#' Loads the AvonCap data from a set of csv files, which may optionally be qualified by site `('BRI' or 'NBT')` and
#' database year `('20-21','21-22','22-23', or 'y1', 'y2', 'y3')` as part of the file name. This selects the most
#' recent files earlier than the `reproduce_at` date and detects whether they are in a set of files.
#'
#' The files are loaded as csv as checked that files have (A) the same columns, (B) the same type (or are empty) (C)
#' have any major parse issues. It then merges the files into a single dataframe, if possible, otherwise it will
#' return the individually loaded files as a list of dataframes.
#'
#' @param type see valid_inputs() for current list in input directory
#' @param file see valid_inputs() for current list in input directory
#' @param reproduce_at - the date at which to cut off newer data files
#' @param merge - setting to `TRUE` forces multiple files be merged into a single data frame by losing mismatching columns.
#' @param ... - passed to `cached` may specifically want to use `nocache=TRUE``
#'
#' @return either a list of dataframes or a single merged dataframe
#' @export
#'
#' @examples
#' load_data("nhs-extract")
load_data = function(type, file=NULL, reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date())), merge = NA, ...) {
  if(reproduce_at != Sys.Date()) warning("REPRODUCING RESULTS FROM: ",reproduce_at, ", to disable this set options(reproduce.at=NULL)")
  tmp = type
  tmp_files = most_recent_files(type, file, reproduce_at)
  if(nrow(tmp_files) == 0) {
    cat("No suitable files matching \"",type,"\", before date: ",format(reproduce_at), ". Try one of: \n", sep="")
    avoncap::valid_inputs()
    cat("as an input to 'type'\n")
    stop("aborting.")
  }
  tmp2 = .cached({
    files = tmp_files %>% dplyr::pull(path)
    if (length(files)>1 & length(unique(tmp_files$date)) > 1) {
      message("The most recent versions of the inputs have different effective dates: ",type," i.e. ", paste0(files,collapse = ";"))
      message("This could mean the files are out of sync.")
    }

    # TODO: This is super slow because it checks all the data types of everything
    # on the larger dataset it throws parse errors because it cannot guess the correct type
    # There is an argument for reading it all in as text then analysing and checking the type when normalising.
    # This would need a rethink on the way in which structural issues are dealt with.

    data = tmp_files %>%
      dplyr::mutate(
        csv = purrr::map(path, ~ suppressWarnings(readr::read_csv(.x, na = c("","NA","Na","na","N/A","UNK"), show_col_types = FALSE))),
        entries = purrr::map_dbl(csv, ~ nrow(.))
      )
    data2 = data %>%
      dplyr::mutate(file = fs::path_file(path)) %>%
      dplyr::mutate(parse_issues = .detect_parse_issues(csv)) %>%
      # get rid of all parsing metadata
      dplyr::mutate(csv = purrr::map(csv, ~ tibble::as_tibble(.x))) %>%
      dplyr::mutate(structure = .detect_structure(csv)) %>%
      dplyr::mutate(empty = .detect_empty(structure)) %>%
      dplyr::mutate(mismatches = .detect_mismatch(structure)) %>%
      dplyr::select(-path,-filename)
    col_suppress = unique(c(unlist(data2$mismatches)))

    if(length(col_suppress) > 0) {
      message("INCONSISTENT COLUMN(S) IN FILES: ",paste0(col_suppress,collapse=";"))
      if (is.na(merge)) {
        message("NOT MERGING FILES")
        merge = FALSE
      }
    } else {
      merge = TRUE
    }

    if (nrow(dplyr::bind_rows(data2$parse_issues))>0) {
      message(nrow(dplyr::bind_rows(data2$parse_issues))," parse issues in raw files. Check the parse_issues attrbute.")
    }

    if (!merge) {
      return(data2 %>% dplyr::select(hospital,study_year,file,csv, entries, mismatches, empty, structure, parse_issues))
    }

    total = sum(data2$entries)

    tmp = data2 %>%
      # Lose empty columns which will have been assigned incorrect type
      dplyr::mutate(csv = purrr::map2(csv, empty, ~ .x %>% dplyr::select(-any_of(.y)))) %>%
      # Lose conflicting data type columns
      dplyr::mutate(csv = purrr::map2(csv, mismatches, ~ .x %>% dplyr::select(-any_of(.y)))) %>%
      # force merge the files together
      dplyr::select(hospital,study_year,file,csv) %>%
      tidyr::unnest(csv)

    message("Loaded ",nrow(tmp)," rows from ",nrow(data2)," files, (", paste0(data2$entries,collapse="+"),"=",total,")")
    if (nrow(tmp) != total) stop("The row numbers of the merged files", nrow(tmp)," do not add up to expected, ",total)

    attr(tmp,"parse_issues") = dplyr::bind_rows(data2$parse_issues)
    attr(tmp,"file") = paste0(fs::path_file(files), collapse="; ")
    attr(tmp,"paths") = files
    attr(tmp,"date") = unique(tmp_files$date)
    tmp
  }, merge, tmp_files, .cache = input("cache"), ...)
  return(tmp2)
}

#' Write file source information out to a text files
#'
#' @param ... A list of data frames loaded with the `load_data(...)` call
#' @param .file the output file location
#'
#' @return the filename written to invisibly
#' @export
save_data_source_info = function(..., .file) {
  dfs = rlang::list2(...)
  files = sapply(dfs, function(x) attr(x,"paths"))
  md5s= sapply(unlist(files), digest::digest, file=TRUE, USE.NAMES = FALSE)
  df=tibble::tibble(
    path = unlist(files),
    md5 = md5s)
  readr::write_csv(x = df, file = .file)
  return(df)
}




# lrtd_normalise = function(lrtd_data = load_data("AvonCAPLRTDCentralDa")) {
#   ethn = readr::read_csv(input("Ethnicity Data.csv"))
#   # With the data from bristol the year and
#   lrtd_data2 = lrtd_data %>% dplyr::left_join(ethn, by="record_number") %>%
#     # the years are sometimes missing when
#     dplyr::mutate(year = dplyr::case_when(
#       !is.na(year) ~ year,
#       year == "y1" & week_number>30 ~ 2020,
#       year == "y1" & week_number<=30 ~ 2021,
#       year == "y2" & week_number>30 ~ 2021,
#       year == "y2" & week_number<=30 ~ 2022,
#       year == "y3" & week_number>30 ~ 2022,
#       year == "y3" & week_number<=30 ~ 2023,
#       TRUE ~ NA_real_
#     )) %>%
#     dplyr::mutate(study_week = (year-2020)*52+week_number)
#   # These variables get picked up by the additional mappings in lrtd_mappings:
#   lrtd_norm = lrtd_data2 %>% normalise_data()
#
#   v = lrtd_norm %>% get_value_sets()
#
#   lrtd_norm = lrtd_norm %>% dplyr::mutate(
#     # this is a study specific variable.
#     admission.study_week_start_date = start_date_of_week(admission.study_week)
#   )
#
#   return(lrtd_norm)
# }

## Column naming ----

#' default column naming mappings
#'
#' @param ... additional named items to add
#' @return a set of mappings
#' @export
default_column_names = function(...) {
  list(
    ...,
    demog.imd_decile = "IMD (decile)",
    genomic.variant_inferred = "Variant",
    admission.curb_65_severity_score = "CURB65 Score",
    demog.age = "Age",
    admission.news2_score = "NEWS2 Score",
    qcovid2.hazard_ratio = "QCovid2 HR",
    qcovid2.log_hazard = "QCovid2 log hazard",
    admission.charlson_comorbidity_index = "CCI",
    admission.cci_category = "CCI Category",
    admission.covid_pcr_result = "PCR Result",
    day_7.WHO_clinical_progression = "WHO Outcome Score",
    day_7.max_o2_level = "Max FiO2",
    day_7.length_of_stay = "Length of Stay",
    demog.pcr_positive_by_age = "PCR Positives (by age)",
    demog.age_eligible = "Age Eligible for PneumoVax",
    admission.presentation_3_class = "aLTRD presentation",
    admission.category = "Aetiology"
  )
}

#' Get a label for a column
#'
#' @param columnVar the column name as a string
#' @param colNames bespoke column names mapping (see `default_column_names(...)`)
#'
#' @return a mapped column name
#' @export
readable_label = function(columnVar, colNames = default_column_names()) {
  columnVar = rlang::ensym(columnVar)
  if (length(columnVar) > 1) return(lapply(columnVar, readable_label, colNames = colNames))
  if (rlang::is_symbol(columnVar)) columnVar = rlang::as_label(columnVar)
  tmp = columnVar
  if (!is.null(colNames[[tmp]])) {
    return(colNames[[tmp]])
  }
  tmp = tmp %>% stringr::str_remove("[a-zA-Z0-9_]+\\.") %>% stringr::str_replace_all("_"," ")
  return(tmp %>% stringr::str_to_title() %>% .fix_acronyms() %>% .fix_symbols())
}

.fix_acronyms = function(strings, acronyms = c("CCI","CKD","ICU","BMI","NEWS2","COVID","LRTI","aLRTI","IMD","CURB65","MI","FiO2","PEEP","BP","HIV","AIDS","AF","PE","STEMI","NSTEMI","LOS","WHO","IHD","CCF","TIA","CVA","DVT","CXR","COPD","ARF","ACS","VTE")) {
  out = strings
  for(acronym in acronyms) {
    r = sprintf("\\b%s\\b",acronym)
    out = out %>% stringr::str_replace_all(stringr::regex(r,ignore_case=TRUE),acronym)
  }
  return(out)
}

.fix_symbols = function(strings, symbols = c(" gt "=">"," gte "="\u2265"," lt "="<"," lte "="\u2264")) {
  out = strings
  for(symbol in names(symbols)) {
    out = out %>% stringr::str_replace_all(stringr::fixed(symbol,ignore_case=TRUE),symbols[[symbol]])
  }
  return(out)
}

#' Get a readable label for the AvonCap data as a named list (for ggplot)
#'
#' @param x either the column names as strings, or a dataframe
#' @param colNames a specific mapping of names (as symbols) to Strings to override the default heuristics
#' @param ... ignored
#'
#' @return a named list of the labels for the columns
#' @export
readable_label_mapping = function(x,...) {
  UseMethod("readable_label_mapping",x)
}

#' @describeIn readable_label_mapping for data frames
#' @param colNames a mapping to convert a column name (as a string) to a readable label
#' @export
readable_label_mapping.data.frame = function(x, colNames = default_column_names(...), ...) {
  columnVars = colnames(x)
  readable_label_mapping.default(columnVars, colNames = colNames, ...)
}

#' @describeIn readable_label_mapping for lists
#' @export
readable_label_mapping.list = function(x, colNames = default_column_names(...), ...) {
  columnVars = x
  tmp = columnVars %>% sapply(readable_label, colNames=colNames)
  names(tmp) = unname(columnVars) %>% sapply(rlang::as_label)
  return(tmp)
}

#' @describeIn readable_label_mapping for character vectors
#' @export
readable_label_mapping.character = function(x, colNames = default_column_names(...), ...) {
  columnVars = as.list(x)
  tmp = columnVars %>% sapply(readable_label, colNames=colNames)
  names(tmp) = unname(columnVars) %>% sapply(rlang::as_label)
  return(tmp)
}

#' @describeIn readable_label_mapping defaults
#' @export
readable_label_mapping.default = function(x, colNames = default_column_names(...), ...) {
  columnVars = as.list(as.character(x))
  tmp = columnVars %>% sapply(readable_label, colNames=colNames)
  if(is.list(columnVars)) {
    names(tmp) = unname(columnVars) %>% sapply(rlang::as_label)
  } else {
    names(tmp) = unname(columnVars)
  }
  return(tmp)
}



#' Get the mapping of transformed columns back to original
#'
#' @param data the transformed data set.
#'
#' @return a named list mapping original to new columns
#' @export
original_field_names = function(data) {
  tmp = sapply(colnames(data), function(x) attr(data[[x]],"src"))
  tmp2 = names(tmp)
  names(tmp2) = substr(tmp,2,1000)
  return(tmp2)
}

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




