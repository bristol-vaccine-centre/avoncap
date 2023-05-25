
## File input locations ----

#' Sets the location of data for an analysis
#'
#' Also performs some structure checks and makes sure that the
#' README files are in place.
#'
#' @param path the path to the input directory
#'
#' @return the full path to the directory
#' @export
set_input = function(path) {
  path = fs::path(fs::path_expand(path))
  if (fs::dir_exists(path)) message("Found data directory: ",path)
  else {
    fs::dir_create(path)
    message("Creating data directory: ",path)
  }
  options("avoncap.input"=path)
  .check_structure(path)
  options("cache.download"=input("download"))
  return(input())
}

#' Locate the input directory
#'
#' @param ... the sub path(s) within the input directory
#'
#' @return a path to the input directory and sub path(s) if provided
#' @export
#'
#' @example inst/examples/data-location-examples.R
input = function(...) {
  dir = getOption("avoncap.input")
  if (is.null(dir)) stop("getOption('avoncap.input') is undefined.\n",
      "please call `set_input()` with the path to the data directory")
  dir = dir  %>% fs::path_expand()
  if (!fs::path(dir,...) %>% fs::path_dir() %>% fs::dir_exists()) {
    fs::path(dir,...) %>% fs::path_dir() %>% fs::dir_create()
  }
  return(fs::path(dir,...))
}

# Read in secrets file from input directory.
.secrets = function(path = input("avoncap.yaml")) {
  out = yaml::read_yaml(path)
  return(out)
}

# make sure the various subdirectories exist adn
.check_structure = function(path) {
  if (!fs::dir_exists(path)) stop ("The input directory must exist: ",path)
  is_empty = length(fs::dir_ls(path)) == 0

  required = list(
    "nhs-extract" = c(
      "# Location for extracts from NHS redcap instance",
      "",
      "These files are sent from Charli, usually as a password protected zip file or excel file",
      "The files should be unzipped or exported as csv from excel.",
      "Files must be in a dated subdirectory (`yyyy-mm-dd`) and named `NBT_y1.csv`, `BRI_y1.csv`, `NBT_y2.csv`, etc... "
    ),
    "ethnicity" = c(
      "# Location for ethnicity reports from NHS redcap instance",
      "",
      "These files are sent from Charli, usually as a password protected zip file or excel file",
      "The files should be unzipped or exported as csv from excel.",
      "Files must be in a dated subdirectory (`yyyy-mm-dd`) and named `y1.csv`, `y2.csv`, etc... "
    ),
    "avoncap-export" = c(
      "# Location for exports from UoB avoncap instance",
      "",
      "These files are downloaded from here:",
      "My Projects > .. select project .. > Data Exports, Reports, and Stats > All data > Export Data",
      "Files must be in a dated subdirectory (`yyyy-mm-dd`) and named `central_y1.csv`, `central_y2.csv`, etc... ",
      "or `uad-controls.csv`."
    ),
    "metadata" = c(
      "# Metadata files from UoB avoncap instance",
      "",
      "These files are the database metadata files from the UoB redcap server.",
      "They can be found on the UoB server at:",
      "",
      "`My Projects` > `.. select project ..` > `Dictionary` > `Download the current Data Dictionary`",
      "Files must be in a dated subdirectory (`yyyy-mm-dd`) and named `central_y1.csv`, `central_y2.csv`, `uad-control.csv`, etc... "
    )
  )
  additional = list(
    "downloads" = c(
      "# Cache for downloaded files",
      "",
      "Automatically managed. It is safe to delete these files but they may then have to be regenerated."
    ),
    "urine-antigens" = c(
      "# Urine antigen results from Pfizer",
      "",
      "Files must be in a dated subdirectory (`yyyy-mm-dd`) and named `uad-cases.csv`")
  )

  tmp = c(required,additional)
  if (is_empty) {
    message("Creating empty directory structure.")
    lapply(names(tmp), function(x) fs::dir_create(fs::path(path,x)))
  }
  directories = fs::path_rel(fs::dir_ls(path,type = "directory"), path)
  if (!all(names(required) %in% directories)) {
    missing = setdiff(names(required),directories)
    stop("missing required directories in data input: \n",paste0(fs::path(path,missing), collapse="\n"))
  }
  if (!all(names(additional) %in% directories)) {
    missing = setdiff(names(additional),directories)
    lapply(missing, function(x) fs::dir_create(fs::path(path,x)))
  }
  # ensure metadata available:
  try({fs::dir_copy(
    system.file("metadata", package="avoncap"),
    fs::path(path),
    overwrite = FALSE
  )},silent = TRUE)

  lapply(names(tmp), function(x) readr::write_lines(x = tmp[[x]],file = fs::path(path, x,"README.md")))
  invisible(NULL)
}


## File handling functions ----

#' Scans the input directory and returns csv or xlsx files in that directory
#'
#' Extracting metadata from the filename where present - particularly hospital, and year number
#'
#' @return a dataframe containing filename, path, date, hospital, and study_year fields
#' @export
all_files = function() {
  files = fs::dir_ls(input(),recurse = TRUE) %>% stringr::str_subset(".+\\.(csv)")
  info = fs::file_info(files) %>% dplyr::mutate(
    filename = path %>% fs::path_ext_remove() %>% fs::path_file(),
    directory = path %>% fs::path_rel(input()) %>% fs::path_split() %>% purrr::map(~ .[[1]]) %>% unlist(),
    filename_date = path %>% fs::path_rel(input()) %>% fs::path_split() %>% purrr::map(~ .[[2]]) %>% unlist()
  )
  info %>%
    dplyr::mutate(
      # modification_date = as.Date(modification_time),
      filename_date = filename_date %>% as.Date(format="%Y-%m-%d",optional=TRUE),
      filetype = fs::path_ext(path),
      hospital = dplyr::case_when(
        stringr::str_detect(path,"NBT") ~ "NBT",
        stringr::str_detect(path,"BRI") ~ "BRI",
        TRUE ~ NA_character_
      ),
      study_year = (path %>% fs::path_file() %>% fs::path_ext_remove() %>% stringr::str_match("_y([1-9])"))[,2],
      study_year = as.integer(study_year)
    ) %>%
    dplyr::filter(!is.na(filename_date)) %>%
    # dplyr::mutate(date = dplyr::if_else(is.na(filename_date),modification_date,filename_date))
    dplyr::rename(date = filename_date) %>%
    dplyr::select(filename, directory, path, date, hospital, study_year, filetype)
}


#' find most recent files of a specific type
#'
#' @param type see valid_inputs() for current list of supported types in input directory
#' @param subtype see valid_inputs() for list of supported filenames
#' @param reproduce_at after this date new files are ignored. This enforces a specific version of the data.
#'
#' @return a list of the file paths to the most up to date files of the given type relevant to each site and study year
#' @export
#'
#' @example inst/examples/data-location-examples.R
most_recent_files = function(type = "", subtype = NULL, reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date()))) {
  file = subtype
  tmp = all_files() %>%
    dplyr::filter(tolower(directory) == tolower(type) & date <= reproduce_at)
  if (!is.null(file)) {
    tmp = tmp %>% dplyr::filter(stringr::str_starts(tolower(filename),tolower(file)))
  }
  tmp = tmp %>%
    dplyr::group_by(filename,directory,hospital,study_year) %>%
    # given a particular hospital or study year combo find he most recent file
    dplyr::filter(date == suppressWarnings(max(date))) %>%
    dplyr::ungroup()
  return(tmp)
}


#' A valid set of types of file that can be loaded by `load_data(...)`
#'
#' @return a dataframe of type, subtype
#' @export
#'
#' @example inst/examples/data-location-examples.R
valid_inputs = function() {
  t = all_files()
  t %>% dplyr::mutate(
    type = tolower(directory),
    file = filename,
    subtype = filename %>% stringr::str_split("_") %>% lapply(function(x) setdiff(x, c("NBT","BRI","y1","y2","y3","y4","y5"))) %>% lapply(paste0, collapse="_") %>% unlist()
  ) %>%
  dplyr::select(type,subtype,file) %>%
  dplyr::group_by(type) %>%
  dplyr::mutate(subtype = ifelse(subtype=="" | dplyr::n()==1, NA_character_, subtype)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(type)
}


# Parsing and combining dataframes ----

# detecting errors when merging dataframes
# extract parse issues
.detect_parse_issues = function(listOfDf) {
  suppressWarnings(purrr::map(listOfDf, ~ readr::problems(.x)))
}

# parse the list of dataframes for columns and emptiness
# (list(iris,mtcars)) %>% .detect_structure()
.detect_structure =  function(listOfDf) {
  purrr::map(listOfDf, ~ dplyr::inner_join(
    .x %>% lapply(class) %>% unlist() %>% tibble::enframe() %>%
      dplyr::rename(type = value) %>%
      dplyr::mutate(convert = dplyr::case_when(
        # bind_rows can automatically convert some data types
        type == "logical" ~ "numeric",
        type == "integer" ~ "numeric",
        TRUE ~ type
      )),
    .x %>% lapply(function(c) all(is.na(c))) %>% unlist() %>% tibble::enframe() %>% dplyr::rename(empty = value),
    by = "name"
  ))
}

# figure out which columns are empty
# (list(iris,mtcars %>% dplyr::mutate(carb=NA))) %>% .detect_structure() %>% .detect_empty()
.detect_empty = function(structure) {
  structure %>% purrr::map(~ .x %>% dplyr::filter(empty) %>% dplyr::pull(name))
}

# figure out which columns have different types in the data
# (list(iris %>% dplyr::mutate(mpg="wrong type"),mtcars %>% dplyr::mutate(carb=NA))) %>% .detect_structure() %>% .detect_mismatch()
.detect_mismatch = function(structure) {
  data = tibble::tibble(index = 1:length(structure), structure = structure) %>% tidyr::unnest(structure)
  # majority = data %>% dplyr::filter(!empty) %>% dplyr::group_by(name,type) %>% dplyr::count() %>% dplyr::group_by(name) %>% dplyr::filter(n == dplyr::n())
  minority = data %>%
    # ignore empty columns as they are generally incorrectly typed as a logical(NA)
    dplyr::filter(!empty) %>%
    dplyr::group_by(name,convert) %>% dplyr::mutate(n=dplyr::n()) %>%
    dplyr::group_by(name) %>% dplyr::mutate(N = dplyr::n()) %>%
    # Check that there is only one type for each of the column names.
    dplyr::filter(n!=N)
  structure %>% purrr::map(~ .x %>% dplyr::filter(!empty) %>% dplyr::semi_join(minority, by=c("name","convert")) %>% dplyr::pull(name))
}


#' Load data and check structure
#'
#' Loads the AvonCap data from a set of csv files, which may optionally be
#' qualified by site `('BRI' or 'NBT')` and database year `('y1', 'y2', 'y3')`
#' as part of the file name. This selects the most recent files earlier than the
#' `reproduce_at` date and detects whether they are in a set of files.
#'
#' The files are loaded as csv as checked that files have (A) the same columns,
#' (B) the same type (or are empty) (C) have any major parse issues. It then
#' merges the files into a single dataframe, if possible, otherwise it will
#' return the individually loaded files as a list of dataframes.
#'
#' @param type the file category see valid_inputs() for current list in input
#'   directory
#' @param file (optional in some circumstances) the file name component see
#'   valid_inputs() for current list in input directory
#' @param reproduce_at - the date at which to cut off newer data files
#' @param merge - setting to `TRUE` forces multiple files be merged into a
#'   single data frame by losing mismatching columns.
#' @param ... - passed to `cached` may specifically want to use `nocache=TRUE``
#'
#' @return either a list of dataframes or a single merged dataframe
#' @export
#'
#' @examples
#' load_data("nhs-extract","deltave")
load_data = function(type, subtype=NULL, reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date())), merge = NA, ...) {
  if(reproduce_at != Sys.Date()) warning("REPRODUCING RESULTS FROM: ",reproduce_at, ", to disable this set options(reproduce.at=NULL)")
  tmp = type
  tmp_files = most_recent_files(type, subtype, reproduce_at)
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

    if (is.na(merge)) {
      if(length(col_suppress) > 0) {
        message("INCONSISTENT COLUMN(S) IN FILES: ",paste0(col_suppress,collapse=";"))
        message("NOT MERGING FILES")
        merge = FALSE
      } else {
        merge = TRUE
      }
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
      dplyr::mutate(csv = purrr::map2(csv, empty, ~ .x %>% dplyr::select(-tidyselect::any_of(.y)))) %>%
      # convert conflicting data type columns to character
      dplyr::mutate(csv = purrr::map2(csv, mismatches, ~ .x %>% dplyr::mutate(dplyr::across(tidyselect::any_of(.y), as.character)))) %>%
      # force merge the files together
      dplyr::select(.hospital=hospital, .study_year=study_year, .file = file, csv) %>%
      tidyr::unnest(csv)

    if (!("hospital" %in% colnames(tmp))) tmp = tmp %>% dplyr::mutate(hospital = .hospital)
    if (!("study_year" %in% colnames(tmp))) tmp = tmp %>% dplyr::mutate(study_year = .study_year)
    if (!("file" %in% colnames(tmp))) tmp = tmp %>% dplyr::mutate(file = .file)
    tmp = tmp %>% dplyr::select(-tidyselect::any_of(c(".hospital",".study_year",".file")))

    message("Loaded ",nrow(tmp)," rows from ",nrow(data2)," files, (", paste0(data2$entries,collapse="+"),"=",total,")")
    if (nrow(tmp) != total) stop("The row numbers of the merged files", nrow(tmp)," do not add up to expected, ",total)

    attr(tmp,"parse_issues") = dplyr::bind_rows(data2$parse_issues)
    attr(tmp,"file") = paste0(fs::path_file(files), collapse="; ")
    attr(tmp,"paths") = files
    attr(tmp,"date") = unique(tmp_files$date)
    tmp
  }, merge, tmp_files, ..., .prefix="data")
  return(
    structure(
      tmp2,
      type=type,
      subtype=subtype
    )
  )
}

#' Write file source information out to a text files
#'
#' @param ... A list of data frames loaded with the `load_data(...)` call
#' @param .file the output file location
#'
#' @return the file name written (invisibly)
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


