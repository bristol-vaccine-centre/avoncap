
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
  options("avoncap.download.dir"=input("download"))
  options("avoncap.cache.dir"=input("cache"))
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
    "cache" = c(
      "# Cache for temporary files",
      "",
      "Automatically managed. It is safe to delete these files but they may then have to be regenerated."
    ),
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
      modification_date = as.Date(modification_time),
      filename_date = filename_date %>% as.Date(format="%Y-%m-%d",optional=TRUE),
      filetype = fs::path_ext(path),
      hospital = dplyr::case_when(
        stringr::str_detect(path,"NBT") ~ "NBT",
        stringr::str_detect(path,"BRI") ~ "BRI",
        TRUE ~ NA_character_
      ),
      study_year = (path %>% fs::path_file() %>% fs::path_ext_remove() %>% stringr::str_match("_y([1-9])"))[,2],
      date = dplyr::if_else(is.na(filename_date),modification_date,filename_date),
      study_year = as.integer(study_year)
    ) %>%
    dplyr::select(filename, directory, path, date, hospital, study_year, filetype)
}


#' find most recent files of a specific type
#'
#' @param type see valid_inputs() for current list of supported types in input directory
#' @param file see valid_inputs() for list of supported filenames
#' @param reproduce_at after this date new files are ignored. This enforces a specific version of the data.
#'
#' @return a list of the file paths to the most up to date files of the given type relevant to each site and study year
#' @export
#'
#' @example inst/examples/data-location-examples.R
most_recent_files = function(type = "", file = NULL, reproduce_at = as.Date(getOption("reproduce.at",default = Sys.Date()))) {
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
  # browser()
  # tmp = tmp %>%
  #   dplyr::arrange(filetype) %>%
  #   dplyr::filter(dplyr::row_number() == suppressWarnings(max(dplyr::row_number()))) %>%
  #   # this will give us versioned and unversioned
  #   # is this file a single file on a particular date with no version, or a versioned set of files (originally this was trust specific but also happens when it is year specific?
  #   dplyr::mutate(
  #     byTrust = !is.na(hospital),
  #     byYear = !is.na(study_year)
  #   ) %>%
  #   # get the most recent file or set of files if it is versioned.
  #   dplyr::group_by(filename,directory,byTrust,byYear) %>%
  #   dplyr::mutate(latest_date = suppressWarnings(max(date))) %>% dplyr::ungroup() %>%
  #   dplyr::filter(latest_date == suppressWarnings(max(latest_date))) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(-byTrust,-byYear)
  return(tmp)
}


#' A valid set of types of file that can be loaded by `load_data(...)`
#'
#' @return nothing.
#' @export
#'
#' @example inst/examples/data-location-examples.R
valid_inputs = function() {
  t = all_files()
  t %>% dplyr::mutate(type = tolower(directory), file = tolower(filename)) %>%
    dplyr::select(type,file) %>%
    dplyr::distinct() %>%
    dplyr::arrange(type)
}

