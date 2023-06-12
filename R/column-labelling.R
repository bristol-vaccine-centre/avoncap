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
    admission.category = "Aetiology",
    comorbid.cva_or_tia = "CVA/TIA"
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
  #columnVar = tryCatch({rlang::ensym(columnVar)}, error = function(e) columnVar)
  if (length(columnVar) > 1) return(lapply(columnVar, readable_label, colNames = colNames))
  if (rlang::is_symbol(columnVar)) columnVar = rlang::as_label(columnVar)
  tmp = columnVar
  if (!is.null(colNames[[tmp]])) {
    return(colNames[[tmp]])
  }
  tmp = tmp %>% stringr::str_remove("[a-zA-Z0-9_]+\\.") %>% stringr::str_replace_all("_"," ")
  return(tmp %>% stringr::str_to_title() %>% .fix_acronyms() %>% .fix_symbols())
}

.fix_acronyms = function(strings, acronyms = c(
  "CCI","CKD","ICU","BMI","NEWS2","COVID","LRTI","aLRTI","IMD","CURB65","MI",
  "FiO2","PEEP","BP","HIV","AIDS","AF","PE","STEMI","NSTEMI","LOS","WHO",
  "IHD","CCF","TIA","CVA","DVT","CXR","COPD","ARF","ACS","VTE","eGFR","pH",
  "CRP","CURB","CRB")
) {
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

# Extract column metadata ----

#' Get the mapping of transformed columns back to original
#'
#' @param data the transformed data set.
#' @param inverse give the data as a old -> new mapping for finding
#' normalised names of original columns. if false gives it as new->old for finding
#' original names of normalised columns
#'
#' @return a named list mapping original to new columns
#' @export
original_field_names = function(data, inverse = TRUE) {
  tmp = sapply(colnames(data), function(x) attr(data[[x]],"src"))
  tmp = tmp[!sapply(tmp,is.null)]
  new = names(tmp)
  original = substr(tmp,2,1000)
  if (inverse) {
    return(new %>% magrittr::set_names(original))
  } else {
    return(original %>% magrittr::set_names(new))
  }
  return(tmp2)
}

#' Extract the
#'
#' @param data the dataframe
#' @param col the column as a symbol
#' @param original map the names to the original column names from the data. If
#' this is false the function returns a list of current normalised column names.
#'
#' @return a named list of dependencies and original column names for a given
#' column
#' @export
extract_dependencies = function(data, col, original = TRUE) {
  tmp = .extract_dependencies(data,{{col}},3,3)
  if (original) {
    return(original_field_names(data, inverse=FALSE)[tmp])
  } else {
    return(tmp)
  }
}

.extract_dependencies = function(data, col, d, max_d) {
  if (d==0) return(character())
  col = ensym(col)
  val = data %>% pull(!!col)
  tmp = attr(val,"depends")
  tmp = unique(c(tmp,sapply(tmp, function(x) {
    s = as.symbol(x)
    .extract_dependencies(data,!!s,d-1, d)
  })))
  # if(d < max_d)
  return(unlist(tmp))
}
