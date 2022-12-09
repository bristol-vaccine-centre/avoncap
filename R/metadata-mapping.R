# devtools::load_all()

metadata_mapping = function(db = "uad-controls") {
  # set_input("~/Data/avoncap")
  metadata = suppressMessages(load_data("metadata",db, nocache=TRUE))
  meta = metadata %>% dplyr::select(
    variable = `Variable / Field Name`,
    form = `Form Name`,
    type = `Field Type`,
    subtype = `Text Validation Type OR Show Slider Number`,
    label = `Field Label`,
    levels = `Choices, Calculations, OR Slider Labels`,
    required = `Required Field?`
  )
  meta = meta %>% dplyr::mutate(
    label = label %>% stringr::str_replace_all("<[^>]+>"," ") %>% stringr::str_replace_all("\\s+"," ") %>% trimws(),
    levels = levels %>% stringr::str_replace_all("<[^>]+>"," ") %>% stringr::str_replace_all("\\s+"," ") %>% trimws(),
    type = ifelse(!is.na(subtype),subtype,type)
  ) %>% dplyr::mutate(
    levels= dplyr::if_else(type %in% c("radio","dropdown","checkbox"), levels, NA_character_)
  ) %>% dplyr::select(-subtype)
  meta = meta %>% dplyr::mutate(
    required = !is.na(required) & required == "y",

    level_df = levels %>% stringr::str_split(stringr::fixed("|")) %>%
      purrr::map(~ tibble::tibble(opt = .x) %>% dplyr::mutate(
        key = stringr::str_extract(opt,"^\\s*[0-9]+") %>% trimws() %>% as.numeric(),
        value = stringr::str_remove(opt,"^\\s*[0-9]+,\\s*") %>% trimws(),
      ) %>% dplyr::select(-opt)),
    level_dp = level_df %>% purrr::map(~ .x %>% dplyr::pull(value) %>% deparse() %>% paste0(collapse = "")),
    level_code = level_df %>% purrr::map(~ .x %>% dplyr::pull(key) %>% deparse() %>% paste0(collapse = "")),
    level_size = level_df %>% purrr::map_dbl(~ .x %>% nrow())
  )
  return(meta)
}

.forms_to_category = list(
  screening = "admin",
  clinical_features=  "admission",
  consent_form = "admin",
  d1_samples = "admission",
  covid_risk_questionnaire = "risk",
  comorbidities = "comorbid",
  h_case_control = "day_7",
  blood_results = "haem",
  microbiology_results = "micro",
  virology_results = "virol",
  radiology_results = "radiol",
  readmission = "outcome",
  outcome_data = "outcome",
  pneumonia_severity_index = "admission",
  comments = "admin",
  consent_management = "admin",
  adhoc_samples = "misc",
  planning_follow_up = "misc",
  follow_up = "misc",
  aes = "outcome",
  yellow_card_page = "outcome",
  admin_page = "admin",
  yr_survival = "outcome",
  study_and_sample_details = "admin",
  cvs_events = "outcome",
  withdrawal = "admin"
)


generate_column_name_review_list = function() {

  files = most_recent_files("metadata")$filename

  combined = dplyr::bind_rows(
    lapply(files, function(x) metadata_mapping(x) %>% dplyr::mutate(source=x))
  )

  duplicated = combined %>% dplyr::select(variable, levels) %>%
    dplyr::mutate(levels = levels %>% stringr::str_remove_all("[^a-zA-Z0-9]") %>% tolower()) %>%
    dplyr::filter(!stringr::str_detect(variable,"staff|nurse")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(variable) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::select(variable) %>%
    dplyr::distinct()

  duplicated = combined %>% dplyr::semi_join(duplicated, by="variable")
  # TODO:

  # non chekbox mappings
  implied = combined %>% dplyr::filter(type != "checkbox" & type !="descriptive") %>%
    dplyr::select(-levels, -level_df) %>% dplyr::mutate(
      prefix = .forms_to_category[form],
      suffix = label %>%
        stringr::str_remove_all("\\s+\\([^\\)]+\\)") %>%
        stringr::str_remove_all("\\s+\\[[^\\]]+\\]") %>%
        tolower() %>% stringr::str_replace_all("[^a-z]+"," ") %>%
        trimws() %>% stringr::str_replace_all("\\s+","_"),
      candidate = paste0(prefix,".",suffix)
    ) %>%
    dplyr::select(-prefix,-suffix) %>%
    dplyr::group_by(across(c(tidyselect::everything(),-source,-level_dp,-level_code,-level_size, -label))) %>%
    dplyr::summarise(
      source = paste0(sort(source),collapse = "|"),
      level_dp = dplyr::last(level_dp, level_size),
      level_code = dplyr::last(level_code, level_size),
      label = dplyr::first(label),
      .groups = "drop")

  code = readr::read_file(system.file("R/zzz-avoncap-mappings.R", package = "avoncap"))
  asserted = code %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_match_all('"([a-zA-Z0-9_]+)"\\s*=\\s*\\.normalise[^\\(]*\\(\\s*([a-zA-Z0-9_\\.]+)') %>%
    as.data.frame() %>%
    dplyr::select(variable=X2,canonical=X3) %>%
    dplyr::filter(!canonical %in% c("dplyr","renameToVars"))

  non_checkboxes = implied %>% dplyr::full_join(asserted, by="variable")

  # checkbox mappings
  implied2 = combined %>% dplyr::filter(type == "checkbox") %>% tidyr::unnest(level_df) %>%
    dplyr::mutate(
      prefix = .forms_to_category[form],
      suffix = value %>%
        stringr::str_remove_all("\\s+\\([^\\)]+\\)") %>%
        stringr::str_remove_all("\\s+\\[[^\\]]+\\]") %>%
        tolower() %>% stringr::str_replace_all("[^a-z]+"," ") %>%
        trimws() %>% stringr::str_replace_all("\\s+","_"),
      candidate = paste0(prefix,".",suffix)
      ) %>%
    dplyr::select(-prefix,-suffix, -levels, -level_dp, -level_code, -level_size) %>%
    dplyr::group_by(across(c(tidyselect::everything(),-source, -label))) %>%
    dplyr::summarise(
      source = paste0(sort(source),collapse = "|"),
      label = dplyr::first(label),
      .groups = "drop"
    )
    # dplyr::mutate(present = TRUE) %>%
    # tidyr::pivot_wider(names_from = source, values_from = present,values_fill = FALSE)


  asserted2 = code %>%
    stringr::str_remove_all("\\n") %>%
    stringr::str_match_all('"([a-zA-Z0-9_]+)"\\s*=\\s*\\.normalise_checkboxes[^\\(]*\\([^\\(]*\\(\\s*([a-zA-Z0-9_\\.,\\s]+)')
  asserted2 = asserted2[[1]]
  colnames(asserted2) = c("X","variable","opts")
  asserted2 = asserted2 %>%
    tibble::as_tibble() %>%
    dplyr::select(variable,opts) %>%
    dplyr::mutate(opts = stringr::str_split(opts, ",\\s*")) %>%
    dplyr::mutate(opts = purrr::map(opts, ~ tibble::tibble(canonical = .x) %>% dplyr::mutate(key=dplyr::row_number()))) %>%
    tidyr::unnest(opts) %>%
    dplyr::filter(!canonical %in% c(" "))

  checkboxes = implied2 %>% dplyr::full_join(asserted2, by=c("variable","key"))

  dplyr::bind_rows(checkboxes,non_checkboxes) %>% dplyr::arrange(form,variable) %>%
    dplyr::mutate(best = ifelse(is.na(canonical),candidate,canonical)) %>%
    readr::write_csv(input("metadata-review",Sys.Date(),"metadata_review.csv"))
}

generate_mapping_list = function() {
  # TODO: need column names
  # meta = meta %>% dplyr::rowwise() %>% dplyr::mutate(
  #   f = dplyr::case_when(
  #     type == "radio" ~ .normalise_list( values = levels$value, codes = levels$key),
  #     type == "dropdown" ~  .normalise_list( values = levels$value, codes = levels$key),
  #     type == "checkbox" ~  .normalise_checkboxes(renameToVars = levels$value),
  #     type == "calc" ~
  #       type == "text" & stringr::str_detect(variable,"date") ~ .normalise_date()
  #     type == "text" & stringr::str_detect(variable,"date") ~ .normalise_list()
  #   )
  # )
}
