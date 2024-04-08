key_dates = list(
  mortality_updated = as.Date("2023-12-23"),

  # Variants
  min_alpha = as.Date("2020-12-05"),
  max_wuhan = as.Date("2021-02-13"),
  min_delta = as.Date("2021-05-15"),
  # maxAlpha = as.Date("2021-06-26") # officially according to sanger but there were very low levels of Alpha prior to this
  max_alpha = as.Date("2021-06-01"), # unofficially this cutoff was used in the Delta Omicron paper.
  # minOmicron = as.Date("2021-11-27") # according to sanger
  min_omicron = as.Date("2021-11-07"),
  # according to in hospital data this was the earliest admission with Omicron but
  # this could be a nosicomially acquired case
  # i.e. df %>% dplyr::filter(genomic.variant == "Omicron") %>% dplyr::summarise(min = min(admission.date)) %>% dplyr::pull(min)
  # maxDelta = as.Date("2022-01-15") # according to sanger
  max_delta = as.Date("2022-02-07") # according to in hospital results
)

usethis::use_data(key_dates, overwrite = TRUE)

