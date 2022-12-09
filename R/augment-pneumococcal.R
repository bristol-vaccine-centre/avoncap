#' Get vaccine coverage group for known serotype
#'
#' For the longitudinal oneumocococcal data, a range of useful serotype groups
#' is defined in the list `uad_groups`. The `default_pcv_map` gives a set of
#' mappings to group headings that gives the overall serotype distribution by
#' vaccine.
#'
#' @param df the normalised longitudinal pneumococcal data
#' @param ... ignored
#' @param pcv_map a 2 column data frame mapping `group` to `serotype`
#' @param not_matched what to call the column of non-matched serotypes? Default is
#' `Other`, but `Non vaccine type` might be preferred.
#' @param col_name the target column name for the pcv grouping (defaults
#' to `pneumo.phe_pcv_group`)
#'
#' @concept derived
#' @return an augmented data frame with an additional column defined by `col_name`
#' @export
derive_phe_pcv_group = function(df, ..., pcv_map = default_pcv_map, not_matched="Other", col_name = "pneumo.phe_pcv_group") {

  col_name = rlang::ensym(col_name)

  df %>%
    dplyr::left_join(pcv_map, by=c("pneumo.phe_serotype"="serotype")) %>%
    dplyr::mutate(group = ifelse(is.na(group),not_matched, group)) %>%
    dplyr::rename(!!col_name := group)
}

#' Group pneumo serotypes according to e.g. vaccine coverage
#'
#' A range of useful serotype groups is defined in the list `uad_groups`. The
#' `default_pcv_map` gives a set of mappings to group headings that gives the
#'  overall serotype distribution by vaccine.
#'
#' The logic employed in combining elements is:
#'
#' * any(result == "Unknown") ~ "Unknown"
#' * any(result == "Positive") ~ "Positive"
#' * all(result == "Negative") ~ "Negative"
#' * TRUE ~ "Other"
#'
#' @param df the normalised urine antigen data
#' @param ... ignored
#' @param pcv_map a 2 column data frame mapping `group` to `serotype`
#' @param not_matched what to call the column of non-matched serotypes? Default is
#' `Other`, but `Non vaccine type` might be preferred.
#' @param col_name the target column name for the pcv grouping (defaults
#' to `pneumo.pcv_group`)
#'
#' @concept derived
#' @return an augmented data frame with an additional column defined by `col_name`
#' @export
derive_pcv_groupings = function(df, ..., pcv_map = default_pcv_map, not_matched="Other", col_name = "pneumo.pcv_group") {

  col_name = rlang::ensym(col_name)

  tmp = df %>%
    dplyr::select(key.sample, pneumo.urine_antigen) %>%
    tidyr::unnest(pneumo.urine_antigen) %>%
    dplyr::left_join(pcv_map, by=c("test"="serotype")) %>%
    dplyr::mutate(group = ifelse(is.na(group),not_matched, group)) %>%
    dplyr::group_by(key.sample, group) %>%
    dplyr::summarise(
      result = case_when(
        any(result == "Unknown") ~ "Unknown",
        any(result == "Positive") ~ "Positive",
        all(result == "Negative") ~ "Negative",
        TRUE ~ "Other"
      )
    ) %>%
    dplyr::mutate(
      group = factor(group, levels = c(unique(pcv_map$group),not_matched)),
      result = factor(result,levels = c("Negative","Positive","Other","Unknown"))
    ) %>%
    tidyr::nest(.tmp_col = c(group,result)) %>%
    dplyr::rename(!!col_name := .tmp_col)

  df %>% dplyr::left_join(tmp, by = "key.sample")

}



#' Calculate summary status from UAD (or other serotype) panel results
#'
#' logic is defined in [derive_pcv_groupings()].
#'
#' @param df a pneumo serotype dataframe
#' @param ... ignored
#'
#' @concept derived
#' @return a dataframe with additional columns `pneumo.uad1_panel_result`,
#' `pneumo.uad2_panel_result`, `pneumo.non_uad_panel_result`,
#' `pneumo.serotype_summary_result`
#' @export
derive_pneumo_uad_status = function(df, ...) {

  tmp1 = df %>%
    dplyr::select(key.sample, pneumo.urine_antigen) %>%
    tidyr::unnest(pneumo.urine_antigen) %>%
    dplyr::mutate(uad_panel = case_when(
      test %in% uad_groups$uad1 ~ "uad1",
      test %in% uad_groups$uad2 ~ "uad2",
      TRUE ~ "non_uad"
    )) %>%
    dplyr::group_by(key.sample, uad_panel) %>%
    dplyr::summarise(
      summary = case_when(
        any(result == "Unknown") ~ "Unknown",
        any(result == "Positive") ~ "Positive",
        all(result == "Negative") ~ "Negative",
        TRUE ~ "Other"
      ) %>% factor(levels = c("Negative","Positive","Other","Unknown"))
    ) %>%
    dplyr::ungroup() %>%
    tidyr::complete(key.sample, uad_panel = c("uad1","uad2","non_uad"), fill=list(summary = "Unknown")) %>%
    tidyr::pivot_wider(names_from = uad_panel,values_from = summary,names_glue = "pneumo.{uad_panel}_panel_result")

  tmp2 = df %>%
    dplyr::select(key.sample, pneumo.urine_antigen) %>%
    tidyr::unnest(pneumo.urine_antigen) %>%
    dplyr::group_by(key.sample) %>%
    dplyr::summarise(
      pneumo.serotype_summary_result = case_when(
        any(result == "Unknown") ~ "Unknown",
        any(result == "Positive") ~ "Positive",
        all(result == "Negative") ~ "Negative",
        TRUE ~ "Other"
      ) %>% factor(levels = c("Negative","Positive","Other","Unknown")),
      pneumo.serotype_positive_count = sum(result == "Positive"),
      pneumo.serotype_results_complete = !any(result == "Unknown")
    )

  return(df %>%
           left_join(tmp1, by="key.sample") %>%
           left_join(tmp2, by="key.sample"))

}
