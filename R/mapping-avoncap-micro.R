
.micro_isolate_list = c("Aspergillus",
                        "Streptococcus Agalactiae",
                        "Candida",
                        "Achromobacter Xylosoxidans",
                        "Bacteroides",
                        "Bacteroides fragilis",
                        "Bacteroides ovatus",
                        "Bacteroides uniformis",
                        "Burholderia cepacia",
                        "Citrobacter freundii complex",
                        "Citrobacter koseri",
                        "Clostridium perfringens",
                        "Eggerthella lenta",
                        "Enterobacter aerogenes",
                        "Enterobacter cloacae",
                        "Enterococcus faecalis",
                        "Enterococcus faecium",
                        "Escherichia coli",
                        "Haemophilus influenzae",
                        "Haemophilus parainfluenzae",
                        "Klebsiella oxytoca",
                        "Klebsiella pneumoniae",
                        "Morganella morganii",
                        "Peptostreptococcus anaerobius",
                        "Proteus mirabilis",
                        "Pseudomonas aeruginosa",
                        "Serratia marcescens",
                        "Staphylcoccus aureus",
                        "Streptococcus anginosus",
                        "Streptococcus pneumoniae",
                        "Streptococcus salivarius group",
                        "Stenotrophomonas maltophilia",
                        "Acinetobacter",
                        "Aspergillus niger",
                        "Bacillus",
                        "Candida albicans",
                        "Candida glabrata",
                        "Candida tropicalis",
                        "Coagulase negative staphylcoccus",
                        "Corynebacterium",
                        "Enterococcus",
                        "Gram positive coccus",
                        "Klebsiella",
                        "Legionella pneumoniae",
                        "MRSA",
                        "MSSA",
                        "Moraxella catarrhalis",
                        "Mycobacterium tuberculosis",
                        "Pneumocytis jirovecii",
                        "Pseudomonas",
                        "Pseudomonas fluorescens",
                        "Staphylcoccus",
                        "Staphylococcus capitis",
                        "Staphylococcus epidermidis",
                        "Staphylococcus hominis",
                        "Streptococcus",
                        "Streptococcus beta-hemolytic",
                        "Streptococcus pyogenes",
                        "Streptococcus viridans",
                        "Yeast",
                        "Other")


keys_avoncap_micro  = function(instrument) {
  list(
    "micro" = sprintf("{admin.record_number}_micro_%s",as.character(instrument))
  )
}

#' Normalise the avoncap data microbiology data
#'
#' `r .document_mapping(map_avoncap_micro)`
#'
#' @param instrument the numeric instrument number
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_micro = function(instrument) {
  tmp = list(
    "microtest_done" = .normalise_yesno(micro.test_performed),
    "microtest_date" = .normalise_date(micro.test_date),
    "microday" = .normalise_pos_integer(micro.test_days_from_admission),
    "micro_test" = .normalise_list(micro.test_type,
        c("Blood culture", "Sputum", "Respiratory swab", "Pleural fluid",
          "Bronchoalveolar lavage", "Tracheal Aspirate",
          "Urinary antigen - pneumococcus", "Urinary antigen - legionella")
    ),
    "micro_isolates" = .normalise_yesno_unknown(micro.pathogen_detected),
    "isolate_identified" = .normalise_checkboxes_to_nested_list(micro.pathogen, .micro_isolate_list, "pathogen", "detected"),
    # "micro_other", text
    "pn_result" = .normalise_list(micro.pneumo_serotype_status,
        c("Not tested/no result", "Non-typeable", "Serotype obtained")),
    "pn_st" = .normalise_pneumo_serotype(micro.pneumo_serotype),
    "micro_lab" = .normalise_yesno_unknown(micro.sent_to_central_lab),
    "pen_susceptibility" = .normalise_checkboxes_to_list(micro.penicillin_susceptibility,
        c("Unknown", "Sensitive", "Intermediate", "Resistant")),
    "septrin_susceptibility" = .normalise_checkboxes_to_list(micro.septrin_susceptibility,
        c("Unknown", "Sensitive", "Intermediate", "Resistant")),
    "doxy_susceptibility" = .normalise_checkboxes_to_list(micro.doxycycline_susceptibility,
        c("Unknown", "Sensitive", "Intermediate", "Resistant")),
    "levoflox_suscept" = .normalise_checkboxes_to_list(micro.levofloxacin_susceptibility,
        c("Unknown", "Sensitive", "Intermediate", "Resistant")),
    "cef_susceptibility" = .normalise_checkboxes_to_list(micro.ceftriaxone_susceptibility,
        c("Unknown", "Sensitive", "Intermediate", "Resistant")),
    "pn_uat_result" = .normalise_list(micro.pneumo_binax_now, c("Negative","Positive")),
    "lg_uat_result" = .normalise_list(micro.pneumo_legionella_uat, c("Negative","Positive")),
    "micro_final_report" = .normalise_yesno(micro.is_final_report)
    # "micro_report_header", text
  )

  names(tmp) = sprintf("%s_%s",names(tmp),as.character(instrument))
  return(c(
    "record_number" = .normalise_name(admin.record_number),
    "ac_study_number" = .normalise_study_id(admin.consented_record_number),
    tmp))
}
