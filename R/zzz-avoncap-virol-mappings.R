# N.b. this is in a non standard order in the AvonCAP code book
.virol_isolate_list = c("Influenza A",
                        "Influenza B",
                        "RSV",
                        "Adenovirus",
                        "Human Metapneumovirus",
                        "Human Rhinovirus/Enterovirus",
                        "Parainfluenza Type 1",
                        "Parainfluenza Type 2",
                        "Parainfluenza Type 3",
                        "Parainfluenza Type 4",
                        "Covid-19",
                        "Other",
                        "Coronavirus 229E",
                        "Coronavirus HKU1",
                        "Coronavirus NL63",
                        "Coronavirus OC43",
                        "Middle East Respiratory Syndrome (MERS)",
                        "Bordetella parapertussis (IS1001)",
                        "Bordetella pertussis (ptxP)",
                        "Chlamydia pneumoniae",
                        "Mycoplasma pneumoniae")


keys_avoncap_radio  = function(instrument) {
  list(
    "radio" = sprintf("{admin.record_number}_radio_%d",instrument)
  )
}

#' Normalise the avoncap data virology data
#'
#' `r .document_mapping(map_avoncap_virol)`
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_virol = function(instrument) {
  tmp = list(
    "viral_testing_performed" = .normalise_yesno(virol.test_performed),
    "virology_date_of_asst" = .normalise_date(virol.test_date),
    "viroldays" = .normalise_name(virol.test_days_from_admission),
    "specimen_type" = .normalise_list(virol.test_type,c("Sputum", "Saliva", "Bronchoalveolar lavage (BAL)", "Pleural Fluid", "Swabbed material", "Blood")),
    "virus_isolated" = .normalise_yesno(virol.pathogen_detected),
    "test_type" = .normalise_list(virol.test_type,c("PCR - COVID only", "PCR - Respiratory panel", "PCR Respiratory (Biofire)", "Viral Culture", "Lateral Flow - COVID only", "POCT Test (Abbott)")),
    "virus_pathogen" = .normalise_checkboxes_to_nested_list(virol.pathogen, .virol_isolate_list, "pathogen", "detected"),
    # "viral_other", text
    "virol_patient_lab" = .normalise_list(virol.test_provenance,c("Laboratory confirmed report (i.e. on ICE or Open Net)", "Patient reported (e.g. COVID-19 community testing)"))
  )
  names(tmp) = sprintf("%s_%d",names(tmp),instrument)
  return(c(
    "record_number" = .normalise_name(admin.record_number),
    "ac_study_number" = .normalise_study_id(admin.consented_record_number),
    tmp))
}

