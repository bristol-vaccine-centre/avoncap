#' Normalise the avoncap data haematology data
#'
#' `r .document_mapping(map_avoncap_haem)`
#'
#' @concept map
#' @return a list
#' @export
map_avoncap_haem = function() {
  list(
    "record_number" = .normalise_name(admin.record_number),
    "ac_study_number" = .normalise_study_id(admin.consented_record_number),
    "ph_7_35" = .normalise_double(haem.blood_gas_ph),
    "glucose" = .normalise_double(haem.glucose),
    "albumin" = .normalise_double(haem.albumin),
    "wcc" = .normalise_double(haem.white_cell_count),
    "hb" = .normalise_double(haem.haemoglobin),
    "haematocrit" = .normalise_double(haem.haemotocrit),
    "pmn" = .normalise_double(haem.neutrophils),
    "lymphocytes" = .normalise_double(haem.lymphocytes),
    "crp" = .normalise_double(haem.crp),
    "na_result" = .normalise_double(haem.sodium),
    "ur_result" = .normalise_double(haem.urea),
    "egfr" = .normalise_double(haem.egfr),
    "ferritin" = .normalise_double(haem.ferritin),
    "nt_probnp" = .normalise_double(haem.pro_bnp),
    "d_dimer" = .normalise_double(haem.d_dimer),
    "patient_blood_group" = .normalise_list(haem.blood_group,
        c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-", "Unknown")
    )
  )
}

# N.B. no specific keys as share with central


.haem_column_names = list(
  haem.blood_gas_ph = "pH on blood gas",
  haem.glucose = "Glucose (mmol/L)",
  haem.albumin = "Albumin (g/L)",
  haem.white_cell_count = "White cell count (x10\u2079/L)",
  haem.haemoglobin = "Haemoglobin (g/L)",
  haem.haemotocrit = "Haematocrit (L/L)",
  haem.neutrophils = "Neutrophils (x10\u2079/L)",
  haem.lymphocytes = "Lymphocytes (x10\u2079/L)",
  haem.crp = "CRP (mg/L)",
  haem.sodium = "Sodium (Na) (mmol/L)",
  haem.urea = "Urea (Ur) (mmol/L)",
  haem.egfr = "eGFR (ml/min/1.73m\u00b2)",
  haem.ferritin = "Ferritin (\u00b5g/mL)",
  haem.pro_bnp = "NT-proBNP (pg/mL)",
  haem.d_dimer = "D-dimer (\u00b5g/ml FEU)",
  haem.blood_group = "Patient Blood Group"
)
