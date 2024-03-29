% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping-avoncap-micro.R
\name{map_avoncap_micro}
\alias{map_avoncap_micro}
\title{Normalise the avoncap data microbiology data}
\usage{
map_avoncap_micro(instrument)
}
\arguments{
\item{instrument}{the numeric instrument number}
}
\value{
a list
}
\description{
\itemize{
\item microtest_done -> micro.test_performed (yesno)
\item microtest_date -> micro.test_date (date)
\item microday -> micro.test_days_from_admission (pos_integer)
\item micro_test -> micro.test_type (list)
\item micro_isolates -> micro.pathogen_detected (yesno_unknown)
\item isolate_identified -> micro.pathogen, .micro_isolate_list (checkboxes_to_nested_list)
\item pn_result -> micro.pneumo_serotype_status (list)
\item pn_st -> micro.pneumo_serotype (pneumo_serotype)
\item micro_lab -> micro.sent_to_central_lab (yesno_unknown)
\item pen_susceptibility -> micro.penicillin_susceptibility (checkboxes_to_list)
\item septrin_susceptibility -> micro.septrin_susceptibility (checkboxes_to_list)
\item doxy_susceptibility -> micro.doxycycline_susceptibility (checkboxes_to_list)
\item levoflox_suscept -> micro.levofloxacin_susceptibility (checkboxes_to_list)
\item cef_susceptibility -> micro.ceftriaxone_susceptibility (checkboxes_to_list)
\item pn_uat_result -> micro.pneumo_binax_now (list)
\item lg_uat_result -> micro.pneumo_legionella_uat (list)
\item micro_final_report -> micro.is_final_report (yesno)
}
}
\concept{map}
