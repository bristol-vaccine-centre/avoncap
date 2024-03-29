% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment-avoncap.R
\name{derive_patient_identifier}
\alias{derive_patient_identifier}
\title{Create a unique patient level id (if it does not already exist)}
\usage{
derive_patient_identifier(df, v)
}
\arguments{
\item{df}{the dataframe.}

\item{v}{the value set. usually precomputed by the augment framework the value
set can be explicitly supplied with \code{v = get_value_sets(df)}}
}
\value{
a dataframe
}
\description{
The patient identifier is derived from the record number or the first record
number (ensuring it matches) an entry in the record number. This deals with
multiple admissions in the data set. In the patient identifiable NHS data
this is the NHS number.
}
\concept{derived}
