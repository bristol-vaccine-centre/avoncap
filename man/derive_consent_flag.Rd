% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment-avoncap.R
\name{derive_consent_flag}
\alias{derive_consent_flag}
\title{Has a patient withheld consent or withdrawn}
\usage{
derive_consent_flag(df, v)
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
If a patient has withdrawn consent there may still be data fields populated
}
\concept{derived}
