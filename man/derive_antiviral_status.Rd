% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment-avoncap.R
\name{derive_antiviral_status}
\alias{derive_antiviral_status}
\title{Create a flag for patients who have been given antivirals}
\usage{
derive_antiviral_status(df, v)
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
Create a flag for patients who have been given antivirals
}
\concept{derived}
