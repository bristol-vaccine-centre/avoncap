% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{denom_by_age_by_day}
\alias{denom_by_age_by_day}
\title{The avoncap denominator dataset}
\format{
A dataframe containing the following columns:
\itemize{
\item method (character) - estimation method. The default is "Campling 2019"
\item age (character) - the age category
\item date (date) - the date for which this estimate is valid
\item population (integer) - the esimtate of the population size for that age group on that day
}

No default value.

32592 rows and 4 columns
}
\usage{
data(denom_by_age_by_day)
}
\description{
The denominator is a time varying quantity
}
\keyword{datasets}
