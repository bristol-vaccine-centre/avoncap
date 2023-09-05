% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate-avoncap.R
\name{validate_data}
\alias{validate_data}
\title{Validate AvonCap raw data}
\usage{
validate_data(rawData, ...)
}
\arguments{
\item{rawData}{\itemize{
\item the raw data from \code{load_data()}
}}

\item{...}{
  Arguments passed on to \code{\link[=normalise_generic]{normalise_generic}}
  \describe{
    \item{\code{remove_mapped}}{gets rid of original columns for which we have a mapping
(leaving the new versions)}
    \item{\code{remove_unmapped}}{gets rid of columns for which we do not have a mapping}
    \item{\code{mappings}}{a set of mappings (see \code{zzz-avoncap-mappings.R})}
    \item{\code{messages}}{a set of \code{dtrackr} glue specs that populate the first box fo
the flow chart. (can use \code{{files}}, \code{{reproduce_at}}, \code{{date}}, \code{{.total}})}
    \item{\code{data_source_info}}{\itemize{
\item if not null a filename, and the function will write
out a file with the details of the input files used.
}}
  }}
}
\value{
a tracked dataframe with
}
\description{
Runs a set of QA checks
}
\details{
files Most of the sanitisation code is held in the
\code{zzz-avoncap-mappings.R} file.
}