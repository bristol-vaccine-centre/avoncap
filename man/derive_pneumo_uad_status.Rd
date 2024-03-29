% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment-uad.R
\name{derive_pneumo_uad_status}
\alias{derive_pneumo_uad_status}
\title{Calculate summary status from UAD (or other serotype) panel results}
\usage{
derive_pneumo_uad_status(df, ...)
}
\arguments{
\item{df}{a pneumo serotype dataframe}

\item{...}{ignored}
}
\value{
a dataframe with additional columns \code{pneumo.uad1_panel_result},
\code{pneumo.uad2_panel_result}, \code{pneumo.non_uad_panel_result},
\code{pneumo.serotype_summary_result}
}
\description{
logic is defined in \code{\link[=derive_pcv_groupings]{derive_pcv_groupings()}}.
}
\concept{derived}
