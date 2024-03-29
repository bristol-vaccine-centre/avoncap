% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/augment-avoncap.R
\name{derive_pneumococcal_risk_category}
\alias{derive_pneumococcal_risk_category}
\title{Determine pneumococcal risk group}
\usage{
derive_pneumococcal_risk_category(df, v, ...)
}
\arguments{
\item{df}{the dataframe.}

\item{v}{the value set. usually precomputed by the augment framework the value
set can be explicitly supplied with \code{v = get_value_sets(df)}}

\item{...}{ignored}
}
\value{
a dataframe
}
\description{
Original algorithm from B1851202 SAP defines a 3 class risk group:
}
\details{
High-risk (immunocompromised)
\itemize{
\item Asplenia - not supported
\item Cancer/Malignancy, Hematologic - OK
\item Cancer/Malignancy, Solid Tumor - OK
\item Chronic Kidney Disease - OK
\item Human Immunodeficiency Virus (HIV) – AIDS - OK
\item Human Immunodeficiency Virus (HIV) – No AIDS - OK
\item Immunodeficiency - OK
\item Immunosuppressant Drug Therapy - OK
\item Organ Transplantation - OK
\item Multiple Myeloma - not supported
}

At Risk (immunocompetent)
\itemize{
\item Asthma - OK
\item Alcoholism - OK
\item Celiac Disease - not supported
\item Chronic Liver Disease without Hepatic Failure - OK
\item Chronic Liver Disease with Hepatic Failure - OK
\item Chronic Obstructive Pulmonary Disease - OK
\item Cochlear Implant - not supported
\item Congestive Heart Failure - OK
\item Coronary Artery Disease (CAD) - OK
\item Chronic Neurologic Diseases - OK
\item Coagulation factor replacement therapy - not supported
\item CSF Leak - not supported
\item Diabetes Treated with Medication - OK
\item Down syndrome - OK
\item Institutionalized in nursing home or LTC facility (Nursing home or long-term care
facility for those with disability or dependency on subject characteristics/risk
determinants eCRF page) - OK
\item Occupational risk with exposure to metal fumes - OK
\item Other Chronic Heart Disease - OK
\item Other Chronic Lung Disease - OK
\item Other pneumococcal disease risk factors - OK
\item Previous Invasive Pneumococcal Disease - not supported
\item Tobacco smoking (Tobacco/E-Cigarettes) - OK
}

Anything else is low risk
}
\concept{derived}
