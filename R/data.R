#' High level IMD to Townsend score map
#'
#' A high level mapping from IMD to Townsend score
#' This is inaccurate as townsend score
#'
#'
#' A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{imd_decile}{The IMD}
#'   \item{mean_townsend}{the average townsend score for this IMD}
#'   ...
#' }
#' @source <https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv>
#' @source <https://s3-eu-west-1.amazonaws.com/statistics.digitalresources.jisc.ac.uk/dkan/files/Townsend_Deprivation_Scores/Scores/Scores-%202011%20UK%20LSOA.csv>
#' @name imd_to_townsend
NULL

#' Pneumococcal UAD serotype groups and crossmaps
#'
#' A list of pneumococcal serotype / UAD cross mappings
#' @name serotype_data
NULL

#' Pneumococcal UAD serotypes
#'
#' A somewhat complete list of pneumococcal serotypes as seen in Bristol
#' @name phe_serotypes
NULL

#' Pneumococcal serotype PCV groups
#'
#' @name serotype_pcv_map
NULL

#' Serotype UAD mappings
#'
#' @name serotype_uad_map
NULL

#' UAD serotype groups
#'
#' @name uad_groups
NULL

#' UAD PCV map
#'
#' @name uad_pcv_map
NULL


#' Frameworks
#'
#' The list of validation, normalisation and augmentation frameworks.
#' There should be one validation per data set. The may be mulitple
#' normalisations and augmentations depending on the aspect of the data
#' we are extracting (e.g. re-nesting flattened data.)
#' @importFrom data.tree as.Node
#' @name frameworks
NULL


#' Key dates:
#'
#' A list of key dates:
#' * mortality_updated - the last time the NHS mortality data was extracted and added to AvonCAP
#' * min_alpha - earliest observation of the alpha variant
#' * max_wuhan - last observation of the wuhan variant
#' * min_delta - earliest observation of the delta variant
#' * max_alpha - last observation of the alpha variant
#' * min_omicron - earliest observation of the omicron variant
#' * max_delta - last observation of the delta variant
#'
#' @name key_dates
NULL

#' Catchment area GP surgeries
#'
#' The default catchment population for AvonCAP is limited to the Bristol,
#' North Somerset and South Gloucestershire Integrated Care Board (BNSSG ICB).
#' This list is the list of GP surgeries considered part of the denominator.
#'
#' * code - the NHS ODS organisational code of the practice.
#' * name - the official name of the practice
#'
#' @name key_dates
NULL


## denom_by_age_by_day definition ----

#' The avoncap denominator dataset
#'
#' The denominator is a time varying quantity
#'
#' @usage data(denom_by_age_by_day)
#'
#' @format
#' A dataframe containing the following columns:
#' - method (character) - estimation method. The default is "Campling 2019"
#' - age (character) - the age category
#' - date (date) - the date for which this estimate is valid
#' - population (integer) - the esimtate of the population size for that age group on that day
#'
#' No default value.
#'
#' 32592 rows and 4 columns
#'
#' @docType data
#' @keywords datasets
#' @name denom_by_age_by_day
NULL

## denom_by_age_by_day definition ends

#' GP surgeries in the Bristol ICB area
#'
#' The denominator relates only to patients coming from these GP surgeries
#'
#' @usage data(icb_surgeries)
#'
#' @format
#' A dataframe containing the following columns:
#' - code - an official ODS code for the GP surgery
#' - name - the official surgery name.
#'
#' 82 rows and 2 columns
#'
#' @docType data
#' @keywords datasets
#' @name icb_surgeries
NULL

