#' High level IMD to Townsend score map
#'
#' A high level mapping from IMD to Townsend score
#' This is inaccurate as townsend score
#'
#' @format ## `imd_to_townsend`
#' A data frame with 10 rows and 2 columns:
#' \describe{
#'   \item{imd_decile}{The IMD}
#'   \item{mean_townsend}{the average townsend score for this IMD}
#'   ...
#' }
#' @source <https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/845345/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv>
#' @source <https://s3-eu-west-1.amazonaws.com/statistics.digitalresources.jisc.ac.uk/dkan/files/Townsend_Deprivation_Scores/Scores/Scores-%202011%20UK%20LSOA.csv>

"imd_to_townsend"
