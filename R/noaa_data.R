#' National Oceanic and Atmospheric Administration Significant Earthquake Data
#'
#' A dataset containing information on destructive earthquakes from 2150 B.C.
#'  to the present that meet at least one of the following criteria: Moderate
#'  damage (approximately $1 million or more), 10 or more deaths, Magnitude 7.5
#'  or greater, Modified Mercalli Intensity X or greater, or the earthquake
#'  generated a tsunami.
#'
#' @name noaa_data
#' @format A data frame with 5982 rows and 48 variables:
#' \describe{
#'  \item{V1}{}
#'  \item{I_D}{}
#'  \item{FLAG_TSUNAMI}{}
#'  \item{YEAR}{}
#'  \item{MONTH}{}
#'  \item{DAY}{}
#'  \item{HOUR}{}
#'  \item{MINUTE}{}
#'  \item{SECOND}{}
#'  \item{FOCAL_DEPTH}{}
#'  \item{EQ_PRIMARY}{}
#'  \item{EQ_MAG_MW}{}
#'  \item{EQ_MAG_MS}{}
#'  \item{EQ_MAG_MB}{}
#'  \item{EQ_MAG_ML}{}
#'  \item{EQ_MAG_MFA}{}
#'  \item{EQ_MAG_UNK}{}
#'  \item{INTENSITY}{}
#'  \item{COUNTRY}{}
#'  \item{STATE}{}
#'  \item{LOCATION_NAME}{}
#'  \item{LATITUDE}{}
#'  \item{LONGITUDE}{}
#'  \item{REGION_CODE}{}
#'  \item{DEATHS}{}
#'  \item{DEATHS_DESCRIPTION}{}
#'  \item{MISSING}{}
#'  \item{MISSING_DESCRIPTION}{}
#'  \item{INJURIES}{}
#'  \item{INJURIES_DESCRIPTION}{}
#'  \item{DAMAGE_MILLIONS_DOLLARS}{}
#'  \item{DAMAGE_DESCRIPTION}{}
#'  \item{HOUSES_DESTROYED}{}
#'  \item{HOUSES_DESTROYED_DESCRIPTION}{}
#'  \item{HOUSES_DAMAGED}{}
#'  \item{HOUSES_DAMAGED_DESCRIPTION}{}
#'  \item{TOTAL_DEATHS}{}
#'  \item{TOTAL_DEATHS_DESCRIPTION}{}
#'  \item{TOTAL_MISSING}{}
#'  \item{TOTAL_MISSING_DESCRIPTION}{}
#'  \item{TOTAL_INJURIES}{}
#'  \item{TOTAL_INJURIES_DESCRIPTION}{}
#'  \item{TOTAL_DAMAGE_MILLIONS_DOLLARS}{}
#'  \item{TOTAL_DAMAGE_DESCRIPTION}{}
#'  \item{TOTAL_HOUSES_DESTROYED}{}
#'  \item{TOTAL_HOUSES_DESTROYED_DESCRIPTION}{}
#'  \item{TOTAL_HOUSES_DAMAGED}{}
#'  \item{TOTAL_HOUSES_DAMAGED_DESCRIPTION}{}
#' }
#'
#' @examples
#' head(noaa_data)
#'
#' @source \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
"noaa_data"
