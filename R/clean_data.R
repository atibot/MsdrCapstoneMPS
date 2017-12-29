#' Clean latitude, longitude, and date in NOAA significant earthquake database
#'
#' A function that builds a date field and ensures that the longitude and
#'  latitude of the earthquake location are numeric.
#' The function takes in the NOAA data, which is obtained from
#'  \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}) and
#'  returns a data frame with a valid date field and numeric longitude and
#'  latitude.
#'
#' @param noaa_data NOAA significant earthquakes dataset
#'
#' @return A data.frame, data.table with updated longitude, latitude, and date
#'  variables.
#'
#' @section Imported functions:
#'  data.table syntax
#'
#' @examples
#' library(data.table)
#' raw_noaa <- as.data.table(noaa_data)
#' clean_noaa <- eq_clean_data(raw_noaa)
#'
#' @import data.table
#' @export

eq_clean_data <- function(noaa_data) {
  noaa_data <- as.data.table(noaa_data)

  ## Fix date
  noaa_data[, DATE := as.Date(paste(YEAR, MONTH, DAY, sep = "-"),
                              format = "%Y-%m-%d")]

  ## Make latitude and longitude numeric
  noaa_data[, c("LATITUDE", "LONGITUDE") := lapply(.SD, as.numeric),
            .SDcols=c("LATITUDE", "LONGITUDE")]

  return(noaa_data)
}

#' Clean location in NOAA significant earthquake database
#'
#' A function that removes country information from the location field and
#'  ensures that the location is in title case.
#' The function takes in the NOAA data, which is obtained from
#'  \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}) and
#'  returns a data frame with a cleaned location name
#'
#' @param noaa_data NOAA significant Earthquakes dataset
#'
#' @return A data.frame, data.table with updated location name
#'
#' @section Imported functions:
#'  data.table syntax
#'
#' @examples
#' library(data.table)
#' raw_noaa <- as.data.table(noaa_data)
#' clean_noaa <- eq_location_clean(raw_noaa)
#'
#' @import data.table
#' @export

eq_location_clean <- function(noaa_data) {
  noaa_data <- as.data.table(noaa_data)

  ## Strip country name from location
  noaa_data[, LOCATION_NAME := sub('^.*: *','', LOCATION_NAME)]
    # removes any character between the beginning of the line and the colon
      # and any following spaces

  noaa_data[, LOCATION_NAME := tools::toTitleCase(tolower(LOCATION_NAME))]

  return(noaa_data)
}
