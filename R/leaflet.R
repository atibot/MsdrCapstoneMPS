#' Create an interactive map of historical earthquakes
#'
#' Create an interactive map of historical earthquakes using the NOAA
#'  earthquake database included with this package.  Earthquakes are plotted
#'  as circles with their radii proportional to the magnatude of the
#'  earthquake. Optionally, labels can be passed which will pop up when the
#'  earthquake is clicked on.
#'
#' This function uses of the popular \code{leaflet} package, which creates
#'  interactive html maps within R
#'
#' @param earthquake_data A dataframe with earthquake location columns
#'  \code{LATITUDE} and \code{LONGITUDE}, and magnatude column
#'  \code{EQ_PRIMARY}. The included noaa_data dataframe is already
#'   correctly formateed, and is intended to be used with this function
#' @param annot_col The character name of the column in \code{data} containing
#'  a character vector of optional popup text to be shown when an earthquake
#'  is clicked on.
#'
#' @return An html map object, to which further Leaflet objects can be added
#'
#' @examples
#' library(data.table)
#' library(dplyr)
#' raw_noaa <- as.data.table(noaa_data)
#' ## Plot earthquakes in Mexico with date labels in popup windows
#' raw_noaa %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#' ## Plot earthquakes in Mexico with nicely formated popup windows
#' raw_noaa %>%
#'   eq_clean_data() %>%
#'   eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'
#' @seealso \pkg{leaflet}
#'
#' @importFrom dplyr mutate filter
#' @importFrom magrittr "%>%"
#' @import leaflet
#' @export

eq_map <- function(earthquake_data, annot_col) {
  map_plot <- leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = earthquake_data, radius = ~ EQ_PRIMARY,
      opacity = 0.5, lng = ~ LONGITUDE, lat = ~ LATITUDE,
      popup = ~ as.character(earthquake_data[[annot_col]]))

  return(map_plot)
}

#' Build an html label for earthquakes
#'
#' Create a nicely formatted HTML label for use with \code{\link{eq_map}}
#'  showing Location, Magnatude, and Total Deaths.  Missing values are skipped
#'  in the created label.
#'
#' @param earthquake_data A dataframe containing location names in column
#'  \code{LOCATION_NAME}, earthquake magnitude in column \code{EQ_PRIMARY},
#'  and total deaths caused by the earthquake in column \code{TOTAL_DEATHS}. 
#'  An example of a correctly formatted dataset is noaa_data, which is included
#'  in the package.
#'
#' @return A character vector of formatted html strings to label the popups in
#'   \code{\link{eq_map}}
#'
#' @examples
#' library(data.table)
#' raw_noaa <- as.data.table(noaa_data)
## Plot earthquakes in Mexico with date labels in popup windows
#' raw_noaa %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#' ## Plot earthquakes in Mexico with nicely formated popup windows that include location, magnitude, and total deaths for each earthquake
#' raw_noaa %>%
#'   eq_clean_data() %>%
#'   eq_location_clean() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#'
#' @seealso \code{\link{eq_map}}, \pkg{leaflet}
#'
#' @export

eq_create_label <- function(earthquake_data) {
  popup_info <- ""

  popup_info <- ifelse(!is.na(earthquake_data$LOCATION_NAME),
        paste0(popup_info, "<b>Location: </b>", earthquake_data$LOCATION_NAME,
          "<br />"),
        popup_info)

  popup_info <- ifelse(!is.na(earthquake_data$EQ_PRIMARY),
          paste(popup_info, "<b>Magnitude:</b>", earthquake_data$EQ_PRIMARY,
            "<br />"),
          popup_info)
  popup_info <- ifelse(!is.na(earthquake_data$TOTAL_DEATHS),
          paste(popup_info, "<b>Total Deaths:</b>",
            earthquake_data$TOTAL_DEATHS, "<br />"),
          popup_info)

  return(popup_info)
}
