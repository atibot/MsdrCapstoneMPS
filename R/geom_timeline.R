#' Create the ggproto object for geom_timeline
#'
#' This function creates the ggproto Geom object to plot the necessary grid grob for
#' \code{geom_timeline}
#'
#' @examples
#' library(data.table)
#' raw_noaa <- as.data.table(noaa_data)
#'
#' ## Build date variable
#' clean_noaa <- eq_clean_data(raw_noaa)
#' clean_noaa <- eq_location_clean(clean_noaa)
#' 
#' ## Set key to date
#' setkey(clean_noaa, DATE)
#' 
#' ## Pull test set of 5 years
#' eq_subset <- clean_noaa[DATE >= "2005-01-01" & DATE <= "2010-12-31"]
#' 
#' ## Subset to 5 countries
#' top_countries <- eq_subset[,.N,by = COUNTRY][order(-N)][1:5, COUNTRY]
#' eq_subset <- eq_subset[COUNTRY %in% top_countries]
#' 
#' ## Set country to factor
#' eq_subset[, COUNTRY := as.factor(COUNTRY)]
#' 
#' ## Set earthquake magnitude to numeric
#' eq_subset[, EQ_PRIMARY := as.numeric(EQ_PRIMARY)]
#' 
#' ## Plot all countries on single time line with labels for 20 largest
#' # earthquakes.
#' g <- ggplot(data = eq_subset, aes(x = DATE))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS,size = EQ_PRIMARY)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake()
#' ## Plot each country on it's own horizontal line with labels for 20 largest
#' # earthquakes
#' g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS, size = EQ_PRIMARY)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake() +
#'   labs(y = "")
#' ## Plot each country on it's own horizontal line with labels for 20 largest
#' # earthquakes. Make all circles the same size.
#' g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake() +
#'   labs(y = "")
#'
#' @import ggplot2 grid

GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
  required_aes = c("x", "xmin", "xmax"),
  non_missing_aes = c("size", "colour"),
  default_aes = ggplot2::aes(
    shape = 19, alpha = 0.5, size = 1.5, colour = 'black', fill = NA,
    stroke = 0.5, y = 0.2
  ),

  draw_panel = function(data, panel_params, coord) {
    data$x <- as.Date(data$x, origin = "1970-01-01")

    data$xmin <- as.Date(data$xmin)
    data$xmax <- as.Date(data$xmax)

    data <- subset(data, x >= data$xmin & x <= data$xmax)

    coords <- coord$transform(data, panel_params)

    grid::pointsGrob(
      coords$x,
      coords$y,
      pch = coords$shape,
      gp = grid::gpar(
        alpha = coords$alpha,
        col = coords$colour,
        fontsize = coords$size * .pt + coords$stroke * .stroke / 2,
        lwd = coords$stroke * .stroke / 2
      )
    )
  },

  draw_key = ggplot2::draw_key_point
)

#' A geom for adding a timeline plot
#'
#' This geom plots a timeline with circles showing the dates when the event occurred.
#' It is intended for the purposes of graphically exploring the NOAA Significant
#' Earthquake Database (included in this package), but can show any data with a column
#' of valid \code{date} objects
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following aesthetics (required are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \code{y}
#'   \item \code{color}
#'   \item \code{fill}
#'   \item \code{size}
#'   \item \code{alpha}
#' }
#'
#' @inheritParams ggplot2::geom_point
#'
#' @param xmin (optional) A Date object of the earliest data to be plotted
#' @param xmax (optional) A Date object of the latest data to be plotted
#'
#' @details This is a general purpose timeline plotting geom, particularly tuned to
#'   the NOAA Significant Earthquake Database included with this package.  Each event
#'   with an associated date will be plotted as a circle on the timeline, so long as
#'   the event lies between \code{x_min} and \code{x_max}.
#'
#'   Additional optional aesthetics make the geom much more useful.  An optional
#'   \code{y} aesthetic allows the comparison of different timelines over the same range.
#'   Size can show how big an event was (such as an earthquake), and color/fill and alpha
#'   can convey yet more information
#'
#' @section References:
#' \itemize{
#'  \item geom_points: \url{https://github.com/tidyverse/ggplot2/blob/master/R/geom-point.r}
#'  \item extending ggplot: \url{https://github.com/tidyverse/ggplot2/blob/master/vignettes/extending-ggplot2.Rmd}
#' }
#'
#' @examples
#' library(data.table)
#' raw_noaa <- as.data.table(noaa_data)
#'
#' ## Build date variable
#' clean_noaa <- eq_clean_data(raw_noaa)
#' clean_noaa <- eq_location_clean(clean_noaa)
#' 
#' ## Set key to date
#' setkey(clean_noaa, DATE)
#' 
#' ## Pull test set of 5 years
#' eq_subset <- clean_noaa[DATE >= "2005-01-01" & DATE <= "2010-12-31"]
#' 
#' ## Subset to 5 countries
#' top_countries <- eq_subset[,.N,by = COUNTRY][order(-N)][1:5, COUNTRY]
#' eq_subset <- eq_subset[COUNTRY %in% top_countries]
#' 
#' ## Set country to factor
#' eq_subset[, COUNTRY := as.factor(COUNTRY)]
#' 
#' ## Set earthquake magnitude to numeric
#' eq_subset[, EQ_PRIMARY := as.numeric(EQ_PRIMARY)]
#' 
#' ## Plot all countries on single time line with labels for 20 largest
#' # earthquakes.
#' g <- ggplot(data = eq_subset, aes(x = DATE))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS,size = EQ_PRIMARY)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake()
#' ## Plot each country on it's own horizontal line with labels for 20 largest
#' # earthquakes
#' g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS, size = EQ_PRIMARY)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake() +
#'   labs(y = "")
#' ## Plot each country on it's own horizontal line with labels for 20 largest
#' # earthquakes. Make all circles the same size.
#' g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake() +
#'   labs(y = "")
#'
#' @import ggplot2 grid
#' @export

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, xmin = NULL,
                          xmax = NULL, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, xmin = xmin, xmax = xmax, ...)
  )
}

#' Create a theme for the earthquake timeline plot
#'
#' This function creates the theme to plot the earthquake timelines
#'
#' @section References:
#' \itemize{
#'  \item geom_points: \url{https://github.com/jrnold/ggthemes/edit/master/R/economist.R}
#' }
#'
#' @examples
#' library(data.table)
#' raw_noaa <- as.data.table(noaa_data)
#'
#' ## Build date variable
#' clean_noaa <- eq_clean_data(raw_noaa)
#' clean_noaa <- eq_location_clean(clean_noaa)
#' 
#' ## Set key to date
#' setkey(clean_noaa, DATE)
#' 
#' ## Pull test set of 5 years
#' eq_subset <- clean_noaa[DATE >= "2005-01-01" & DATE <= "2010-12-31"]
#' 
#' ## Subset to 5 countries
#' top_countries <- eq_subset[,.N,by = COUNTRY][order(-N)][1:5, COUNTRY]
#' eq_subset <- eq_subset[COUNTRY %in% top_countries]
#' 
#' ## Set country to factor
#' eq_subset[, COUNTRY := as.factor(COUNTRY)]
#' 
#' ## Set earthquake magnitude to numeric
#' eq_subset[, EQ_PRIMARY := as.numeric(EQ_PRIMARY)]
#' 
#' ## Plot all countries on single time line with labels for 20 largest
#' # earthquakes.
#' g <- ggplot(data = eq_subset, aes(x = DATE))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS,size = EQ_PRIMARY)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake()
#' ## Plot each country on it's own horizontal line with labels for 20 largest
#' # earthquakes
#' g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS, size = EQ_PRIMARY)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake() +
#'   labs(y = "")
#' ## Plot each country on it's own horizontal line with labels for 20 largest
#' # earthquakes. Make all circles the same size.
#' g <- ggplot(data = eq_subset, aes(x = DATE, y = COUNTRY))
#' g + geom_timeline(xmin = "2005-01-01", xmax = "2010-12-31",
#'   aes(color = DEATHS)) + geom_timeline_labels(n_max = 20,
#'   aes(label = LOCATION_NAME, magnitude = EQ_PRIMARY)) + theme_earthquake() +
#'   labs(y = "")
#'
#' @import ggplot2 grid
#' @export

theme_earthquake <- function(base_size = 10, base_family = "sans",
                             y_axis = TRUE) {

  ret <- theme_foundation(base_size = base_size, base_family = base_family) +
    theme(line = element_line(colour = "black"),
          rect = element_rect(fill = "white", colour = NA,
                              linetype = 1),
          text = element_text(colour = "black"),

          ## Axis
          axis.line = element_line(size = rel(0.8)),
          axis.line.y = element_blank(),
          axis.text = element_text(size = rel(1)),
          axis.text.x = element_text(vjust = 0,
                                     margin = margin(t = base_size,
                                                     unit = "pt")),
          axis.text.y = element_text(hjust = 0,
                                     margin = margin(r = base_size,
                                                     unit = "pt")),
          axis.ticks = element_line(),
          axis.ticks.y = element_blank(),
          axis.title = element_text(size = rel(1)),
          axis.title.x = element_text(),
          axis.title.y = element_text(angle = 90),
          axis.ticks.length = unit( -base_size * 0.5, "points"),

          ## Legend
          legend.background = element_rect(linetype = 0),
          legend.spacing = unit(base_size * 1.5, "points"),
          legend.key = element_rect(linetype = 0),
          legend.key.size = unit(1.2, "lines"),
          legend.key.height = NULL,
          legend.key.width = NULL,
          legend.text = element_text(size = rel(0.70)),
          legend.text.align = NULL,
          legend.title = element_text(size = rel(1),  hjust = 0),
          legend.title.align = NULL,
          legend.position = "bottom",
          legend.direction = NULL,
          legend.justification = "center",

          ## Panel
          panel.background = element_rect(fill = "white", linetype = 0),
          panel.border = element_blank(),
          panel.grid.major.y = element_line(colour = "lightgray", size = rel(1.75)),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(0.25, "lines"),

          ## Strip
          strip.background = element_rect(fill = "white",
                                          colour = NA, linetype = 0),
          strip.text = element_text(size = rel(1.25)),
          strip.text.x = element_text(),
          strip.text.y = element_text(angle = -90),

          ## Plot title
          plot.background = element_rect(fill = "white"),
          plot.title = element_text(size = rel(1.5),
                                    hjust = 0, face = "bold"),
          plot.margin = unit(c(6, 5, 6, 5) * 2, "points"),
          complete = TRUE
          )

  ret
}
