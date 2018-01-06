#' A geom for labeling the timeline plot from \code{\link{geom_timeline}}
#'
#' This geom labels the timeline created from \code{\link{geom_timeline}}.
#'  It draws vertical lines up from the points on the timeline, with labels
#'  for the particular event.
#' You have the option of only labeleing the top \code{n_max} number of events
#'  sorted by a specified variable
#'
#' @section Aesthetics:
#' \code{geom_timeline} understands the following aesthetics (required are in
#'  bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{label}} The labels to be added
#'   \item \code{y}
#'   \item \code{magnatude} The feature whose value determines the top
#'    \code{n_max} to be labeled
#'   \item \code{color}
#'   \item \code{fill}
#'   \item \code{size}
#'   \item \code{alpha}
#' }
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams geom_timeline
#'
#' @param n_max The top \code{n_max} labels to plot.  For example, if
#'  earthquake occurrences are being plotted, and you only want to label 5
#'  largest earthquakes, by magnitude, pass 5 here
#'
#' @section References:
#' \itemize{
#'  \item geom_text: \url{https://github.com/tidyverse/ggplot2/blob/master/R/geom-text.r}
#' }
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
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

geom_timeline_labels <- function(mapping = NULL,
                                 data = NULL,
                                 na.rm = TRUE,
                                 show.legend = NA,
                                 stat = "identity",
                                 position = "identity",
                                 inherit.aes = TRUE,
                                 n_max = NULL, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabels,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}

#' Create the ggproto object for geom_timeline_labels
#'
#' This function creates the ggproto Geom object to plot the necessary grid grob for
#' \code{geom_timeline_labels}
#'
#' @examples
#' library(data.table)
#' library(ggplot2)
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

GeomTimelineLabels <- ggplot2::ggproto('GeomTimelineLabels', ggplot2::Geom,
  required_aes = c("x", "label", "magnitude"),

  default_aes = ggplot2::aes(
    colour = "black", size = 3, angle = 45, hjust = 0,
    vjust = 0, alpha = NA, family = "", n_max = 5,
    fontface = 1, lineheight = 1.2, y = 0.2
  ),

  draw_key = ggplot2::draw_key_text,

  draw_panel = function(data, panel_params, coord) {

    ## Order by magnitude and subset to n_max earthquakes
    data <- data[order(data$magnitude, decreasing = TRUE),]
    data <- data[1:data$n_max[1],]

    coords <- coord$transform(data, panel_params)

    Timeline_segments <- grid::segmentsGrob(
      x0 = unit(coords$x, "npc"),
      x1 = unit(coords$x, "npc"),
      y0 = unit(coords$y, "npc"),
      y1 = unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
      default.units = "npc",
      arrow = NULL,
      name = NULL,
      gp = grid::gpar(),
      vp = NULL
    )

    Timeline_labels <- grid::textGrob(
      label = coords$label,
      x = unit(coords$x, "npc"),
      y = unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
      rot = 45,
      just = "left",
      gp = gpar(
        col = alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      )
    )

    grid::gList(Timeline_segments, Timeline_labels)
  }
)
