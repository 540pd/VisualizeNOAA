#' Clean NOAA earthquake data
#'
#' This function takes raw NOAA earthquake data and returns a clean data frame.
#' The clean data frame includes a date column created by uniting the year, month, and day,
#' converting it to the Date class. It also converts the LATITUDE and LONGITUDE columns to numeric class.
#'
#' @param path_of_data character string containing path of .tsv file for NOAA earthquake data.
#' @return A clean data frame with date, latitude, and longitude columns.
#' @examples
#' \dontrun{
#' data <- read.csv(system.file("extdata", "your_data.csv", package = "visualizeNOAA"))
#' eq_clean_data(data)
#' }
#'
#' @importFrom dplyr filter mutate select
#' @importFrom lubridate ymd
#' @importFrom readr read_delim
#' @importFrom stringr str_pad
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @export
eq_clean_data <- function(path_of_data = "inst/extdata/earthquakes.tsv") {
  data <- read_delim(path_of_data, delim = "\t", show_col_types = FALSE)
  names(data)<-make.names(names(data))
  clean_data <- data%>%
    dplyr::filter(!is.na(.data$Mo) & !is.na(.data$Dy))  %>%
    dplyr::mutate(date = lubridate::ymd(paste(stringr::str_pad(abs(.data[["Year"]]), width = 4, side = "left", pad = "0"), .data[["Mo"]], .data[["Dy"]], sep = "-")),
                  latitude = as.numeric(.data[["Latitude"]]),
                  longitude = as.numeric(.data[["Longitude"]])) %>%
    dplyr::select("date", "latitude", "longitude", "Location.Name", "Mag", "Deaths") %>%
    dplyr::rename("magnitude" = "Mag", "no_deaths" =  "Deaths")
  return(clean_data)
}


#' Clean earthquake location names
#'
#' This function cleans the LOCATION_NAME column by stripping out the country name
#' (including the colon) and converts names to title case (as opposed to all caps).
#'
#' @param data_df A dataframe containing raw earthquake data with a LOCATION_NAME column.
#' @return A dataframe with the LOCATION_NAME column cleaned.
#' @importFrom dplyr mutate filter arrange
#' @importFrom tidyr  separate
#' @importFrom stringr str_trim str_to_title
#' @export
eq_location_clean <- function(data_df) {
  fin_data <- data_df %>%
    tidyr::separate(.data$Location.Name, into = c("country", "location"), sep = ":", remove = FALSE, extra = "merge", fill = "right") %>%
    dplyr::mutate(country = str_trim(.data[["country"]]),
           location = stringr::str_to_title(stringr::str_trim(.data[["location"]]))) %>%
    dplyr::arrange(dplyr::desc(.data$date))
  return(fin_data)
}

#' Geom for plotting a time line of earthquakes
#'
#' Build a geom for ggplot2 called GeomTimeline for plotting a time line of earthquakes
#' ranging from \code{xmin} to \code{xmax} dates with a point for each earthquake.
#' @importFrom ggplot2 ggproto Geom aes
#' @importFrom grid pointsGrob gpar
#' @export
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline",
  Geom,
  required_aes =  c("x"),
  default_aes = aes(
    shape = 19,
    colour = "black",
    size = .5,
    fill = NA,
    alpha = .5,
    stroke = 0,
    y = 1
  ),
  # draw_key = draw_key_point,
  draw_panel = function(data, panel_params, coord) {
    # browser()
    force_scale<-if(all(data$y==1)){T} else {F}
    coords <- coord$transform(data, panel_params)
    coords$y<-if(force_scale) coords$y*0.5 else coords$y
    stroke_size <- coords$stroke
    stroke_size[is.na(stroke_size)] <- 0
    grid::pointsGrob(
      coords$x,
      coords$y ,
      pch = coords$shape,
      gp = grid::gpar(
        col = alpha(coords$colour, coords$alpha),
        fill = coords$fill,
        # Stroke is added around the outside of the point
        fontsize = coords$size * .pt + stroke_size * .stroke / 2,
        lwd = coords$stroke * .stroke / 2
      )
    )
    # scale_y_continuous(limits = coord$limits$y)
  }
)

#' Geom for plotting a time line of earthquakes
#'
#' Build a geom for ggplot2 called geom_timeline() for plotting a time line of earthquakes
#' ranging from \code{xmin} to \code{xmax} dates with a point for each earthquake.
#'
#' @param mapping Aesthetic mappings created by \code{\link[=aes]{aes()}} or \code{\link[=aes_]{aes_()}}.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If \code{TRUE}, inherits aesthetics from the plot default,
#' which is set with \code{\link[=theme_set]{theme_set()}}. If \code{FALSE}, overrides all aesthetics.
#' @param ... Other arguments passed on to \code{\link[=layer]{layer}}.
#' These are often aesthetics, used to set an aesthetic to a fixed value, like \code{color = "red"} or \code{size = 3}.
#' They may also be parameters to the paired geom/stat.
#' @importFrom ggplot2 layer
#' @export
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          ...) {
  ggplot2::layer(
    geom = GeomTimeline,
    data = data,
    stat = stat,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Geom for adding annotations to earthquake data
#'
#' Build a geom called GeomTimelineLabel for adding annotations to the earthquake data.
#' This geom adds a vertical line to each data point with a text annotation (e.g., the location
#' of the earthquake) attached to each line. There should be an option to subset to \code{n_max}
#' number of earthquakes, where we take the \code{n_max} largest (by magnitude) earthquakes.
#'
#' @importFrom ggplot2 ggproto Geom
#' @importFrom grid segmentsGrob gpar textGrob gList
GeomTimelineLabel <- ggproto(
  "GeomTimelineLabel",
  Geom,
  required_aes =  c("x", "label"),
  default_aes = aes(
    shape = 19,
    colour = "black",
    size = .5,
    fill = NA,
    alpha = .5,
    stroke = 0,
    y = 1
  ),
  # draw_key = draw_key_text,
  draw_panel = function(data, panel_params, coord, n_max) {
        force_scale<-if(all(data$y==1)){T} else {F}

        coord <- coord$transform(data, panel_params)
        coord$y<-if(force_scale) coord$y*0.5 else coord$y


    coord <- coord[order(coord$size, decreasing = T)[1:n_max], ]
    seg_grob <-
      (
        grid::segmentsGrob(
          coord$x,
          coord$y,
          coord$x,
          coord$y + max(coord$y) * .1,
          default.units = "native",
          gp = grid::gpar(
            col = "grey",
            lwd = 1,
            lty = 1,
            lineend = "butt",
            linejoin = "round"
          )
        )
      )
    text_grob <- grid::textGrob(
      coord$label,
      coord$x,
      coord$y + max(coord$y) * .1,
      default.units = "native",
      hjust = 0,
      vjust = 0.5,
      rot = 45,
      gp = grid::gpar(
        col = "black",
        fontsize = 5,
        fontfamily = "",
        fontface = 1,
        lineheight = 1.2
      )
    )
    grid::gList(seg_grob, text_grob)
  }
)


#' Geom for adding annotations to earthquake data
#'
#' Build a geom called geom_timeline_label() for adding annotations to the earthquake data.
#' This geom adds a vertical line to each data point with a text annotation (e.g., the location
#' of the earthquake) attached to each line. There should be an option to subset to \code{n_max}
#' number of earthquakes, where we take the \code{n_max} largest (by magnitude) earthquakes.
#'
#' @param mapping Aesthetic mappings created by \code{\link[=aes]{aes()}} or \code{\link[=aes_]{aes_()}}.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data for this layer.
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning. If \code{TRUE}, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If \code{TRUE}, inherits aesthetics from the plot default,
#' which is set with \code{\link[=theme_set]{theme_set()}}. If \code{FALSE}, overrides all aesthetics.
#' @param ... Other arguments passed on to \code{\link[=layer]{layer}}.
#' These are often aesthetics, used to set an aesthetic to a fixed value, like \code{color = "red"} or \code{size = 3}.
#' They may also be parameters to the paired geom/stat.
#' @importFrom ggplot2 layer
#' @export
geom_timelinelabel <- function(mapping = NULL,
                               data = NULL,
                               stat = "identity",
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel,
    data = data,
    stat = stat,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Visualize Earthquake Epicenters on Leaflet Map
#'
#' This function takes a filtered data frame with earthquakes to visualize.
#' It maps the epicenters (latitude/longitude) and annotates each point
#' with annotation data stored in a column of the data frame. The user
#' can choose which column is used for the annotation in the pop-up with
#' the \code{annot_col} argument. Each earthquake is shown with a circle,
#' and the radius of the circle is proportional to the earthquake's magnitude.
#'
#' @param data A filtered data frame containing earthquakes to visualize.
#' @param annot_col A character string specifying the column name containing annotation data.
#' @param margin_plot A numeric value specifying the margin percentage for fitBounds.
#' @return A leaflet map displaying earthquake epicenters.
#' @importFrom leaflet leaflet addTiles fitBounds addCircleMarkers
#' @examples
#' \dontrun{
#' #' eq_data <- filter(data, country == "MEXICO" & lubridate::year(date) >= 2000)
#' eq_map(eq_data, annot_col = "date")
#' }
#' @export
#'
eq_map <- function(data, annot_col = "date", margin_plot = 0.01) {
  leaflet::leaflet(data) %>%
    leaflet::addTiles() %>%
    leaflet::fitBounds(
      ~min(longitude, na.rm = TRUE) * (1 - margin_plot),
      ~min(latitude, na.rm = TRUE) * (1 - margin_plot),
      ~max(longitude, na.rm = TRUE) * (1 + margin_plot),
      ~max(latitude, na.rm = TRUE) * (1 + margin_plot)
    ) %>%
    leaflet::addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = ~magnitude,
      stroke = FALSE,
      fillOpacity = 0.4,
      popup = ~get(annot_col)
    )
}

#' Create HTML Labels for Leaflet Map Annotations
#'
#' This function takes a dataset as an argument and creates an HTML label that can be
#' used as the annotation text in a leaflet map. The function puts together a character
#' string for each earthquake that will show the cleaned location, the magnitude, and
#' the total number of deaths, with boldface labels for each ("Location", "Total deaths",
#' and "Magnitude").
#'
#' @param data A dataset containing earthquake information.
#' @return A character vector with HTML labels for each earthquake.
#'
#' @importFrom dplyr mutate pull
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' data <- read.csv("earthquake_data.csv")
#' eq_create_label(data)
#' }
#'
#' @export
eq_create_label <- function(data) {
  data %>%
    dplyr::mutate(annt = paste0("<b>Location: </b>", .data$location, "<br><b> Magnitude: </b>", .data$magnitude, "<br><b>Total deaths: </b>", .data$no_deaths)) %>%
    dplyr::pull(.data$annt)
}



