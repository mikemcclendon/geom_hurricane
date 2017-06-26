
library(dplyr)
#' make_ext_tracks
#'
#'This is a function that reads in Extended Best Tract hurricane data and tidies 
#'it into a format to use for visualization
#'
#' @params NULL
#'
#' @return Returns a tidied data frame object
#' 
#' @importFrom readr read_fwf, fwf_widths
#' @importFrom dplyr mutate, gather, unite, %>%, spread, gather
#' @importFrom lubridate ymd_h
#' 
#' @export
#'
#' @examples make_ext_tracks()

make_ext_tracks <- function() {
  ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                         4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
  ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                           "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "pressure_2",
                           paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                           paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                           "storm_type", "distance_to_land", "final")
  
  ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt", 
                         readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99")

  ext_tracks <- ext_tracks %>%
    dplyr::mutate(storm_id = paste(storm_name, year, sep = "-")) %>%
    tidyr::unite(datetime, year, month, day, hour) %>%
    dplyr::mutate(date = lubridate::ymd_h(datetime)) %>%
    dplyr::mutate(longitude=longitude * -1) %>%
    dplyr::select(storm_id, date, latitude, longitude, dplyr::starts_with("radius")) %>%
    tidyr::gather(key, wind, radius_34_ne:radius_64_nw, na.rm = TRUE) %>%
    tidyr::separate(key, c("dump","wind_speed", "quad"), sep="_") %>%
    tidyr::unite(key, dump, wind_speed, remove=FALSE) %>%
    tidyr::spread(quad, wind) %>%
    dplyr::mutate(wind_speed = as.factor(wind_speed)) %>%
    dplyr::select(-dump, -key) 
}
    

#' get_storm_observation
#' 
#' Function to return a single tidied storm observation
#'
#' @param data ext_tracks data frame
#' @param name name of storm passed as string
#' @param dt date of observation 
#' @param tm time of observation
#'
#' @return data frame containing observation of a single storm
#' 
#' @importFrom dplyr %>%
#' @importFrom lubridate ymd_h
#' 
#' @export
#'
#' @examples get_storm_observation()

get_storm_observation <- function(data = make_ext_tracks(), name="IKE-2008", dt="2008-09-13", tm="12:00:00") {
  dt <- lubridate::ymd_hms(paste(dt, tm))
  data %>% 
    dplyr::filter(storm_id == name & date == dt)}

#' hurrican_proto
#' 
#' Function to create a hurricane ggproto object
#'
#' @param required_aes required aesthetic arguments 
#' @param default_aes default aesthetic values 
#' @param draw_key  function to draw the legend 
#' @param draw_group constructing the geom
#'
#' @importFrom dplyr, dplyr, grid, 
#' 
#' @export
#'
#' @examples geom_hurricane(data = storm_observation,
#' ggplot2::aes(x = longitude, y = latitude, 
#'             r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
#'             fill = wind_speed, color = wind_speed, scale_radii = 1)) + 
#'  ggplot2::scale_color_manual(name = "Wind speed (kts)", 
#'                              values = c("red", "orange", "yellow")) + 
#'  ggplot2::scale_fill_manual(name = "Wind speed (kts)", 
#'                             values = c("red", "orange", "yellow"))



hurricane_proto <- ggplot2::ggproto("hurricane_proto", ggplot2::Geom, required_aes = c("x", "y","r_ne", "r_se", "r_nw", "r_sw"),
                           default_aes = ggplot2::aes(fill="red", colour="red", size=0.5, linetype=1, alpha=.5, arc_step=1, 
                                             scale_radii=1),
                           draw_key = ggplot2::draw_key_polygon,
                           draw_group = function(dat, panel_scales, coord) {
                             #Correcting for nautical miles to meters
                             vars <- names(dat)[5:8]
                             dat <- dat %>% dplyr::mutate_at(vars, funs(. * scale_radii * 1852))
                             #Creating vertices in each quadrant
                             apply(dat, 1, function(i) {
                               nw <- data.frame(geosphere::destPoint(p = c(i["x"], i["y"]),b = 271:360, d = i["r_nw"]))
                               ne <- data.frame(geosphere::destPoint(p = c(i["x"], i["y"]), b = 1:90,d = i["r_ne"]))
                               se <- data.frame(geosphere::destPoint(p = c(i["x"], i["y"]), b = 91:180, d = i["r_se"]))
                               sw <- data.frame(geosphere::destPoint(p = c(i["x"], i["y"]), b = 181:270, d = i["r_sw"]))
                               #Filling in AES for each quadrant 
                               specs <- data.frame(colour = rep(i[["colour"]], times = 360), fill = rep(i[["fill"]], times = 360),
                                                   alpha = rep(i[["alpha"]], times = 360), group = rep(i[["group"]], times = 360), 
                                                   PANEL = rep(i[["PANEL"]], times = 360))
                               specs[,1:2] <- apply(specs[,1:2], 1, as.character)
                               vertices <- bind_rows(nw, ne, se, sw) 
                               vertices <- cbind(vertices, specs)
                               #Renaming from ggproto required names
                               vertices <- vertices %>% rename('x' = 'lon', 'y' = 'lat')
                               #Correcting format of AES
                               vertices$alpha <- as.character(vertices[,'alpha'])
                               #bad habit below
                               coords <<- coord$transform(vertices, panel_scales)
                             })
                             grid::polygonGrob(
                               x= coords$x,
                               y = coords$y,
                               gp = grid::gpar(col = coords$colour, fill = coords$fill, 
                                         alpha = coords$alpha)
                             )
                           }
                           
)


#' geom_hurricane
#' 
#' A function to create a hurricane geom layer
#'
#' @param mapping aesthetic mappings
#' @param data data to be displayed in this layer
#' @param stat The statistical transformation to use on the data for this layer, as a string
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' @param show.legend logical. Include legend?
#' @param inherit.aes if FALSE, overrides the default aesthetics, rather than combining with them. 
#'
#' @importFrom ggplot2
#'
#' @return ggproto layer
#' @export
#'
#' @examples geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude) 
  
geom_hurricane <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(geom = hurricane_proto, mapping = mapping,data = data, stat = stat, 
        position = position,show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    
  )
}

#' hurricane_plot
#' 
#' Returns plot of hurricane
#' 
#' @param storm_observation A tidied observation of a single storm at a single time containing long, lat, and windspeed data
#'
#' @return ggplot of hurricane
#' 
#' @importFrom ggmap ggmap, get_map
#' @importFrom ggplot2 aes, scale_color, scale_fill_manual 
#' 
#' @export
#'
#' @examples plot(hurricane_plot)

#modified from provided code TO DEMONSTRATE WORKING SCALE_RADII
hurricane_plot <- ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap::ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 ggplot2::aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed, scale_radii = 1)) + 
  ggplot2::scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  ggplot2::scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))

