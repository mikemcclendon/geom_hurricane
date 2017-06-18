library(readr)
library(dplyr)
library(lubridate)
library(chron)
library(tidyr)
library(ggmap)
library(stringr)
library(geosphere)
library(ggplot2)

#' make_ext_tracks
#'
#'This is a function that reads in Extended Best Tract hurricane data and tidies 
#'it into a format to use for visualization
#'
#' @return Returns a tidied data frame object
#' @export
#'
#' @examples

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
  
  ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                         fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                         na = "-99")

  ext_tracks <- ext_tracks %>%
    mutate(storm_id = paste(storm_name, year, sep = "-")) %>%
    unite(datetime, year, month, day, hour) %>%
    mutate(date = ymd_h(datetime)) %>%
    mutate(longitude=longitude * -1) %>%
    select(storm_id, date, latitude, longitude, starts_with("radius")) %>%
    gather(key, wind, radius_34_ne:radius_64_nw, na.rm = TRUE) %>%
    separate(key, c("dump","wind_speed", "quad"), sep="_") %>%
    unite(key, dump, wind_speed, remove=FALSE) %>%
    spread(quad, wind) %>%
    mutate(wind_speed = as.factor(wind_speed)) %>%
    select(-dump, -key) 
}
    

#' Title
#'
#' @param data 
#' @param name 
#' @param dt 
#' @param tm 
#'
#' @return
#' @export
#'
#' @examples
get_storm_observation <- function(ext, name="IKE-2008", dt="2008-09-13", tm="12:00:00") {
  dt <- ymd_hms(paste(dt, tm))
  ext %>% 
    filter(storm_id == name & date == dt)}



geom_hurricane_proto <- ggplot2::ggproto("geom_hurricane_proto", Geom,
                                         required_aes = c("x", "y",
                                                          "r_ne", "r_se", "r_nw", "r_sw"
                                         ),
                                         default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1),
                                         draw_key = draw_key_polygon,
                                         draw_group = function(data, panel_scales, coord) {
                                           
                                           ## Transform the data first
                                           coords <- coord$transform(data, panel_scales)
                                           
                                           # Convert nautical miles to meters and multiply by scale factor
                                           data <- data %>% mutate_(r_ne = ~r_ne*1852*scale_radii,
                                                                    r_se = ~r_se*1852*scale_radii,
                                                                    r_sw = ~r_sw*1852*scale_radii,
                                                                    r_nw = ~r_nw*1852*scale_radii
                                           )
                                           
                                           
                                           # Loop over the data and create the points for each quandrant
                                           for (i in 1:nrow(data)) {
                                             
                                             # Create the Northwest Quandrant
                                             df_nw <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 270:360,
                                                                                            d = data[i,]$r_nw),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # Create the Northeast Quandrant
                                             df_ne <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 1:90,
                                                                                            d = data[i,]$r_ne),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # Create the Southeast Quandrant
                                             df_se <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 90:180,
                                                                                            d = data[i,]$r_se),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # Create the Southwest Quandrant
                                             df_sw <- data.frame(colour = data[i,]$colour,
                                                                 fill = data[i,]$fill,
                                                                 geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                      b = 180:270,
                                                                                      d = data[i,]$r_sw),
                                                                 group = data[i,]$group,
                                                                 PANEL = data[i,]$PANEL,
                                                                 alpha = data[i,]$alpha
                                             )
                                             
                                             # bind all the rows into a dataframe
                                             df_points <- dplyr::bind_rows(list(df_nw, df_ne, df_se, df_sw))
                                             
                                           }
                                           
                                           
                                           # Rename columns x and y from lon and lat repectively
                                           df_points <- df_points %>% dplyr::rename_('x' = 'lon',
                                                                                     'y' = 'lat'
                                           )
                                           
                                           # Convert to character
                                           df_points$colour <- base::as.character(df_points$colour)
                                           df_points$fill <- base::as.character(df_points$fill)
                                           
                                           
                                           ## transform data points
                                           coords_df <- coord$transform(df_points, panel_scales)
                                           
                                           ## Construct grid polygon
                                           grid::polygonGrob(
                                             x= coords_df$x,
                                             y = coords_df$y,
                                             gp = grid::gpar(col = coords_df$colour, fill = coords_df$fill, alpha = coords_df$alpha)
                                           )
                                           
                                         }
                                         
)



#' Title
#'
#' @param mapping 
#' @param data 
#' @param stat 
#' @param position 
#' @param na.rm 
#' @param show.legend 
#' @param inherit.aes 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
geom_hurricane <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geom_hurricane_proto, mapping = mapping,
    data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
    
  )
}



map_ike <- get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))
