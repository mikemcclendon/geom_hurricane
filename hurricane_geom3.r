library(readr)
library(dplyr)
library(lubridate)
library(chron)
library(tidyr)
library(ggmap)
library(stringr)
library(geosphere)
library(ggplot2)
library(grid)

#' make_ext_tracks
#'
#'This is a function that reads in Extended Best Tract hurricane data and tidies 
#'it into a format to use for visualization
#'
#' @return Returns a tidied data frame object
#' 
#' @importFrom readr, dplyr, lubridate
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
#' @importFrom dplyr, lubridate
#' 
#' @export
#'
#' @examples get_storm_observation()

get_storm_observation <- function(data = make_ext_tracks(), name="IKE-2008", dt="2008-09-13", tm="12:00:00") {
  dt <- ymd_hms(paste(dt, tm))
  data %>% 
    filter(storm_id == name & date == dt)}

#' hurricane_proto
#' 
#' Constructs a hurricane ggproto object
#'
#' @return ggproto object
#' 
#' @importFrom ggplot2, grid 
#' 
#' @export
#'
#' @examples hurricane_proto



hurricane_proto <- ggproto("hurricane_proto", Geom, required_aes = c("x", "y",
                                                                     "r_ne", "r_se", "r_nw", "r_sw"),
                           default_aes = aes(fill="red",
                                             colour="red",
                                             size=0.5,
                                             linetype=1,
                                             alpha=.5,
                                             arc_step=1,
                                             scale_radii=1),
                                         draw_key = draw_key_polygon,
                                         draw_group = function(dat, panel_scales, coord) {
                                           #Correcting for nautical miles to meters
                                           nauticalConv <- 1852
                                           dat <- dat %>% mutate_(r_ne = ~r_ne * scale_radii * nauticalConv,
                                                                    r_se = ~r_se * scale_radii * nauticalConv,
                                                                    r_sw = ~r_sw * scale_radii * nauticalConv,
                                                                    r_nw = ~r_nw * scale_radii * nauticalConv)
                                           #Making alpha available to grob
                                           #dat$alpha <- as.numeric(default_aes$alpha)
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
                                           polygonGrob(
                                             x= coords$x,
                                             y = coords$y,
                                             gp = gpar(col = coords$colour, fill = coords$fill, 
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
#' @param show.legend logical. Should this layer be included in the legends?
#' @param inherit.aes f FALSE, overrides the default aesthetics, rather than combining with them. 
#' @param ... 
#'
#' @return ggproto layer
#' @export
#'
#' @examples geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude) 
  
geom_hurricane <- function(mapping = NULL, data = NULL, stat = 'identity',
                           position = 'identity', na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  layer(geom = hurricane_proto, mapping = mapping,data = data, stat = stat, 
        position = position,show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    
  )
}

#' hurricane_plot
#' 
#' Returns plot of hurricane
#'
#' @return ggplot of hurricane
#' 
#' @importFrom ggmap, ggplot
#' 
#' @export
#'
#' @examples plot(hurricane_plot)


#modified from provided code
hurricane_plot <- ggmap::get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = storm_observation,
                 aes(x = longitude, y = latitude, 
                     r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind speed (kts)", 
                     values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind speed (kts)", 
                    values = c("red", "orange", "yellow"))

