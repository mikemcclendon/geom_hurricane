library(readr)
library(dplyr)
library(lubridate)
library(chron)
library(tidyr)



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
  #concat storm id
  # storm_id <- ext_tracks %>%
  #   select(storm_name, year) %>%
  #   unite(storm_id, storm_name, year, sep = "-")
  #assign values
  # ext_tracks$storm_id <- unlist(storm_id)
  #correct longitude
  # ext_tracks$longitude <- as.numeric(sapply(ext_tracks$longitude,
  #function(x) {x * -1}))
  # #make wind speeds long
  # #creating the gathers
  # gathers <- names(ext_tracks)[contains(match="radius_", vars=names(ext_tracks))]
  # ext_tracks <- gather_(ext_tracks,key_col="key",
  #                             value_col="wind",gather_cols=gathers,
  #                             na.rm=FALSE,factor_key=FALSE)
  # ext_tracks <- separate_(ext_tracks,col="key",
  #                               into=c("ignore","wind_speed","quad"),
  #                               sep="_",remove=TRUE,convert=TRUE)
  # ext_tracks$ignore <- NULL
  # ext_tracks <- spread(ext_tracks, quad, wind)
  
  #selecting relevant columns
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
    select(-dump) 
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
get_observation <- function(data, name="ALBERTO-1988", dt="1988-08-05", tm="18:00:00") {
  dt <- ymd_hms(paste(dt, tm))
  data %>% filter(storm_id == name & date == dt)}




