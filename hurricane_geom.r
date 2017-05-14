library(readr)
library(dplyr)
library(lubridate)
library(chron)

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

ext_tracks$date <- as.Date( with(ext_tracks, paste( ext_tracks$year , ext_tracks$month , 
                                   ext_tracks$day, sep = "-" )  
                            , format = "%y.%m.%d" )) 

ext_tracks$hour <- as.Date(paste( ext_tracks$hour, "00", "00", sep = ":"), 
                           format = "%H:%M:%S")
                               
ext_tracks <- within(ext_tracks, { timestamp=format(as.POSIXct(paste(date, hour)), "%Y/%m/%d %H:%M:%S") })
