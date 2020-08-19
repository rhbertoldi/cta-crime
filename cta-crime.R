library(haven)
library(tidyverse)
library(sf)
library(snakecase)
library(lubridate)
library(gapminder)
library(gganimate)

setwd("C:/Users/User02/Desktop/Harris/CTA_Data_Project")

# sfc_as_cols function from  mountainMath/cancensusHelpers
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

railstations <- st_read("CTA_RailStations/CTA_RailStations.shp") %>%
  set_names(to_snake_case(colnames(.))) %>%
  st_set_crs(102671) %>%
  st_transform(2163) %>%
  st_buffer(100) %>%
  select(longname, lines, gtfs, geometry)

crimes <- read_csv("crimes_2001.csv") %>%
  set_names(to_snake_case(colnames(.)))

crimes_fil <- crimes %>%
  filter(
    str_detect(location_description, "CTA") &
      !(str_detect(location_description, "BUS"))
  ) %>%
  filter(!is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326, remove = FALSE) %>%
  st_transform(2163) %>%
  mutate(
    date = as_date(
      date,
      format = "%m/%d/%Y %H:%M:%S %p",
      tz = "America/Chicago"),
    month = month(date),
    year = year(date)
  ) %>%
  select(id, date, primary_type, month, year, latitude, longitude)

stations <- st_read("CTA_RailStations/CTA_RailStations.shp") %>%
  set_names(to_snake_case(colnames(.))) %>%
  st_set_crs(102671) %>%
  st_transform(2163) %>%
  sfc_as_cols(names = c("longitude", "latitude")) %>%
  select(gtfs, longitude, latitude) %>%
  st_set_geometry(NULL)

crimes_per_stop <- st_join(crimes_fil, railstations, join = st_within) %>%
  filter(!is.na(longname)) %>%
  st_set_geometry(NULL) %>%
  mutate(new_date = as_date(paste0(month(date),"/01/",year(date)),
                            format = "%m/%d/%Y", tz = "UTC")) %>%
  group_by(gtfs, new_date) %>%
  summarise(crime_count = n()) %>%
  left_join(stations)

line_color <- railstations %>%
  select(lines, gtfs) %>%
  st_set_geometry(NULL)

rail_lines <- st_read("CTA_RailLines/CTA_RailLines.shp") %>%
  set_names(to_snake_case(colnames(.))) %>%
  st_transform(2163)

p <- crimes_per_stop %>%
  left_join(line_color, by = "gtfs") %>%
  ggplot() +
  geom_sf(data = rail_lines, aes(color = lines), alpha = .5) +
  geom_point(aes(x = longitude, y = latitude,
                 size = crime_count, group = gtfs, color = lines)) +
  scale_color_manual(values = c("Green Line" = "green",
                                "Red Line" = "red",
                                "Yellow Line" = "yellow",
                                "Brown Line" = "brown",
                                "Green Line (Lake)" = "green",
                                "Blue Line (O'Hare)" = "blue",
                                "Orange Line" = "orange",
                                "Purple Line, Evanston Express" = "purple",
                                "Red, Brown, Purple (Express)" = "red",
                                "Green Line (Englewood)" = "green",
                                "Brown, Orange, Pink, Putple (Express), Green, Blue" = "brown",
                                "Brown, Purple (Express)" = "brown",
                                "Blue Line" = "blue",
                                "Brown, Orange, Pink, Purple (Express)" = "brown",
                                "Green (Lake), Pink" = "green",
                                "Green (Lake), Pink" = "green",
                                "Blue Line (Congress)" = "blue",
                                "Pink" = 'pink',
                                "Brown, Orange, Pink, Purple (Express), Green" = "brown",
                                "Red, Yellow, Purple, Evanston Express" = "red",
                                "Orange & Green Lines" = "orange",
                                "Blue Line (Forest Park)" = "blue",
                                "Brown, Purple (Express), Red" = "red",
                                "Brown, Green, Orange, Pink, Purple (Exp)" = "brown",
                                "Brown, Orange, Pink, Purple (Express)" = "brown",
                                "Green, Orange" = "green",
                                "Green, Pink" = "green",
                                "Purple Line" = "purple",
                                "Brown, Purple" = "brown",
                                "Pink Line" = "pink",
                                "Red, Purple Line" = "red")) +
  transition_components(new_date) +
  labs(title = 'CTA Stop Crime Counts (2001 - 2018)',
       subtitle = '{frame_time}') +
  theme(
    legend.position="none",
    line = element_blank(),
    rect = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid.major = element_line(colour = "transparent")
  )

animate(p, duration = 20)
anim_save("cta-crime.gif", animation = last_animation())
