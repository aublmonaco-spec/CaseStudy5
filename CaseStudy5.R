#install.packages("tigris")
#install.packages("sf")
#install.packages("spData")
#install.packages("ggpubr")
library(RColorBrewer)
library(tigris)
library(tidyverse)
library(readr)
library(sf)
library(dplyr)
library(spData)
library(ggpubr)
setwd("C:/Users/aublm/OneDrive/GEO511/CaseStudy5")

eyes_hurt_map_state <- read.csv("eyes_hurt_map_state.csv")
eyes_hurt_timeline <- read.csv("eyes_hurt_timeline.csv")

upath_lo <- st_read("upath_lo.kml")
eyes_hurt_timeline <- eyes_hurt_timeline %>%
  rename(Searches = eyes.hurt...United.States.,
         Date = Day)
eyes_hurt_map_state <- eyes_hurt_map_state %>%
  rename(Searches = eyes.hurt...4.7.24...4.12.24.,
         NAME = Region)

us_states_sf <- states()

us_states_sf <- us_states_sf %>%
  filter(!NAME %in% c("Guam", "Hawaii", "Alaska",
                      "Commonwealth of the Northern Mariana Islands",
                      "United States Virgin Islands", "American Samoa", "Puerto Rico"))

us_states <- select(us_states_sf, geometry)

joined_state_search <- us_states_sf %>% left_join(eyes_hurt_map_state, by = join_by(NAME))

st_crs(joined_state_search)
st_crs(upath_lo)
upath_lo <- st_transform(upath_lo, 4269)
st_crs(upath_lo)
upath_lo <- st_crop(upath_lo, us_states_sf)

joined_state_search <- joined_state_search %>%
  mutate(dist_m = st_distance(joined_state_search, upath_lo),
         dist_km = as.numeric((dist_m)/1000),
         Intersect = ifelse(dist_km == 0, dist_km, NA_real_))

world1 <- world %>%
  filter(continent == "North America")

eclipse_search_map <- ggplot() + geom_sf(data = world1) + geom_sf(data = joined_state_search, aes(fill = Searches)) + geom_sf(data = upath_lo, fill = NA, aes(color = "red")) + scale_fill_distiller(palette = "Blues", direction = 1) + labs(title = "Intersection of Solar Eclipse Path and Search Popularity", x = "Longitude", y = "Latitude", colour = "Eclipse Path Outline") + theme(legend.position = "bottom") + scale_color_manual(labels = NULL, values = "red")
  
eclipse_search_map + coord_sf()

eclipse_day <- eyes_hurt_timeline %>%
  filter(Date == "4/8/2024")

search_over_time_plot <- ggplot() + geom_line(data = eyes_hurt_timeline, aes(x = Date, y = Searches, group = 1), color = "darkblue") + geom_point(data = eclipse_day, aes(x = Date, y = Searches, color = "red")) + theme(axis.text.x = element_text(angle = 90), panel.background = element_blank(), panel.grid = element_line(color = "lightblue"), legend.position = "bottom", panel.border = element_rect(color = "black")) + labs(title = "Search For 'Eyes Hurt' Over Time", x = "Date", y = "Search Frequency", colour = "Solar Eclipse Day (4/8/24)") + scale_color_manual(labels = "100 Searches", values = "red")
search_over_time_plot                                

ggarrange(eclipse_search_map, search_over_time_plot)
