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

#read.csv opens csv files
eyes_hurt_map_state <- read.csv("eyes_hurt_map_state.csv")
eyes_hurt_timeline <- read.csv("eyes_hurt_timeline.csv")

#st_read opens spatial files
upath_lo <- st_read("upath_lo.kml")

#Used "rename" to make column names in both csv files easier to use and join later
eyes_hurt_timeline <- eyes_hurt_timeline %>%
  rename(Searches = eyes.hurt...United.States.,
         Date = Day)
eyes_hurt_map_state <- eyes_hurt_map_state %>%
  rename(Searches = eyes.hurt...4.7.24...4.12.24.,
         NAME = Region)

#used the "states" function with the tigris package to get the polygon data of US states and saved it as an object
us_states_sf <- states()

#used filter to get only the contiguous US; "!" tells the filter that I do not want these places included in the data
us_states_sf <- us_states_sf %>%
  filter(!NAME %in% c("Guam", "Hawaii", "Alaska",
                      "Commonwealth of the Northern Mariana Islands",
                      "United States Virgin Islands", "American Samoa", "Puerto Rico"))

#I used "select" to only include the geometry and name columns in the US states data
us_states <- select(us_states_sf, geometry, NAME)

#Joined the US states data with a left_join to keep the data from the US states file and include the "searches" column; Joined by the "NAME" column
joined_state_search <- us_states %>% left_join(eyes_hurt_map_state, by = join_by(NAME))

#I used st_crs to check the coordinate systems of the joined file and the kml file
st_crs(joined_state_search)
st_crs(upath_lo)
#Used st_transform to change the kml's coordinate system to the joined file's coordinate system
upath_lo <- st_transform(upath_lo, 4269)
#Checked the kml crs again to check that st_transform worked
st_crs(upath_lo)
#Used st_crop to crop the kml file to only show data within the bounds of the us_states file
upath_lo <- st_crop(upath_lo, us_states)

#Used mutate to add columns for distance; Used st_distance to find distance between coordinates in the joined_state_search file and the coordinates in the kml file; Added an ifelse statement to create a column that shows the eclipse path interesected a given state (value = 0 = intersects)
joined_state_search <- joined_state_search %>%
  mutate(dist_m = st_distance(joined_state_search, upath_lo),
         dist_km = as.numeric((dist_m)/1000),
         Intersect = ifelse(dist_km == 0, dist_km, NA_real_))

#Used geom_sf to map the search intensity and eclipse path; Used scale_fill_distiller to change and invert the search intensity color palette; Added labels; Added arguments to move the legend to the bottom and to get rid of the graph background; Used scale_color_manual to remove the legend label for the eclipse path
eclipse_search_map <- ggplot() + geom_sf(data = joined_state_search, aes(fill = Searches)) + geom_sf(data = upath_lo, fill = NA, aes(color = "red"), size = 1) + scale_fill_distiller(palette = "Blues", direction = 1) + labs(title = "Google Trends for 'eyes hurt' Following the 2024 Solar Eclipse", subtitle = "Search intensity by state (Week of April 3-10, 2024)", x = "Longitude", y = "Latitude", colour = "Eclipse Path Outline") + theme(legend.position = "bottom", panel.background = element_blank()) + scale_color_manual(labels = NULL, values = "red")

#Used coord_sf to create the map projection
eclipse_search_map + coord_sf()

#Filtered eyes_hurt_timeline to create an eclipse_day point
eclipse_day <- eyes_hurt_timeline %>%
  filter(Date == "4/8/2024")

#Used geom_line to create a line graph for search trends over time; Added geom_vline to add a vertical line showing the trend on eclipse day (used linetype and linewidth to change the dash and size of the line); Used annotate to label the vertical line by specifying what point the label should be at; Added theme arguments to make the x-axis labels vertical since I could not figure out how to sort the data by month and have it on the graph properly, remove the graph background, change the gridline and border colors, and move the legend 
search_over_time_plot <- ggplot() + geom_line(data = eyes_hurt_timeline, aes(x = Date, y = Searches, group = 1), linewidth = 1, color = "darkblue") + geom_vline(data = eclipse_day, aes(xintercept = Date), linewidth = 1, linetype = "dashed", color = "red") + labs(title = "Google Search Trends For 'eyes hurt' In the US", subtitle = "March 1st - May 31st, 2024", x = "Date", y = "Relative Search Interest (0-100)") + annotate("text", x = "4/8/2024", y = 100, label = "Solar Eclipse (April 8, 2024)") + theme(axis.text.x = element_text(angle = 90), panel.background = element_blank(), panel.grid = element_line(color = "lightgray"), legend.position = "bottom", panel.border = element_rect(color = "black")) + coord_fixed(ratio = 0.2)

search_over_time_plot           

#ggarange will show multiple graphs on the same panel
ggarrange(eclipse_search_map, search_over_time_plot)
