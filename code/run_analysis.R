library(renv)
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(units) 
options(tigris_use_cache = TRUE)
census_api_key(Sys.getenv("CENSUS_API_KEY"))

source('code/utils.R') 

state_boundaries <- get_state_boundaries() 

# zip3_centroids <- get_zip3_centroids()
# st_write(zip3_centroids, "data/zip3_centroids.shp")
zip3_centroids <- st_as_sf(st_read("data/zip3_centroids.shp"))
zip3_circles <- zip3_centroids %>%
	mutate(radius = sqrt(population / pi) / 1000) %>% 
	mutate(geometry = st_buffer(geometry, dist = sqrt(radius) * 40000))

 # Plot using geom_sf with real circular geometries
fig_centroids <- ggplot() +
  geom_sf(data = state_boundaries, fill=NA) + 
  geom_sf(data = zip3_circles, color = "black", alpha = 0.6) +
  coord_sf(crs = 5070) +
  theme_void() 

distmat <- set_units(st_distance(zip3_centroids),"km")

