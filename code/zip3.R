# Load necessary libraries
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
library(ggforce)  # for geom_circle()
options(tigris_use_cache = TRUE)

# Set your Census API key (register at https://api.census.gov/data/key_signup.html)
# Replace with your key or use tidycensus::census_api_key() to store it for future sessions
# census_api_key("YOUR_CENSUS_API_KEY", install = TRUE)
census_api_key(Sys.getenv("CENSUS_API_KEY"))


# Download U.S. state boundaries
states_sf <- states(cb = TRUE, year = 2020) %>%
  st_transform(5070)  # use the same CRS as your ZIP3 data

# Lower 48 FIPS codes to exclude AK, HI, PR, and other territories
excluded_states <- c("02", "15", "72", "60", "66", "69", "78")  # AK, HI, PR, territories

# Filter to Lower 48 states
lower48_sf <- states_sf %>%
  filter(!STATEFP %in% excluded_states)

# Union into a single geometry
lower48_union <- st_union(lower48_sf)

# Get ZIP code shapefiles
zcta_sf <- tigris::zctas(year = 2020)

# Get population data from the ACS
pop_data <- tidycensus::get_acs(
  geography = "zcta",
  variables = "B01003_001",  # total population
  year = 2021,
  geometry = FALSE
)

# Merge population with shapefiles
zcta_sf <- zcta_sf %>%
  left_join(pop_data, by = c("ZCTA5CE20" = "GEOID")) %>%
  filter(!is.na(estimate))

# Create 3-digit ZIP codes
zcta_sf <- zcta_sf %>%
  mutate(zip3 = substr(ZCTA5CE20, 1, 3))

# Compute centroids of each 5-digit ZIP (ZCTA)
zcta_sf_centroids <- zcta_sf %>%
  mutate(
    centroid = st_centroid(geometry),
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  ) %>% 
  st_drop_geometry()

# Aggregate to 3-digit ZIP using population-weighted centroids
zip3_df <- zcta_sf_centroids %>%
  group_by(zip3) %>%
  summarise(
    population = sum(estimate, na.rm = TRUE),
    lon = weighted.mean(lon, estimate, na.rm = TRUE),
    lat = weighted.mean(lat, estimate, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    radius = sqrt(population / pi) / 1000  # scale radius for visualization
  )

# Convert lon/lat to sf POINT geometries
zip3_points <- zip3_df %>%
  filter(!is.na(lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # WGS84
  st_transform(crs = 5070)  # Use Albers Equal Area for buffering in meters

# Ensure zip3_points is in the same CRS
zip3_points <- st_transform(zip3_points, 5070)

# Logical vector of which centroids fall within the Lower 48
inside <- st_within(zip3_points, lower48_union, sparse = FALSE)[,1]

# Keep only those ZIP3s
zip3_points_filtered <- zip3_points[inside, ]

# Create actual circular buffers around each centroid
zip3_circles <- zip3_points_filtered %>%
  mutate(geometry = st_buffer(geometry, dist = sqrt(radius) * 40000))  # radius in km

# Plot using geom_sf with real circular geometries
fig_centroids <- ggplot() +
  # geom_sf(data = st_union(zcta_sf), fill = "grey95", color = "white") +
  # geom_sf(data = zip3_points, aes(size=population), color = "black", alpha = 0.6) +
  geom_sf(data=lower48_sf, fill=NA) + 
  geom_sf(data = zip3_circles, color = "black", alpha = 0.6) +
  scale_fill_viridis_c(option = "plasma", trans = "log10") +
  coord_sf(crs = 5070) +
  theme_minimal() +
  labs(
    title = "US Population by 3-Digit ZIP Code",
    subtitle = "Circle area proportional to population size",
    fill = "Population"
  ) + 
  theme_void() 