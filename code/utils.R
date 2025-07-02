library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
options(tigris_use_cache = TRUE)
census_api_key(Sys.getenv("CENSUS_API_KEY"))

get_state_boundaries <- function(internal=TRUE){

	# Download U.S. state boundaries
	states_sf <- states(cb = TRUE, year = 2020) %>%
	  st_transform(5070)  # use the same CRS as your ZIP3 data

	# Lower 48 FIPS codes to exclude AK, HI, PR, and other territories
	excluded_states <- c("02", "15", "72", "60", "66", "69", "78")  # AK, HI, PR, territories

	# Filter to Lower 48 states
	lower48_sf <- states_sf %>%
	  filter(!STATEFP %in% excluded_states) %>% 
	  select(GEOID, STUSPS, NAME)

	# Union into a single geometry
	lower48_union <- st_union(lower48_sf)

	if(internal){
		return(lower48_sf)
	} else {
		return(lower48_union)
	}
}

get_zip3_centroids <- function(){
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
	  ungroup()

	# Convert lon/lat to sf POINT geometries
	zip3_points <- zip3_df %>%
	  filter(!is.na(lon)) %>% 
	  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%  # WGS84
	  st_transform(crs = 5070)  # Use Albers Equal Area for buffering in meters

	# Ensure zip3_points is in the same CRS
	zip3_points <- st_transform(zip3_points, 5070)

	# Logical vector of which centroids fall within the Lower 48
	lower48_union <- get_state_boundaries(internal=FALSE)
	inside <- st_within(zip3_points, lower48_union, sparse = FALSE)[,1]

	# Keep only those ZIP3s
	zip3_points_filtered <- zip3_points[inside, ]

	return(zip3_points_filtered)
}


get_foi <- function(infvec, popvec, distmat, b0=0, bd=0.77, mu=0.23, rho=96){

	distmat <- drop_units(distmat)
	expvec <- colSums(exp(-distmat/rho))
	foi <- b0 + bd*(popvec^mu)*(sum(expvec[infvec]))/(sum(expvec)-1)
	return(foi)

}


simulate_outbreak <- function(seed, geodf, b0=0, bd=0.77, mu=0.23, rho=96){

	distmat <- drop_units(set_units(st_distance(geodf),"km"))
	popvec <- geodf$population / mean(geodf$population)
	denominator <- colSums(exp(-distmat/rho)) - 1

	infvec <- c(seed)
	tvec <- c(0)

	for(t in 1:100){

		if(length(infvec)>1){
			numerator <- colSums(exp(-distmat[infvec,]/rho))
		} else {
			numerator <- exp(-distmat[infvec,]/rho)
		}

		foi <- b0 + bd*(popvec^mu)*numerator/denominator
		pinf <- 1 - exp(-foi)
		draw <- runif(length(pinf))

		newinf <- setdiff(which(draw < pinf), infvec)
		infvec <- c(infvec, newinf)
		tvec <- c(tvec, rep(t, length(newinf)))

	}

	out_df <- tibble(t=tvec, loc=infvec)
	return(out_df)

}








