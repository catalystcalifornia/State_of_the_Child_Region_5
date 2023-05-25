# Calculate AV and County houseless stats and visualize in a map

#install packages if not already installed
list.of.packages <- c("tidyverse","RPostgreSQL","sf", "leaflet")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Packages ----------------------------------------------------------------
library(RPostgreSQL)
library(sf)
library(leaflet)


# Postgres credentials
source("W:\\RDA Team\\R\\credentials_source.R") 

con <- connect_to_db("rda_shared_data") 

# get census tract shapefile from database previously downloaded from:
# https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2020.html
census_tracts <- st_read(con, query = "SELECT * FROM geographies_ca.cb_2020_06_tract_500k") 

# get homeless data by census tract from database previously downloaded from:
# https://www.lahsa.org/documents?id=6543-hc22-data-by-census-tract-split.xlsx
lahsa_22_split <- dbGetQuery(con, "SELECT * FROM housing.lahsa_homelesscount_tract_2022") 

dbDisconnect(con)


## Summarize LAHSA data by geography ---------------------------------------------------------------------------
lahsa_22_split$tract <- paste0("06037", substr(lahsa_22_split$tract_split, 1, 6))

# Get tract estimates
# Note: totpeople is Total population of adult & youth sheltered and adult unsheltered homeless persons
lahsa_22_tract <- lahsa_22_split %>% group_by(tract) %>% summarise(totpeople = sum(totpeople))
lahsa_22_tract <- mutate(lahsa_22_tract, quantile_rank = ntile(lahsa_22_tract$totpeople,3))
lahsa_22_tract <- lahsa_22_tract %>% rename(ct_geoid = tract)


# Get city estimates
# Note: Lancaster & Palmdale estimates match lahsa's: https://www.lahsa.org/data?id=54-homeless-count-by-city-community
lahsa_22_city <- lahsa_22_split %>% group_by(city) %>% summarise(geography = "city", totpeople = round(sum(totpeople),0))
                                                                 
av_city <- subset(lahsa_22_city, city %in% c("Lancaster", "Palmdale"))
av_city <- av_city %>% rename(name = city)

# Get community estimates
# Note: Lake Los Angeles, North Lancaster, Unincorporated Palmdale, Pearblossom/Llano, Littlerock, Littlerock/Juniper Hills, and
# Littlerock/Pearblossom estimates match lahsa's: https://www.lahsa.org/data?id=54-homeless-count-by-city-community
lahsa_22_community <- lahsa_22_split %>% group_by(community_name) %>% summarise(geography = "community", 
                                                                                totpeople = round(sum(totpeople),0))

#subset community estimates - leaving out "Littlerock", since it has a value of zero
av_community <- subset(lahsa_22_community, community_name %in% c("Lancaster", "North Lancaster", "Palmdale", "Unincorporated Palmdale",
                                                            "Littlerock/Juniper Hills", "Littlerock/Pearblossom", 
                                                            "Pearblossom/Llano", "Lake Los Angeles"))

av_community <- av_community %>% rename(name = community_name)

# Get county estimate
# Note: summing totpeople doesn't yield the correct county figure, so hard coding from 
# https://www.lahsa.org/data?id=53-homeless-count-by-la-county-supervisorial-district-2015-2022 
name <- c("Los Angeles County")
geography <- c("county")
totpeople <- c(65111)
av_county <- data.frame(name, geography, totpeople)

#combine pertinent geographies. Leaving av_city out b/c Lancaster & Palmdale figures appear duplicated in av_community
av_table <- rbind(av_community, av_county) %>% dplyr::select(-geography) %>% rename ("Adult & youth sheltered & adult unsheltered" = totpeople)


## Format and map tract data ---------------------------------------------------------------------------
census_tracts_to_map <- merge(x=census_tracts,y=lahsa_22_tract,by="ct_geoid",all.x=TRUE)
census_tracts_to_map$map_color <- ifelse(census_tracts_to_map$quantile_rank == 3, "#d7301f",
                                     ifelse(census_tracts_to_map$quantile_rank == 2, "#fc8d59",
                                            ifelse(census_tracts_to_map$quantile_rank == 1, "#fdcc8a", "#d3d3d3")))

census_tracts_to_map <- st_transform(census_tracts_to_map, crs = 4326)

map_data <- leaflet(width = "100%", height = "495px") %>%

  #basemap
  addProviderTiles("CartoDB.Positron") %>%

  #setView
  setView(-118.291102, 34.5823153, 9) %>%

  #adding ed rates per census tract
  addPolygons(data = census_tracts_to_map,
              color = "#8C8C8C",
              fillColor= ~map_color,
              smoothFactor = 0.5,
              weight = 1,
              opacity = .75,
              fillOpacity = 0.75,
              popup = ~paste0("<strong> Adult & youth sheltered & adult unsheltered homeless persons: </strong>", round(totpeople, 0)))

map_data



