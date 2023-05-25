# get population data for entire AV (including areas beyond Luz Wilerson boundaries)

# library(dplyr)
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)

# postgres credentials
source("W:\\RDA Team\\R\\credentials_source.R") 


# download la spas to W drive from:
# https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view
lac_spas <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Shapes/Service_Planning_Areas__2022_.shp", quiet=TRUE)

av <- lac_spas %>% dplyr::filter (SPA_NAME == "Antelope Valley") #  filter for SPA 1 to serve as AV

av <- st_transform(av, crs = 3310) #check #st_crs(av) and convert crs


#get av pop as tracts within SPA 1
av_pop <- get_acs(geography = "tract",
                  state = "CA",
                  county = "Los Angeles",
                  variables = "B01001_001",
                  year = 2021,
                  geometry = TRUE)

av_pop <- st_transform(av_pop, crs = 3310) #check st_crs(av_pop) and convert crs


#calculate spatial intersection of tracts in av (SPA 1)
av_pop <- st_intersection(av, av_pop)
#plot(av_pop$geometry)

#sum population of av tracts
av_population <- sum(av_pop$estimate)


#get LA County pop
lac_pop <- get_acs(geography = "county",
                   state = "CA",
                   variables = "B01001_001",
                   year = 2021) 

lac_pop <- lac_pop %>% dplyr::filter(NAME == "Los Angeles County, California") 
lac_pop <- lac_pop$estimate[1]