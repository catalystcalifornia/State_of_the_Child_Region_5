# get population data for entire AV (including areas beyond Luz Wilerson boundaries)

library(dplyr)
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)

# postgres credentials
source("W:\\RDA Team\\R\\credentials_source.R") 


# download la spas to W drive from: 
# https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view
lac_spas <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Shapes/Service_Planning_Areas__2022_.shp", quiet=TRUE)

av <- lac_spas %>% filter (SPA_NAME == "Antelope Valley") # filter for SPA 1 to serve as AV

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

lac_pop <- lac_pop %>% filter(NAME == "Los Angeles County, California") 
lac_pop <- lac_pop$estimate[1]


################################# 2011 ################################

# download 2011 spas to W drive from:
# https://egis-lacounty.hub.arcgis.com/datasets/lacounty::la-county-service-planning-area-2012
lac_spas_2011 <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Shapes/LA_County_Service_Planning_Area.shp", quiet=TRUE)
av_2011 <- lac_spas %>% filter (SPA_NAME == "Antelope Valley")


av_2011 <- st_transform(av_2011, crs = 3310) #convert crs

#get 2011 av pop as tracts within SPA 1
av_pop_2011 <- get_acs(geography = "tract",
                       state = "CA",
                       county = "Los Angeles",
                       variables = "B01001_001",
                       year = 2011,
                       geometry = TRUE)

av_pop_2011 <- st_transform(av_pop_2011, crs = 3310) #convert crs


#calculate spatial intersection of tracts in av (SPA 1)
av_pop_2011 <- st_intersection(av_2011, av_pop_2011)
#plot(av_pop_2011$geometry)

#sum population of av tracts
av_population_2011 <- sum(av_pop_2011$estimate)


#get LA County pop
lac_pop_2011 <- get_acs(geography = "county",
                        state = "CA",
                        variables = "B01001_001",
                        year = 2011) 

lac_pop_2011 <- lac_pop_2011 %>% filter(NAME == "Los Angeles County, California") 
lac_pop_2011 <- lac_pop_2011$estimate[1]


############################ DIF ##############################################

av_pct_change <- (av_population - av_population_2011) / av_population * 100
la_pct_change <- (lac_pop - lac_pop_2011) / lac_pop * 100 

