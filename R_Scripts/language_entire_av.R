# get language data for entire av

library(dplyr)
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)

# ACS credentials
source("W:\\RDA Team\\R\\credentials_source.R") 

# download la spas to W drive from: 
# https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view 
lac_spas <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Shapes/Service_Planning_Areas__2022_.shp", quiet=TRUE)

av <- lac_spas %>% filter (SPA_NAME == "Antelope Valley") # filter for SPA 1 to serve as AV

av <- st_transform(av, crs = 3310) #check #st_crs(av) and convert crs


#get av language as tracts within SPA 1
av_language <- get_acs(geography = "tract",
                      state = "CA",
                      county = "Los Angeles",
                      variables = c(pop_5plus = "DP02_0112",
                                    english = "DP02_0113",
                                    spanish = "DP02_0116",
                                    indo_european = "DP02_0118",
                                    api = "DP02_0120"),
                      year = 2021,
                      geometry = TRUE)

#check and convert crs
av_language <- st_transform(av_language, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_language <- st_intersection(av, av_language)

#sum population of av tracts
av_language <- av_language %>% group_by(variable) %>%
  summarize(language = sum(estimate))

#calculate percentages
av_language$percent = av_language$language / av_language[[4,2]] * 100
  
  
  
  
  
  
  