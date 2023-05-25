# get age 0 to 5 data for entire AV (including areas beyond Luz Wilkerson boundaries)

library(dplyr)
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)

# acs credentials
source("W:\\RDA Team\\R\\credentials_source.R") 


# load la SPAs and filter for SPA 1 to serve as AV
# SPAs from https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view/explore?location=34.025999%2C-118.300941%2C8.00 
lac_spas <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Shapes/Service_Planning_Areas__2022_.shp", quiet=TRUE)
av <- lac_spas %>% filter (SPA_NAME == "Antelope Valley")

#check and convert crs
#st_crs(av)
av <- st_transform(av, crs = 3310)
#st_crs(av)


#get av pop under age 5 as tracts within SPA 1
av_age_under5 <- get_acs(geography = "tract",
                  state = "CA",
                  county = "Los Angeles",
                  variables = "DP05_0005E",
                  year = 2021,
                  geometry = TRUE)

#check and convert crs
#st_crs(av_pop)
av_age_under5 <- st_transform(av_age_under5, crs = 3310)
#st_crs(av_pop)


#calculate spatial intersection of tracts in av (SPA 1)
av_age_under5 <- st_intersection(av, av_age_under5)
#plot(av_pop$geometry)

#sum population of av tracts
av_population_under5 <- sum(av_age_under5$estimate)


#get LA County pop
lac_age_under5 <- get_acs(geography = "county",
                   state = "CA",
                   variables = "DP05_0005E",
                   year = 2021) 

lac_age_under5 <- lac_age_under5 %>% filter(NAME == "Los Angeles County, California") 
lac_age_under5 <- lac_age_under5$estimate[1]



#get av pop as tracts within SPA 1
av_pop <- get_acs(geography = "tract",
                  state = "CA",
                  county = "Los Angeles",
                  variables = "B01001_001",
                  year = 2021,
                  geometry = TRUE)

#check and convert crs
#st_crs(av_pop)
av_pop <- st_transform(av_pop, crs = 3310)
#st_crs(av_pop)


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


av_pct_under5 <- av_population_under5 /av_population * 100
lac_pct_under5 <- lac_age_under5 / lac_pop * 100


##### Children under 18 #####


#get av pop under age 18 as tracts within SPA 1
av_age_under18 <- get_acs(geography = "tract",
                         state = "CA",
                         county = "Los Angeles",
                         variables = "DP05_0019E",
                         year = 2021,
                         geometry = TRUE)

#convert crs
av_age_under18 <- st_transform(av_age_under18, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_age_under18 <- st_intersection(av, av_age_under18)

#sum population of av tracts
av_population_under18 <- sum(av_age_under18$estimate)

#get LA County pop
lac_age_under18 <- get_acs(geography = "county",
                          state = "CA",
                          variables = "DP05_0019E",
                          year = 2021) 

lac_age_under18 <- lac_age_under18 %>% filter(NAME == "Los Angeles County, California") 
lac_age_under18 <- lac_age_under18$estimate[1]




