# Calculate AV and County rates of hospitals, mental health centers and health clinics

library(dplyr)
library(sf)
library(stringr)

# get la spas and download to W drive from: 
#https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view
lac_spas <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Shapes/Service_Planning_Areas__2022_.shp", quiet=TRUE)

av <- lac_spas %>% filter (SPA_NAME == "Antelope Valley") # filter for SPA 1 to serve as AV 

av <- st_transform(av, crs = 3310) #check st_crs(av) and convert crs to 3310


#get av pop as tracts within SPA 1
av_pop <- get_acs(geography = "tract",
                  state = "CA",
                  county = "Los Angeles",
                  variables = "B01001_001",
                  year = 2020,
                  geometry = TRUE)

av_pop <- st_transform(av, crs = 3310) #check st_crs(av) and convert crs to 3310


#calculate spatial intersection of tracts in av (SPA 1)
av_pop <- st_intersection(av, av_pop)


#sum population of av tracts
av_pop <- sum(av_pop$estimate)


#get LA County pop
lac_pop <- get_acs(geography = "county",
                   state = "CA",
                   variables = "B01001_001",
                   year = 2020) 

lac_pop <- lac_pop %>% filter(NAME == "Los Angeles County, California") 
lac_pop <- lac_pop$estimate[1]



#download hospitals and medical center to W drive from
#https://data.lacounty.gov/datasets/lacounty::hospitals-and-medical-centers
hospitals <- st_read('W:/Project/RDA Team/Region 5 State of the Child/Shapes/Hospitals_and_Medical_Centers.shp')

#check and convert crs
hospitals <- st_transform(hospitals, crs = 3310)
st_crs(hospitals)

av_hospitals <- st_intersection(av, hospitals) #3 services
av_hospitals_rate <- nrow(av_hospitals)/av_pop * 100000 
lac_hospitals_rate <- nrow(hospitals)/lac_pop * 100000



#download mental health clinics to W drive from:
#https://data.lacounty.gov/datasets/lacounty::health-clinics
clinics <- st_read('W:/Project/RDA Team/Region 5 State of the Child/Shapes/Health_Clinics.shp')

clinics <- st_transform(clinics, crs = 3310) #check st_crs(clinics) and convert crs

av_clinics <- st_intersection(av, clinics) #3 services
av_clinics_rate <- nrow(av_clinics)/av_pop * 100000 
lac_clinics_rate <- nrow(clinics)/lac_pop * 100000



#download mental health centers to W drive from:
#https://data.lacounty.gov/datasets/lacounty::mental-health-centers
mh_centers <- st_read('W:/Project/RDA Team/Region 5 State of the Child/Shapes/Mental_Health_Centers.shp')

#check and convert crs
mh_centers <- st_transform(mh_centers, crs = 3310)
st_crs(mh_centers)

av_mh_centers <- st_intersection(av, mh_centers) #3 services
av_mh_centers_rate <- nrow(av_mh_centers)/av_pop * 100000 
lac_mh_centers_rate <- nrow(mh_centers)/lac_pop * 100000

