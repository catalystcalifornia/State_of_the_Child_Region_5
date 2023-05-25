# Calculate AV and County substantiated referral rates of child abuse,
# domestic violence services rates, and children and family services rates,
# the latter two requiring AV and county population denominators.

library(dplyr)
library(sf)

# postgres credentials
source("W:\\RDA Team\\R\\credentials_source.R") 

# get DCFS substantiated referral saved to W drive,from
# https://data.lacounty.gov/datasets/lacounty::dcfs-all-substantiated-referrals-2021
referrals <- st_read('W:/Project/RDA Team/Region 5 State of the Child/Shapes/dcfs_2021_csa.shp')

referrals <- st_transform(referrals, crs = 3310) # check st_crs(referrals) and convert crs

# calculate LA county referral rate as the mean of all countywide statistical area (CSA) rates
lac_referral_rate <- mean(referrals$Rate_per_1, na.rm = TRUE)

# get la spas saved to W drive, originally from
# https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view 
lac_spas <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Shapes/Service_Planning_Areas__2022_.shp", quiet=TRUE)

av <- lac_spas %>% filter (SPA_NAME == "Antelope Valley") # filter for SPA 1 to serve as AV
av <- st_transform(av, crs = 3310) # check st_crs(av) and convert crs to 3310

av_referrals <- st_intersection(av, referrals)
av_referrals <- av_referrals %>% arrange(desc(av_referrals$Rate_per_1))

# calculate AV referral rate as the mean of all AV CSA rates
av_referral_rate <- mean(av_referrals$Rate_per_1, na.rm = TRUE)



#get domestic violence service data saved to W drive, originally from
#https://data.lacounty.gov/datasets/lacounty::domestic-violence-services
dv_services <- st_read('W:/Project/RDA Team/Region 5 State of the Child/Shapes/Domestic_Violence_services.shp')

dv_services <- st_transform(dv_services, crs = 3310) # check st_crs(av) and convert crs to 3310

av_dv_services <- st_intersection(av, dv_services) #3 services

# get av pop as tracts within SPA 1
av_pop <- get_acs(geography = "tract",
                  state = "CA",
                  county = "Los Angeles",
                  variables = "B01001_001",
                  year = 2020,
                  geometry = TRUE)

av_pop <- st_transform(av_pop, crs = 3310) # check st_crs(av) and convert crs to 3310

# calculate spatial intersection of tracts in av (SPA 1)
av_pop <- st_intersection(av, av_pop)

# sum population of av tracts
av_pop <- sum(av_pop$estimate)

# get LA County pop
lac_pop <- get_acs(geography = "county",
                   state = "CA",
                   variables = "B01001_001",
                   year = 2020) 

lac_pop <- lac_pop %>% filter(NAME == "Los Angeles County, California") 
lac_pop <- lac_pop$estimate[1]

# calculate DV rates
av_dv_service_rate <- nrow(av_dv_services)/av_pop * 100000 
lac_dv_service_rate <- nrow(dv_services)/lac_pop * 100000
dv_rate_difference <- lac_dv_service_rate/av_dv_service_rate



#get children and family services saved to W drive, originally from
#https://data.lacounty.gov/datasets/lacounty::children-and-family-services-1
cf_services <- st_read('W:/Project/RDA Team/Region 5 State of the Child/Shapes/Children_and_Family_Services.shp')

cf_services <- st_transform(cf_services, crs = 3310) # check st_crs(av) and convert crs to 3310

av_cf_services <- st_intersection(av, cf_services) #37 services
av_cf_service_rate <- 37/397272 * 100000
lac_cf_service_rate <- 810/10260237 * 100000



#table of values used in analysis
labels <- c("AV referral rate", "LA County referral rate", "Difference AV-LAC DV rates",
            "AV % of Children Who's Caregivers Reported Difficulty Accessing Medical Care",
            "LAC % of Children Who's Caregivers Reported Difficulty Accessing Medical Care",
            "AV % Seeking Health Care Experience Worse than Other Races",
            "LAC % Seeking Health Care Experience Worse than Other Races")
values <- c(av_referral_rate, lac_referral_rate, dv_rate_difference, 16.4, 9.3, 6.7, 3.8)
analysis_table <- data.frame(labels, values)

# 
# Analysis
# **The Antelope Valley is a County hotspot for high substantiated referral rates.** The average countywide statistical area referral rate in the Antelope Valley is nearly twice the county average (12.2 substantiated referrals per 1,000 children in the Antelope Valley vs. 6.4 per 1,000 in LA County)[1](https://data.lacounty.gov/datasets/lacounty::dcfs-all-substantiated-referrals-2021).
# 
# **The Antelope Valley is a County cold spot for services supporting children and families in the child welfare system**, unfortunately. Some families experience domestic violence at the same time child abuse or neglect is occurring. According to Catalyst California calculations of LA County Open Data Portal data[1](https://data.lacounty.gov/datasets/lacounty::domestic-violence-services), the Antelope Valley has half as many domestic violence services per capita than the county as a whole. Three organizations are listed as offering domestic violence services in the Antelope Valley (High Road Program - Lancaster, Neighborhood Legal Services Of Los Angeles County - Antelope Valley Domestic Abuse Self Help, and Antelope Valley Domestic Violence Council - Homeless Solutions Access Center). While capacities of domestic violence services across the county are unavailable, there are 0.8 domestic violence services per 100,000 people in the Av, and 6.4 domestic violence services per 100,000 people countywide.
# 
# All foster children are required to have a mental health screening [1](https://theacademy.sdsu.edu/wp-content/uploads/2015/01/understanding-cws.pdf), but AV (SPA 1) parents are most likely to report difficulties accessing medical care for their children than parents in all other SPAs. AV parents reported medical care access difficulties for 16.4% of AV children, whereas LA County parents reported difficulties for 9.3% of county children overall[1](http://publichealth.lacounty.gov/ha/docs/2018LACHS/MDT/Child/M3_ChildAccessToCare/M3_AccessToHealthCare_CACCDIFF.xlsx).
# 
# These parents are part of the larger adult population that reports highest-among-SPA rates of discrimination when seeking health care. 6.7% of AV adults rated their experience as worse Compared to people of other races when seeking health care in 2018, compared to 3.8% of county adults overall[1](http://publichealth.lacounty.gov/ha/docs/2018LACHS/MDT/Adult/M3_SocialDeterminantsOfHealth/M3_Discrimination_DISC66HC.xlsx).
# 
# On a positive note, there are many services aimed at children and families in the Antelope Valley, including the Black Infant Health Program of Antelope Valley, the Children's Bureau Sunrise Center, and others who work to ensure families have what they need to thrive.
# 

