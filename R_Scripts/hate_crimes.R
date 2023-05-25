# Compare la county hate crime data to av hate crime data. 

library(dplyr)
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)

# ACS credentials
source("W:\\RDA Team\\R\\credentials_source.R") 


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

av_pop <- st_transform(av_pop, crs = 3310) #check st_crs(av) and convert crs to 3310


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


# get hate crimes data and save to W drive from
# https://egis-lacounty.hub.arcgis.com/datasets/lacounty::reported-hate-crimes
hate_crimes <- st_read('W:/Project/RDA Team/Region 5 State of the Child/Shapes/hate_crime_2003_2020_LAC_HRC_by_tract.shp', quiet=TRUE)
hate_crimes$geoid = paste0("0", hate_crimes$geoid)

hate_crimes <- st_transform(hate_crimes, crs = 3310) #check st_crs(av) and convert crs to 3310


# sum data for la county
lac_hate_crimes <- hate_crimes %>% summarise_at(vars(disability:tract_tota), sum, na.rm = TRUE)


# calculate rates
number_years <- 18

lac_hate_crime_rates <- as.data.frame(lac_hate_crimes) %>% dplyr::select(-geometry) / lac_pop/number_years * 100000
lac_hate_crime_rates <- lac_hate_crime_rates %>%  mutate(geography = "LA County", variable = "per 100k") %>%
  dplyr::select(geography, variable, everything())


# av hate crime data with spatial selection bc (non-spatial) join doesn't work
av_hate_crimes <- st_intersection(av, hate_crimes)

# sum data 
av_hate_crimes <- av_hate_crimes %>% summarise_at(vars(disability:tract_tota), sum, na.rm = TRUE)

# calculate rates
av_hate_crime_rates <- as.data.frame(av_hate_crimes) %>% dplyr::select(-geometry) / av_pop/number_years * 100000
av_hate_crime_rates <- av_hate_crime_rates %>%  mutate(geography = "Antelope Valley", variable = "per 100k") %>%
  dplyr::select(geography, variable, everything())


#combine in one table
lac_hate_crimes <- as.data.frame(lac_hate_crimes) %>% dplyr::select(-geometry) %>%
  mutate(geography = "LA County", variable = "Hate Crimes") %>%
  dplyr::select(geography, variable, everything())

av_hate_crimes <- as.data.frame(av_hate_crimes) %>% dplyr::select(-geometry) %>%
  mutate(geography = "Antelope Valley", variable = "Hate Crimes") %>%
  dplyr::select(geography, variable, everything())

table <- rbind(av_hate_crimes, av_hate_crime_rates, lac_hate_crimes, lac_hate_crime_rates)

names(table) <- c('geography',
                  'variable',
                  'disability_mental',
                  'disability_physical',
                  'disability_total',
                  'gender_female',
                  'gender_trans',
                  'gender_total',
                  'race_eth_origin_african',
                  'race_eth_origin_african_american',
                  'race_eth_origin_american',
                  'race_eth_origin_american_indian',
                  'race_eth_origin_armenian',
                  'race_eth_origin_api',
                  'race_eth_origin_azerbaijani',
                  'race_eth_origin_canadian',
                  'race_eth_origin_croatian',
                  'race_eth_origin_danish',
                  'race_eth_origin_ethiopian',
                  'race_eth_origin_french',
                  'race_eth_origin_german',
                  'race_eth_origin_jamaican',
                  'race_eth_origin_latino',
                  'race_eth_origin_middle_easterner',
                  'race_eth_origin_multi_racial',
                  'race_eth_origin_nonwhite',
                  'race_eth_origin_other',
                  'race_eth_origin_portuguese',
                  'race_eth_origin_romani_gypsy',
                  'race_eth_origin_russian',
                  'race_eth_origin_serbian',
                  'race_eth_origin_turkish',
                  'race_eth_origin_unknown',
                  'race_eth_origin_white',
                  'race_eth_origin_total',
                  'religion_atheist_agnostic',
                  'religion_buddhist',
                  'religion_catholic',
                  'religion_christian',
                  'religion_hindu',
                  'religion_jehovahs_witness',
                  'religion_jewish',
                  'religion_mormon_lds',
                  'religion_muslim',
                  'religion_other',
                  'religion_other_christian',
                  'religion_protestant',
                  'religion_scientologist',
                  'religion_sikh',
                  'religion_unknown',
                  'religion_total',
                  'sept11_middle_east_conflict_armenian',
                  'sept11_middle_east_conflict_danish',
                  'sept11_middle_east_conflict_jewish',
                  'sept11_middle_east_conflict_middle_easterner',
                  'sept11_middle_east_conflict_muslim',
                  'sept11_middle_east_conflict_unknown',
                  'sept11_middle_east_conflict_total',
                  'sexual_orientation_bisexual',
                  'sexual_orientation_gay_male',
                  'sexual_orientation_heterosexual',
                  'sexual_orientation_lesbian',
                  'sexual_orientation_lgbt_not_specified',
                  'sexual_orientation_total',
                  'unknown_total',
                  'blank_total',
                  'tract_total'
)


# format final table
hate_crime_table <- table %>% dplyr::select(geography, variable, disability_total, race_eth_origin_total, 
                                     religion_total, sexual_orientation_total, unknown_total, tract_total) %>%
  filter(geography == "Antelope Valley" | variable == "per 100k")

hate_crime_table <- as.data.frame(t(hate_crime_table))

hate_crime_table <- hate_crime_table[3:8,]

names(hate_crime_table) <- c("AV Hate Crimes 2003-2020", "AV Hate Crimes per 100k", "LA County Hate Crimes per 100k")
rownames(hate_crime_table) <- c("Disability", "Race-ethnicity", "Religion", "Sexual Orientation", "Unknown", "TOTAL")

hate_crime_table$`AV Hate Crimes 2003-2020` <- round(as.numeric(hate_crime_table$`AV Hate Crimes 2003-2020`, 1))
hate_crime_table$`AV Hate Crimes per 100k` <- round(as.numeric(hate_crime_table$`AV Hate Crimes per 100k`), 2)
hate_crime_table$`LA County Hate Crimes per 100k` <- round(as.numeric(hate_crime_table$`LA County Hate Crimes per 100k`), 2)
