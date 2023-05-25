# calculate entire AV races of children and compare to LA County

library(dplyr)
library(sf)
library(tidycensus)
options(tigris_use_cache = TRUE)

# postgres credentials
source("W:\\RDA Team\\R\\credentials_source.R") 

# get la spas and 
# https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view

lac_spas <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Shapes/Service_Planning_Areas__2022_.shp", quiet=TRUE)

av <- lac_spas %>% filter (SPA_NAME == "Antelope Valley") # filter for SPA 1 to serve as AV

av <- st_transform(av, crs = 3310) #check st_crs(av) and convert crs


################################################### TOTAL ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18 <- get_acs(geography = "tract",
                         state = "CA",
                         county = "Los Angeles",
                         variables = c(m_under5 = "B01001_003",
                                       m_5to9 = "B01001_004",
                                       m_10to14 = "B01001_005",
                                       m_15to17 = "B01001_006",
                                       f_under5 = "B01001_027",
                                       f_5to9 = "B01001_028",
                                       f_10to14 = "B01001_029",
                                       f_15to17 = "B01001_030"),
                         year = 2021,
                         geometry = TRUE)

#check and convert crs
av_under18 <- st_transform(av_under18, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18 <- st_intersection(av, av_under18)

#sum population of av tracts
av_under18_population <- sum(av_under18$estimate)

#get lac pop under age 18
lac_under18 <- get_acs(geography = "county",
                      state = "CA",
                      county = "Los Angeles",
                      variables = c(m_under5 = "B01001_003",
                                    m_5to9 = "B01001_004",
                                    m_10to14 = "B01001_005",
                                    m_15to17 = "B01001_006",
                                    f_under5 = "B01001_027",
                                    f_5to9 = "B01001_028",
                                    f_10to14 = "B01001_029",
                                    f_15to17 = "B01001_030"),
                      year = 2021,
                      geometry = TRUE)

#sum population of av tracts
lac_under18_population <- sum(lac_under18$estimate)

################################################### BLACK ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18_black <- get_acs(geography = "tract",
                      state = "CA",
                      county = "Los Angeles",
                      variables = c(m_under5 = "B01001B_003",
                                    m_5to9 = "B01001B_004",
                                    m_10to14 = "B01001B_005",
                                    m_15to17 = "B01001B_006",
                                    f_under5 = "B01001B_018",
                                    f_5to9 = "B01001B_019",
                                    f_10to14 = "B01001B_020",
                                    f_15to17 = "B01001B_021"),
                      year = 2021,
                      geometry = TRUE)

#check and convert crs
av_under18_black <- st_transform(av_under18_black, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18_black <- st_intersection(av, av_under18_black)

#sum population of av tracts
av_under18_black_pop <- sum(av_under18_black$estimate)

#get lac pop under age 18
lac_under18_black <- get_acs(geography = "county",
                             state = "CA",
                             county = "Los Angeles",
                             variables = c(m_under5 = "B01001B_003",
                                           m_5to9 = "B01001B_004",
                                           m_10to14 = "B01001B_005",
                                           m_15to17 = "B01001B_006",
                                           f_under5 = "B01001B_027",
                                           f_5to9 = "B01001B_028",
                                           f_10to14 = "B01001B_029",
                                           f_15to17 = "B01001B_030"),
                             year = 2021,
                             geometry = TRUE)

#sum population of av tracts
lac_under18_black_pop <- sum(lac_under18_black$estimate)

################################################### AIAN ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18_aian <- get_acs(geography = "tract",
                           state = "CA",
                           county = "Los Angeles",
                           variables = c(m_under5 = "B01001C_003",
                                         m_5to9 = "B01001C_004",
                                         m_10to14 = "B01001C_005",
                                         m_15to17 = "B01001C_006",
                                         f_under5 = "B01001C_018",
                                         f_5to9 = "B01001C_019",
                                         f_10to14 = "B01001C_020",
                                         f_15to17 = "B01001C_021"),
                           year = 2021,
                           geometry = TRUE)

#check and convert crs
av_under18_aian <- st_transform(av_under18_aian, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18_aian <- st_intersection(av, av_under18_aian)

#sum population of av tracts
av_under18_aian_pop <- sum(av_under18_aian$estimate)

#get lac pop under age 18
lac_under18_aian <- get_acs(geography = "county",
                            state = "CA",
                            county = "Los Angeles",
                            variables = c(m_under5 = "B01001C_003",
                                          m_5to9 = "B01001C_004",
                                          m_10to14 = "B01001C_005",
                                          m_15to17 = "B01001C_006",
                                          f_under5 = "B01001C_027",
                                          f_5to9 = "B01001C_028",
                                          f_10to14 = "B01001C_029",
                                          f_15to17 = "B01001C_030"),
                            year = 2021,
                            geometry = TRUE)

#sum population of av tracts
lac_under18_aian_pop <- sum(lac_under18_aian$estimate)

################################################### ASIAN ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18_asian <- get_acs(geography = "tract",
                            state = "CA",
                            county = "Los Angeles",
                            variables = c(m_under5 = "B01001D_003",
                                          m_5to9 = "B01001D_004",
                                          m_10to14 = "B01001D_005",
                                          m_15to17 = "B01001D_006",
                                          f_under5 = "B01001D_018",
                                          f_5to9 = "B01001D_019",
                                          f_10to14 = "B01001D_020",
                                          f_15to17 = "B01001D_021"),
                            year = 2021,
                            geometry = TRUE)

#check and convert crs
av_under18_asian <- st_transform(av_under18_asian, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18_asian <- st_intersection(av, av_under18_asian)

#sum population of av tracts
av_under18_asian_pop <- sum(av_under18_asian$estimate)

#get lac pop under age 18
lac_under18_asian <- get_acs(geography = "county",
                             state = "CA",
                             county = "Los Angeles",
                             variables = c(m_under5 = "B01001D_003",
                                           m_5to9 = "B01001D_004",
                                           m_10to14 = "B01001D_005",
                                           m_15to17 = "B01001D_006",
                                           f_under5 = "B01001D_027",
                                           f_5to9 = "B01001D_028",
                                           f_10to14 = "B01001D_029",
                                           f_15to17 = "B01001D_030"),
                             year = 2021,
                             geometry = TRUE)

#sum population of av tracts
lac_under18_asian_pop <- sum(lac_under18_asian$estimate)

################################################### NHPI ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18_nhpi <- get_acs(geography = "tract",
                           state = "CA",
                           county = "Los Angeles",
                           variables = c(m_under5 = "B01001E_003",
                                         m_5to9 = "B01001E_004",
                                         m_10to14 = "B01001E_005",
                                         m_15to17 = "B01001E_006",
                                         f_under5 = "B01001E_018",
                                         f_5to9 = "B01001E_019",
                                         f_10to14 = "B01001E_020",
                                         f_15to17 = "B01001E_021"),
                           year = 2021,
                           geometry = TRUE)

#check and convert crs
av_under18_nhpi <- st_transform(av_under18_nhpi, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18_nhpi <- st_intersection(av, av_under18_nhpi)

#sum population of av tracts
av_under18_nhpi_pop <- sum(av_under18_nhpi$estimate)

#get lac pop under age 18
lac_under18_nhpi <- get_acs(geography = "county",
                            state = "CA",
                            county = "Los Angeles",
                            variables = c(m_under5 = "B01001E_003",
                                          m_5to9 = "B01001E_004",
                                          m_10to14 = "B01001E_005",
                                          m_15to17 = "B01001E_006",
                                          f_under5 = "B01001E_027",
                                          f_5to9 = "B01001E_028",
                                          f_10to14 = "B01001E_029",
                                          f_15to17 = "B01001E_030"),
                            year = 2021,
                            geometry = TRUE)

#sum population of av tracts
lac_under18_nhpi_pop <- sum(lac_under18_nhpi$estimate)

################################################### Other ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18_other <- get_acs(geography = "tract",
                            state = "CA",
                            county = "Los Angeles",
                            variables = c(m_under5 = "B01001F_003",
                                          m_5to9 = "B01001F_004",
                                          m_10to14 = "B01001F_005",
                                          m_15to17 = "B01001F_006",
                                          f_under5 = "B01001F_018",
                                          f_5to9 = "B01001F_019",
                                          f_10to14 = "B01001F_020",
                                          f_15to17 = "B01001F_021"),
                            year = 2021,
                            geometry = TRUE)

#check and convert crs
av_under18_other <- st_transform(av_under18_other, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18_other <- st_intersection(av, av_under18_other)

#sum population of av tracts
av_under18_other_pop <- sum(av_under18_other$estimate)

#get lac pop under age 18
lac_under18_other <- get_acs(geography = "county",
                             state = "CA",
                             county = "Los Angeles",
                             variables = c(m_under5 = "B01001F_003",
                                           m_5to9 = "B01001F_004",
                                           m_10to14 = "B01001F_005",
                                           m_15to17 = "B01001F_006",
                                           f_under5 = "B01001F_027",
                                           f_5to9 = "B01001F_028",
                                           f_10to14 = "B01001F_029",
                                           f_15to17 = "B01001F_030"),
                             year = 2021,
                             geometry = TRUE)

#sum population of av tracts
lac_under18_other_pop <- sum(lac_under18_other$estimate)

################################################### Two or More ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18_twoormor <- get_acs(geography = "tract",
                               state = "CA",
                               county = "Los Angeles",
                               variables = c(m_under5 = "B01001G_003",
                                             m_5to9 = "B01001G_004",
                                             m_10to14 = "B01001G_005",
                                             m_15to17 = "B01001G_006",
                                             f_under5 = "B01001G_018",
                                             f_5to9 = "B01001G_019",
                                             f_10to14 = "B01001G_020",
                                             f_15to17 = "B01001G_021"),
                               year = 2021,
                               geometry = TRUE)

#check and convert crs
av_under18_twoormor <- st_transform(av_under18_twoormor, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18_twoormor <- st_intersection(av, av_under18_twoormor)

#sum population of av tracts
av_under18_twoormor_pop <- sum(av_under18_twoormor$estimate)

#get lac pop under age 18
lac_under18_twoormor <- get_acs(geography = "county",
                                state = "CA",
                                county = "Los Angeles",
                                variables = c(m_under5 = "B01001G_003",
                                              m_5to9 = "B01001G_004",
                                              m_10to14 = "B01001G_005",
                                              m_15to17 = "B01001G_006",
                                              f_under5 = "B01001G_027",
                                              f_5to9 = "B01001G_028",
                                              f_10to14 = "B01001G_029",
                                              f_15to17 = "B01001G_030"),
                                year = 2021,
                                geometry = TRUE)

#sum population of av tracts
lac_under18_twoormor_pop <- sum(lac_under18_twoormor$estimate)

################################################### NH White ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18_nh_white <- get_acs(geography = "tract",
                               state = "CA",
                               county = "Los Angeles",
                               variables = c(m_under5 = "B01001H_003",
                                             m_5to9 = "B01001H_004",
                                             m_10to14 = "B01001H_005",
                                             m_15to17 = "B01001H_006",
                                             f_under5 = "B01001H_018",
                                             f_5to9 = "B01001H_019",
                                             f_10to14 = "B01001H_020",
                                             f_15to17 = "B01001H_021"),
                               year = 2021,
                               geometry = TRUE)

#check and convert crs
av_under18_nh_white <- st_transform(av_under18_nh_white, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18_nh_white <- st_intersection(av, av_under18_nh_white)

#sum population of av tracts
av_under18_nh_white_pop <- sum(av_under18_nh_white$estimate)

#get lac pop under age 18
lac_under18_nh_white <- get_acs(geography = "county",
                                state = "CA",
                                county = "Los Angeles",
                                variables = c(m_under5 = "B01001H_003",
                                              m_5to9 = "B01001H_004",
                                              m_10to14 = "B01001H_005",
                                              m_15to17 = "B01001H_006",
                                              f_under5 = "B01001H_027",
                                              f_5to9 = "B01001H_028",
                                              f_10to14 = "B01001H_029",
                                              f_15to17 = "B01001H_030"),
                                year = 2021,
                                geometry = TRUE)

#sum population of av tracts
lac_under18_nh_white_pop <- sum(lac_under18_nh_white$estimate)

################################################### Latinx ##############################################

#get av pop under age 18 as tracts within SPA 1
av_under18_latinx <- get_acs(geography = "tract",
                             state = "CA",
                             county = "Los Angeles",
                             variables = c(m_under5 = "B01001I_003",
                                           m_5to9 = "B01001I_004",
                                           m_10to14 = "B01001I_005",
                                           m_15to17 = "B01001I_006",
                                           f_under5 = "B01001I_018",
                                           f_5to9 = "B01001I_019",
                                           f_10to14 = "B01001I_020",
                                           f_15to17 = "B01001I_021"),
                             year = 2021,
                             geometry = TRUE)

#check and convert crs
av_under18_latinx <- st_transform(av_under18_latinx, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
av_under18_latinx <- st_intersection(av, av_under18_latinx)

#sum population of av tracts
av_under18_latinx_pop <- sum(av_under18_latinx$estimate)

#get lac pop under age 18
lac_under18_latinx <- get_acs(geography = "county",
                              state = "CA",
                              county = "Los Angeles",
                              variables = c(m_under5 = "B01001I_003",
                                            m_5to9 = "B01001I_004",
                                            m_10to14 = "B01001I_005",
                                            m_15to17 = "B01001I_006",
                                            f_under5 = "B01001I_027",
                                            f_5to9 = "B01001I_028",
                                            f_10to14 = "B01001I_029",
                                            f_15to17 = "B01001I_030"),
                              year = 2021,
                              geometry = TRUE)

#sum population of av tracts
lac_under18_latinx_pop <- sum(lac_under18_latinx$estimate)

################################################# SUM #####################################################

race <- c("AIAN", "Asian", "Black", "Latinx", "Non-Hispanic White", "NHPI", "Other", "Multiracial", "Total")
  
av <- c(av_under18_aian_pop, av_under18_asian_pop, av_under18_black_pop, av_under18_latinx_pop,
                   av_under18_nh_white_pop, av_under18_nhpi_pop, av_under18_other_pop, av_under18_twoormor_pop,
                   av_under18_population)

lac <- c(lac_under18_aian_pop, lac_under18_asian_pop, lac_under18_black_pop, lac_under18_latinx_pop,
         lac_under18_nh_white_pop, lac_under18_nhpi_pop, lac_under18_other_pop, lac_under18_twoormor_pop,
         lac_under18_population)

race_groups <- data.frame(race, av, lac)

race_groups$av_pct = race_groups$av / av_under18_population * 100
race_groups$lac_pct = race_groups$lac / lac_under18_population * 100
                         
                         
                         
                         
                         
                         
                         