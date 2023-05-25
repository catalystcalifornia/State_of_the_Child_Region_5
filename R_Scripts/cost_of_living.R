# Get real cost measure data

library("tidyverse")
library("readxl")
library("scales")

# real cost measure public dataset downloaded to W drive from:
# https://www.unitedwaysca.org/the-real-cost-measure-in-california-2021
rcm_data_puma_state <- read_excel("W:/Project/RDA Team/First5LA/V2.0 Report/Data/Real Cost Measure 2019/2021-public-data-set.xlsx", sheet = 3)
rcm_data_county <- read_excel("W:/Project/RDA Team/First5LA/V2.0 Report/Data/Real Cost Measure 2019/2021-public-data-set.xlsx", sheet = 2)

rcm_data_county <- rcm_data_county %>% dplyr::filter(`County/County Cluster` == "Los Angeles County") %>% 
  dplyr::rename(Geography = `County/County Cluster`)

rcm_data_puma_state <- rcm_data_puma_state %>% dplyr::filter(`Neighborhood Cluster` %in% c("Los Angeles County (North Central)--Lancaster City", 
                                                              "Los Angeles County (North Central)--Palmdale City",
                                                              "Los Angeles County (North/Unincorporated)--Castaic",
                                                              "State of California")) %>%
  dplyr::select(-`PUMA ID`) %>%
  dplyr::rename(Geography = `Neighborhood Cluster`)  


rcm_data <- rbind(rcm_data_puma_state, rcm_data_county) %>%
  dplyr::select(Geography,#`% Households below Official Poverty Measure`,
         `% Households Below RCM`, 
         `% White Households Below RCM`,
         `% African American Households Below RCM`,
         #`% Asian American/Pacific Islander Households Below RCM`, #no data
         `% Latino Households Below RCM`,
         #`% Native American/Alaska Native Households Below RCM`, #no data
         `% Households with a Child Under Age 6 Below RCM`,
         #`% Households Paying 30% or More on Housing`,
        `Median Adjusted Household Income`) %>%
  dplyr::rename(`% Black Households Below RCM` = `% African American Households Below RCM`,
         `% Households with a child under 6 Below RCM` = `% Households with a Child Under Age 6 Below RCM`)

#format columns
rcm_data$`% Households Below RCM` <- round(rcm_data$`% Households Below RCM` * 100, 1)
rcm_data$`% White Households Below RCM` <- round(rcm_data$`% White Households Below RCM` * 100, 1)
rcm_data$`% Black Households Below RCM` <- round(rcm_data$`% Black Households Below RCM` * 100, 1)
rcm_data$`% Latino Households Below RCM` <- round(rcm_data$`% Latino Households Below RCM` * 100, 1)
rcm_data$`% Households with a child under 6 Below RCM` <- round(rcm_data$`% Households with a child under 6 Below RCM` * 100, 1)
rcm_data$`Median Adjusted Household Income` = dollar_format()(rcm_data$`Median Adjusted Household Income`)

# create a vector with letters in the desired order
x <- c("Los Angeles County (North Central)--Lancaster City", 
       "Los Angeles County (North Central)--Palmdale City", 
       #"Los Angeles County (North/Unincorporated)--Castaic",
       "Los Angeles County", "State of California")

rcm_data <- rcm_data %>%
  slice(match(x, Geography))
# 
# # add Spanish translation
# rcm_data <- rcm_data %>% rename("Geography/Geografía"="Geography",
#                                 "Total" = `% Households Below RCM`,
#                               "White/Blanco"  = `% White Households Below RCM`,
#                               "Black/Negro"  = `% Black Households Below RCM`,
#                               "Latino"  = `% Latino Households Below RCM`,
#                               "Households with a child under 6/Hogares con un niño menor de 6 años" = `% Households with a child under 6 Below RCM`) %>%
#   select(-`Median Adjusted Household Income`)
# rcm_data
