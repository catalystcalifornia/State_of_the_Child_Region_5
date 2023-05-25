# Get language data from Luz's file for AV neighborhoods, LA County, California

library("tidyverse") #was listed as stale
library("readxl")

# data downloaded to W drive from:
# https://public.tableau.com/app/profile/luz3725/viz/2020CensusData-AVBESTSTARTREGION5/GRAPI
data <- read_excel("W:/Project/RDA Team/Region 5 State of the Child/Data/ACS 2016-2020 WORKBOOK - for AV report.xlsx", sheet = 1)

data <- data %>% filter(is.na(GEO_ID) | NEIGHBORHOOD %in% c("LOS ANGELES COUNTY", "CALIFORNIA"))%>% 
  
  dplyr::select(NEIGHBORHOOD,
                        `% LANGUAGE SPOKEN AT HOME!!English only`,
                        `% LANGUAGE SPOKEN AT HOME!!Spanish`,
                        `% LANGUAGE SPOKEN AT HOME!!PAsian and Pacific Islander languages`,
                        `% LANGUAGE SPOKEN AT HOME!!Other Indo-European languages`)

names(data) <- c("Geography", "English (%)", "Spanish (%)",
                 "Asian or Pacific Islander language (%)", 
                 "Other Indo-European language (%)")

data$'English (%)' <- data$'English (%)' * 100
data$'Spanish (%)' <- data$'Spanish (%)' * 100
data$'Asian or Pacific Islander language (%)' <- data$'Asian or Pacific Islander language (%)' * 100
data$'Other Indo-European language (%)' <- data$'Other Indo-European language (%)' * 100
view(data)