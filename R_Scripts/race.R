# Get race data from Luz's file

# library("tidyverse")
library("readxl")

#data downloaded to W drive from:
#https://public.tableau.com/app/profile/luz3725/viz/2020CensusData-AVBESTSTARTREGION5/GRAPI
data <- read_excel("W:/Project/RDA Team/Region 5 State of the Child/Data/ACS 2016-2020 WORKBOOK - for AV report.xlsx", sheet = 1)

race_data <- data %>% filter(is.na(GEO_ID) | NEIGHBORHOOD %in% c("LOS ANGELES COUNTY", "CALIFORNIA")) %>% 
  
  dplyr::select(NEIGHBORHOOD,
         `% Hispanic or Latino (of any race)`,
         `% White`,
         `% Black or Afro American`,
         `% Indigenous and Alaska Native`,
         `% Asian`,
         #`% Asian Indian`,	
         #`% Chinese`,
         #`% Filipino`,
         #`% Japanese`,
         #`% Korean`,
         #`% Vietnamese`,	
         #`% Other Asian`,	
         `% Native Hawaiian and Other Pacific Islander`,	
         `% Some other race alone`)

#the rest sum up to Asian
 #rowwise() %>% mutate(Asian_combined = sum(`% Asian`,
  #       `% Asian Indian`,
   #      `% Chinese`,
    #     `% Filipino`,
     #    `% Japanese`,
      #   `% Korean`,
       #  `% Vietnamese`,	
        # `% Other Asian`, na.rm=TRUE))

names(race_data) <- c("Geography", "Latinx (%)", "White (%)", "Black (%)", "AIAN (%)", "Asian (%)", "Pacific Islander (%)", "Other (%)")

race_data$"AIAN (%)" <- round(race_data$"AIAN (%)",3)

race_data$"Latinx (%)" <- race_data$"Latinx (%)" * 100
race_data$"White (%)" <- race_data$"White (%)" * 100
race_data$"Black (%)" <- race_data$"Black (%)" * 100
race_data$"AIAN (%)" <- race_data$"AIAN (%)" * 100
race_data$"Asian (%)" <- race_data$"Asian (%)" * 100
race_data$"Pacific Islander (%)" <- race_data$"Pacific Islander (%)" * 100
race_data$"Other (%)" <- race_data$"Other (%)" * 100

#View(race_data)