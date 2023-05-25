# Foreclosures per 10k Owner-Occupied Households
### source: "DataQuick (2017-2022), purchased from DQNews"

# Install packages if not already installed
list.of.packages <- c("stringr", "dplyr", "srvyr", "tidyr", "readxl", "janitor")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load libraries
library(stringr)
library(dplyr)
library(srvyr)
library(tidyr)
library(readxl)
library(janitor)
options(scipen=999)

#load data purchased from DataQuick and clean (2022 only has to quarters of data)
foreclosure <- read_excel("W:/Data/Housing/Foreclosure/Foreclosure - Dataquick/Original Data/AdvanceProj 081122.xlsx", sheet = 2, skip = 5)

num_qtrs = 22   # update depending on how many data yrs you are working with
foreclosure <- foreclosure %>% select(-matches('2010|2011|2012|2013|2014|2015|2016')) 

foreclosure <- foreclosure %>%
  mutate(sum_foreclosure = rowSums(.[3:22], na.rm = TRUE)) %>%  # total number of foreclosures over all data quarters
  select(-matches('Q'))   # remove quarterly foreclosure columns
  
foreclosure$County = str_to_title(foreclosure$County)
foreclosure <- clean_names(foreclosure)     # remove spaces from col names
foreclosure <- foreclosure %>% filter(county =="Los Angeles")
foreclosure$geoid <- paste0("06037",foreclosure$census_tract)

#load in the Antelope Valley population by census tract
av_data <- read_excel("W:/Project/RDA Team/Region 5 State of the Child/Data/ACS 2016-2020 WORKBOOK - for AV report.xlsx", sheet = "DP04") %>% 
  select("NEIGHBORHOOD", #select only useful columns
         "GEO_ID", 
         "Owner-occupied") %>% rename("Owner_occupied"="Owner-occupied","geoid"="GEO_ID") %>% 
  drop_na() %>% 
  filter(NEIGHBORHOOD == "LANCASTER" | NEIGHBORHOOD == "PALMDALE" | 
                                NEIGHBORHOOD == "QUARTZ HILL" | NEIGHBORHOOD == "EAST ANTELOPE VALLEY" | 
                                NEIGHBORHOOD == "WEST ANTELOPE VALLEY" | NEIGHBORHOOD == "LAKE LOS ANGELES")
av_data$Owner_occupied <- as.numeric(av_data$Owner_occupied)


# get the total raw and pop by census tract----

df <- av_data %>% left_join(foreclosure, by="geoid") %>% 
  dplyr::rename("geoname" = "NEIGHBORHOOD") %>% 
  dplyr::select(-c(county, census_tract))

#summarize by geoname to bet by neighborhood data
df_grouped <- df %>% 
  group_by(geoname) %>% 
  dplyr::select(-geoid) %>% 
  mutate(raw = sum(sum_foreclosure, na.rm=T), 
         pop = sum(Owner_occupied, na.rm=T),
         avg = (raw/ num_qtrs),
         rate = (avg/pop*10000)) %>%  # avg quarterly number of foreclosures
  dplyr::select(-c(Owner_occupied, sum_foreclosure)) %>% 
  distinct() %>% 
  ungroup() %>% 
  as.data.frame()

av_tot <- df %>%
  select(-geoid, geoname) %>% 
  mutate(raw = sum(sum_foreclosure, na.rm=T), 
         pop = sum(Owner_occupied, na.rm=T),
         avg=(raw/ num_qtrs),
         rate = (avg/pop*10000)) %>%  # avg quarterly number of foreclosures
  select(-c(Owner_occupied, sum_foreclosure)) %>% 
  as.data.frame()
av_tot$geoname <- "AV BEST START REGION 5"
av_tot <- av_tot %>% distinct()

av_table <- rbind(df_grouped, av_tot)
av_table <- av_table[order(sub("ANTELOPE VALLEY BEST START REGION 5", " ", av_table$geoname)), ]
av_table <- av_table %>% 
  dplyr::rename("Geography" = "geoname", "Total Households" = "pop", "Total Count" = "raw", "Average Count" = "avg", "Avg Foreclosure Rate Per 10K Households" = "rate") %>% 
  dplyr::select(-`Average Count`)



