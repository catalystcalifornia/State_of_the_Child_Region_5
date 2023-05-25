## Use of Force 2016-2021 for State of the Child
## based on the RC v4 script here: W:/Project/RACE COUNTS/2022_v4/Crime & Justice/R/crim_use_of_force_2022.R

## Set up ----------------------------------------------------------------
#install packages if not already installed
list.of.packages <- c("DBI", "tidyverse","RPostgreSQL", "tidycensus", "readxl", "sf", "janitor", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## packages
library(tidyverse) # listed as stale
library(readxl) # listed as stale
library(RPostgreSQL)
library(sf)
library(tidycensus)
library(DBI)
library(janitor) # listed as stale
library(stringr)
library(data.table) # listed as stale
source("W:\\RDA Team\\R\\credentials_source.R")

# Read Data ---------------------------------------------------------------

## 2016 

df_2016_df <-  read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2016/URSUS_Civilian-Officer_2016.csv")
colnames(df_2016_df) <- tolower(colnames(df_2016_df))

df_2016_incident <- read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2016/URSUS_Incident_2016.csv")
colnames(df_2016_incident) <- tolower(colnames(df_2016_incident))

df_2016_df <- left_join(df_2016_df, df_2016_incident) %>% select(incident_id, civilian_officer, race_ethnic_group, received_force, county, state, city, zip_code, num_involved_civilians) %>% filter(
  received_force == "TRUE", civilian_officer == "Civilian"
) %>% mutate(year = 2016)

df_2016_df$county <- gsub("County", "" , df_2016_df$county)
df_2016_df$county <- str_trim(df_2016_df$county) # remove white space


## 2017

df_2017_df <-  read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2017/URSUS_Civilian-Officer_2017.csv")
colnames(df_2017_df) <- tolower(colnames(df_2017_df))

df_2017_incident <- read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2017/URSUS_Incident_2017.csv")
colnames(df_2017_incident) <- tolower(colnames(df_2017_incident))
df_2017_incident$county <- tolower(df_2017_incident$county)

df_2017_df <- left_join(df_2017_df, df_2017_incident) %>% select(incident_id, civilian_officer, race_ethnic_group, received_force, county, state, city, zip_code, num_involved_civilians) %>% filter(
  received_force == "TRUE", civilian_officer == "Civilian"
) %>% mutate(year = 2017)


## 2018 

df_2018_df <-  read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2018/URSUS_Civilian-Officer_2018.csv")
colnames(df_2018_df) <- tolower(colnames(df_2018_df))

df_2018_incident <- read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2018/URSUS_Incident_2018.csv")
colnames(df_2018_incident) <- tolower(colnames(df_2018_incident))
df_2018_incident$county <- tolower(df_2018_incident$county)

df_2018_df <- left_join(df_2018_df, df_2018_incident) %>% select(incident_id, civilian_officer, race_ethnic_group, received_force, county, state, city, zip_code, num_involved_civilians) %>% filter(
  received_force == "TRUE", civilian_officer == "Civilian"
) %>% mutate(year = 2018)


## 2019

df_2019_df <-  read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2019/URSUS_Civilian-Officer_2019.csv")
colnames(df_2019_df) <- tolower(colnames(df_2019_df))

df_2019_incident <- read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2019/URSUS_Incident_2019.csv")
colnames(df_2019_incident) <- tolower(colnames(df_2019_incident))
df_2019_incident$county <- tolower(df_2019_incident$county)

df_2019_df <- left_join(df_2019_df, df_2019_incident) %>% select(incident_id, civilian_officer, race_ethnic_group, received_force, county, state, city, zip_code, num_involved_civilians) %>% filter(
  received_force == "TRUE", civilian_officer == "Civilian"
) %>% mutate(year = 2019)


## 2020

df_2020_df <-  read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2020/URSUS_Civilian-Officer_2020.csv")
colnames(df_2020_df) <- tolower(colnames(df_2020_df))

df_2020_incident <- read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2020/URSUS_Incident_2020.csv")
colnames(df_2020_incident) <- tolower(colnames(df_2020_incident))
df_2020_incident$county <- tolower(df_2020_incident$county)

df_2020_df <- left_join(df_2020_df, df_2020_incident) %>% select(incident_id, civilian_officer, race_ethnic_group, received_force, county, state, city, zip_code, num_involved_civilians) %>% filter(
  received_force == "TRUE", civilian_officer == "Civilian"
) %>% mutate(year = 2020)


## 2021
df_2021_df <-  read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2021/UseofForce_Civilian-Officer_2021.csv")
colnames(df_2021_df) <- tolower(colnames(df_2021_df))

df_2021_incident <- read_csv("W:/Data/Crime and Justice/Police Violence/OpenJustice/2021/UseofForce_Incident_2021.csv")
colnames(df_2021_incident) <- tolower(colnames(df_2021_incident))
df_2021_incident$county <- tolower(df_2021_incident$county)

df_2021_df <- left_join(df_2021_df, df_2021_incident) %>% select(incident_id, civilian_officer, race_ethnic_group, received_force, county, state, city, zip_code, num_involved_civilians) %>% filter(
  received_force == "TRUE", civilian_officer == "Civilian"
) %>% mutate(year = 2021)


## merge all

df_all_years <- full_join(df_2016_df, df_2017_df)
df_all_years <- full_join(df_all_years, df_2018_df)
df_all_years <- full_join(df_all_years, df_2019_df)
df_all_years <- full_join(df_all_years, df_2020_df)
df_all_years <- full_join(df_all_years, df_2021_df)


# make first letter capital for county
df_all_years$county <- str_to_title(df_all_years$county)


# make race_ethnic_group values more uniform
df_all_years$race_ethnic_group <- sub("_", " ", df_all_years$race_ethnic_group)   # replace _ with space
df_all_years$race_ethnic_group <- sub(" /", ",", df_all_years$race_ethnic_group)  # replace / with comma

# Calculate Raw: Total number of incidents across all data years included NOT average per year ---------------------------------------------------------
#### NOTE: There is 1 row per civilian involved in each incident, but each of those rows reports the total # involved and has the same incident ID.
##### So if you sum the num_involved_civilians (which is the total # per incident) column, you will be double/triple-counting folks etc.

# total
df_calcs <- df_all_years %>% select(c(zip_code)) %>%
  group_by(zip_code) %>% 
  mutate(total_involved = n()) %>%  # calc number of civilians involved
  group_by(zip_code) %>% summarise(total_involved = min(total_involved)  # keep only distinct rows
                                 
  ) %>%
  left_join(
    
    # nh white alone
    df_all_years %>% 
      filter(race_ethnic_group == "white")  %>%
      group_by(zip_code) %>% 
      mutate(nh_white_involved = n()) %>%
      group_by(zip_code) %>% summarise(nh_white_involved = min(nh_white_involved))  # keep only distinct rows
    
  ) %>%
  left_join(
    
    # all aian
    df_all_years %>% 
      filter(grepl('american indian', race_ethnic_group)) %>%
      group_by(zip_code) %>% 
      mutate(aian_involved = n())   %>%
      group_by(zip_code) %>% summarise(aian_involved = min(aian_involved))  # keep only distinct rows
    
  ) %>%
  left_join(
    
    # all API
    # note from V3: the majority of NHPI incidents would fall under an api race_ethnic_group so leaving combined for now
    df_all_years %>% 
      filter(grepl('asian|islander', race_ethnic_group)) %>%
      group_by(zip_code) %>% 
      mutate(api_involved = n())    %>%
      group_by(zip_code) %>% summarise(api_involved = min(api_involved))  # keep only distinct rows
    
  ) %>% 
  left_join(
    
    # all black
    df_all_years %>% 
      filter(grepl('black', race_ethnic_group)) %>%
      group_by(zip_code) %>% 
      mutate(black_involved = n())  %>%
      group_by(zip_code) %>% summarise(black_involved = min(black_involved))  # keep only distinct rows
    
  ) %>%
  left_join(
    
    # latino 
    df_all_years %>% 
      filter(grepl('hispanic', race_ethnic_group)) %>%
      group_by(zip_code) %>% 
      mutate(latino_involved = n())   %>%
      group_by(zip_code) %>% summarise(latino_involved = min(latino_involved))  # keep only distinct rows
    
  )

# make NA = 0 
df_calcs[is.na(df_calcs)] = 0

#convert column 'b' from numeric to character
df_calcs$zip_code <- as.character(df_calcs$zip_code)


# Get Total Population ----------------------------------------------------

con2 <- connect_to_db("rda_shared_data")

ACS <- dbGetQuery(con2, "SELECT * FROM demographics.acs_5yr_dp05_multigeo_2020")

dbDisconnect(con2)


ACS$total_pop <- ACS$dp05_0001e 

ACS$nh_white_pop <- ACS$dp05_0077e 

ACS$black_pop <- ACS$dp05_0065e

ACS$aian_pop <- ACS$dp05_0066e

ACS$api_pop <- ACS$dp05_0068e + ACS$dp05_0067e

ACS$latino_pop <- ACS$dp05_0071e

ACS <- ACS %>% filter(geolevel %in% c("zcta")) %>% select(geoid, name, total_pop, nh_white_pop, black_pop, aian_pop, api_pop, latino_pop)

# remove "ZCTA5 "
ACS$name <- gsub("ZCTA5 ", "", ACS$name)

# join usof and pop data
df_calcs <- right_join(df_calcs, ACS, by = c("zip_code" = "name"))
dropvar <- names(df_calcs) %in% c("geoid")
df_calcs <- df_calcs[!dropvar]

#subset to AV ZIP codes
#from SOTC ECE query "W:\Project\RDA Team\Region 5 State of the Child\Queries\ece.sql"

# library(data.table)
av_zips <- c("93523","93551","93535","93534","93550","93552","93591","93536",
             "93544","93553","93543","92397","93563","93532","93243")
av_calcs <- df_calcs[which(df_calcs$zip_code %in% av_zips),] 

#set ZIP code's 93523 Null values to zeros to conservatively keep it's population in the denominator
av_calcs[is.na(av_calcs)] = 0

#sum for AV
av_calcs <- av_calcs %>% summarise(geo = "AV ZIP codes",
            total_involved = sum(total_involved),
            nh_white_involved = sum(nh_white_involved),
            aian_involved = sum(aian_involved),
            api_involved = sum(api_involved),
            black_involved = sum(black_involved),
            latino_involved = sum(latino_involved),
            
            total_pop = sum(total_pop),
            nh_white_pop = sum(nh_white_pop),
            black_pop = sum(black_pop),
            aian_pop = sum(aian_pop),
            api_pop = sum(api_pop),
            latino_pop = sum(latino_pop))

#calculate rates
data_yrs = 6
av_calcs <- av_calcs %>% mutate(
  total_rate = total_involved / (total_pop * data_yrs) * 100000,
  nh_white_rate = nh_white_involved / (nh_white_pop * data_yrs) * 100000,
  black_rate = black_involved / (black_pop * data_yrs) * 100000,
  aian_rate = aian_involved / (aian_pop * data_yrs) * 100000,
  api_rate = api_involved / (api_pop * data_yrs) * 100000,
  latino_rate = latino_involved / (latino_pop * data_yrs) * 100000
)


#refashion table
# select variables pop, num, rate
blackvars <- c("black_pop", "black_involved", "black_rate")
black_data <- av_calcs[blackvars] %>%
  rename(pop = black_pop, number = black_involved, per_100k = black_rate) %>%
  mutate(race_ethnicity = "Black")

whitevars <- c("nh_white_pop", "nh_white_involved", "nh_white_rate")
white_data <- av_calcs[whitevars]%>%
  rename(pop = nh_white_pop, number = nh_white_involved, per_100k = nh_white_rate) %>%
  mutate(race_ethnicity = "White")

totalvars <- c("total_pop", "total_involved", "total_rate")
total_data <- av_calcs[totalvars]%>%
  rename(pop = total_pop, number = total_involved, per_100k = total_rate) %>%
  mutate(race_ethnicity = "Total")

latinovars <- c("latino_pop", "latino_involved", "latino_rate")
latino_data <- av_calcs[latinovars]%>%
  rename(pop = latino_pop, number = latino_involved, per_100k = latino_rate) %>%
  mutate(race_ethnicity = "Latinx")

aianvars <- c("aian_pop", "aian_involved", "aian_rate")
aian_data <- av_calcs[aianvars]%>%
  rename(pop = aian_pop, number = aian_involved, per_100k = aian_rate) %>%
  mutate(race_ethnicity = "AIAN")

apivars <- c("api_pop", "api_involved", "api_rate")
api_data <- av_calcs[apivars]%>%
  rename(pop = api_pop, number = api_involved, per_100k = api_rate) %>%
  mutate(race_ethnicity = "API")

police_violence_data <- rbind(black_data, white_data, total_data, latino_data, aian_data, api_data) %>%
  select(race_ethnicity, everything())
