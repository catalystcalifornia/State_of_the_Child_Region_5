# Antelope Valley Children by Race

# install packages if not already installed
list.of.packages <- c("dplyr","data.table", "srvyr", "tidyr", "sf","readxl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load packages -----
library(dplyr)
library(srvyr)
library(tidyr)
library(readxl)
library(sf)
library(data.table)


options(scipen = 100) # disable scientific notation

# connect to postgres database

source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("rda_shared_data")


## Load in ancestry codes ------

# create list of swana codes for PUMS
pums_swana_list<-list("Algerian","Arab","Assyrian","Bahraini","Berber","Chaldean","Egyptian","Emirati",
                      "Iranian","Iraqi","Israeli","Jordanian","Kurdish","Kuwaiti","Lebanese","Libyan",
                      "Middle Eastern","Moroccan","North African","Omani","Palestinian","Qatari","Saudi",
                      "Syriac","Syrian","Tunisian","Yazidi","Yemeni","Mideast","Saudi Arabian","Arabic",
                      "Other Arab","Libyan (2017 or later)","Kuwaiti (2017 or later)","Turkish","Sudanese",
                      "Afghan") # 2017 or later needed based on reviewing data dictionary and saw fields for Arabic and Other Arab

# import PUMS codes
anc_codes <- read_excel("W:/Data/Demographics/PUMS/CA_2017_2021/PUMS_Data_Dictionary_2017-2021_ANC1P.xlsx")%>%
  mutate_all(as.character) # created this excel document separate by opening PUMS Data Dictionary in excel and deleting everything but ancestry fields -- since ANC1P and ANC2P have same data values no need to do both


# filter PUMS codes for swana descriptions based on our swana_list
swana_codes<-anc_codes%>%filter(Description %in% pums_swana_list)



## Load in PUMS data
# Data Dictionary: W:\Data\Demographics\PUMS\CA_2017_2021\PUMS_Data_Dictionary_2017-2021.pdf 

####### GET PUMS DATA & PUMA-COUNTYCROSSWALK #######

# crosswalk
crosswalk <- st_read(con, query = "select county_id AS geoid, county_name AS geoname, puma, num_county from crosswalks.puma_county_2020")

# PUMS Data
## path where my data lives 
root <- "W:/Data/Demographics/PUMS/"

## Load the people PUMS data
ppl <- fread(paste0(root, "CA_2017_2021/psam_p06.csv"), header = TRUE, data.table = FALSE, 
             colClasses = list(character = c("PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", 
                                             "RACNH")))

# cut the number of variables so it isn't so slow
ppl <- ppl %>% dplyr::select("RT", "SERIALNO",, "DIVISION", "REGION", "ST", "PUMA", "ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", 
                        "RACAIAN", "RACPI", "RACNH", "AGEP", starts_with("PWGT"))

## Add state_geoid to ppl, add state_geoid to PUMA id, so it aligns with crosswalks.puma_county_2020
ppl$state_geoid <- "06"
ppl$puma_id <- paste0(ppl$state_geoid, ppl$PUMA)

## create list of replicate weights
repwlist = rep(paste0("PWGTP", 1:80))

# join county crosswalk 
# use left join function
ppl <- left_join(ppl, crosswalk, by=c("puma_id" = "puma"))    # specify the field join

# save copy unadultered prior to filtering
ppl_orig <- ppl
# ppl <- ppl_orig #use this line to refresh ppl to this point but keep it commented out in the meantime


####### Reclassify Race/Ethnicity #######


race_reclass <- function(x) {
  
  ## Recode race/eth
  x$latino <- "latino"
  x$latino[x$HISP=="01"] <- "not latino"
  x$latino <- as.factor(x$latino)
  
  x$aian <- "not aian"
  x$aian[x$RACAIAN==1] <- "aian"
  x$aian <- as.factor(x$aian)
  
  x$pacisl <- "not pacisl"
  x$pacisl[x$RACPI==1 | x$RACNH==1] <- "pacisl"
  x$pacisl <- as.factor(x$pacisl)
  
  #swana
  x$swana <- "not swana"
  x$swana[x$ANC1P%in% swana_codes$Code_1 | x$ANC2P%in% swana_codes$Code_1 | x$RAC2P%in% swana_codes$Code_1 | x$RAC3P%in% swana_codes$Code_1] <- "swana"
  x$swana <- as.factor(x$swana)
  
  
  
  # Check that latino/AIAN/NHPI/sswana columns are added and populated correctly
  #head(ppl)
  
  # code race groups
  x$race = as.factor(ifelse(x$RAC1P == 1 & x$latino =="not latino", "nh_white",
                            ifelse(x$RAC1P == 1 & x$latino =="latino", "white",
                                   ifelse(x$RAC1P == 2 & x$latino =="not latino", "nh_black",
                                          ifelse(x$RAC1P == 2 & x$latino =="latino", "black",
                                                 ifelse(x$RAC1P == 6 & x$latino =="not latino", "nh_asian",
                                                        ifelse(x$RAC1P == 6 & x$latino =="latino", "asian",
                                                               ifelse(x$RAC1P == 8 & x$latino =="not latino", "nh_other", 
                                                                      ifelse(x$RAC1P == 8 & x$latino =="latino", "other",
                                                                             ifelse(x$RAC1P == 9 & x$latino =="not latino", "nh_twoormor",
                                                                                    ifelse(x$RAC1P == 9 & x$latino =="latino", "twoormor",
                                                                                           NA)))))))))))
  
  # note that the latino includes all races. AIAN is AIAN alone/combo latino/non-latino, NHPI is alone/combo latino/non-latino, therefore NHPI and AIAN are double-counted in the latino category. Non-Latinx AIANs and NHPIs in combo are included in Two or More category.
  
  as.data.frame(x)
  
  return(x)  
}

ppl <- race_reclass(ppl)


####### Subset Data for Age Under 18 ########

# Filter data for pop of interest  ----
ppl$AGEP <- as.numeric(ppl$AGEP)
ppl <- filter(ppl, AGEP <18) # screen out people 18+
# max(ppl$AGEP) # check that ppl only includes under age 18


#Filter by Geographies -------

# select specific the three pumas that are in the AV in LA County. See geographies_ca.cb_2019_06_puma10_500k/rda_shared_data/postgres@aws-postgres-db
av_ppl <- ppl %>% dplyr::filter(puma_id=="0603703"|puma_id=="0603704"|puma_id=="0603701")


## CALCULATE COUNTY AND STATE ESTIMATES/CVS ETC. -----

# Define indicator and weight variables for function
# You must use to WGTP (if you are using psam_h06.csv and want housing units, like for Low Quality Housing) or PWGTP (if you want person units, like for Connected Youth)
key_indicator <- 'under18'  # update this to the indicator you are working with
weight <- 'PWGTP' 


############### AV CALCS ###############

# create survey design  for indicator
s <- av_ppl %>%  mutate(geoname = 'Antelope Valley')
ppl <- s %>%
  as_survey_rep(
    # deleted 'indicator' from variables
    variables = c(geoname, race, latino, aian, pacisl, swana),   # dplyr::select grouping variables
    weights = weight,                       # person weight
    repweights = repwlist,                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )


###### Summarize Data -----
###### calc for total
indicator_tot <-
  ppl %>%
  group_by(geoname) %>%
  summarise(
    num = survey_total(na.rm=T))

indicator_tot <- mutate(indicator_tot, "raceeth" = "total") # note I've added a column to assign race = total

head(indicator_tot)

# calculate by latino
indicator_lat <- ppl %>%
  group_by(geoname, latino) %>%   # group by latino and indicator
  summarise(
    num = survey_total(na.rm=T))        # get the (survey weighted) count for the numerators

head(indicator_lat)

# calculate by aian
indicator_aian <- ppl %>%
  #dplyr::filter(!is.na(indicator)) %>%
  group_by(geoname, aian) %>%
  summarise(
    num = survey_total(na.rm=T)) #%>%        # get the (survey weighted) count for the numerators

head(indicator_aian)

# calculate by pacisl
indicator_pacisl <- ppl %>%
  group_by(geoname, pacisl) %>%
  summarise(
    num = survey_total(na.rm=T)) #%>%        # get the (survey weighted) count for the numerators

head(indicator_pacisl)

# calculate by swana
indicator_swana <- ppl %>%
  group_by(geoname, swana) %>%   # group by swana and indicator
  summarise(
    num = survey_total(na.rm=T)) #%>%        # get the (survey weighted) count for the numerators

head(indicator_swana)


### calc by race
indicator_race =
  ppl %>%
  dplyr::filter(!is.na(race)) %>%
  group_by(geoname, race) %>%
  summarise(
    num = survey_total(na.rm=T)) #%>%

head(indicator_race)


#####Combine the data frames into one ----
indicator_race$raceeth <- as.character(indicator_race$race) # make a new column called race, convert factor to character
indicator_lat$raceeth <- as.character(indicator_lat$latino)
indicator_aian$raceeth <- as.character(indicator_aian$aian)
indicator_pacisl$raceeth <- as.character(indicator_pacisl$pacisl)
indicator_swana$raceeth <- as.character(indicator_swana$swana)


# Combine race/eth estimates with total estimates ----

indicator_av <-
  bind_rows(
    indicator_race %>%
      dplyr::select(-race) %>%
      dplyr::filter(!raceeth %in% c("white", "asian", "black", "other", "twoormor")),      # keep only non-latino/aian/pacisl race groups
    indicator_lat %>%
      dplyr::select(-latino) %>%
      dplyr::filter(raceeth =="latino"),
    indicator_aian %>%
      dplyr::select(-aian) %>%
      dplyr::filter(raceeth =="aian"),
    indicator_pacisl %>%
      dplyr::select(-pacisl) %>%
      dplyr::filter(raceeth =="pacisl"),
    indicator_swana %>%
      dplyr::select(-swana) %>%
      dplyr::filter(raceeth =="swana"),
    indicator_tot)


indicator_av <-indicator_av %>% as.data.frame()

# convert long format to wide
rc_indicator_av <- indicator_av %>%
  
  # convert to wide format
  pivot_wider(id_cols = geoname,
              names_from = raceeth,
              values_from = c("num")) %>%
  
  as.data.frame()



# SCREEN DATA and calculate rate and raw values -----


rc_av <- rc_indicator_av

# Define threshold variables
raw_rate_threshold <- 0

screened <- rc_av  %>%                # screen joined table
  mutate(
    latino_rate = ifelse((rc_av$latino/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$latino/rc_av$total*100)),
    nh_white_rate = ifelse((rc_av$nh_white/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$nh_white/rc_av$total*100)),
    nh_black_rate = ifelse((rc_av$nh_black/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$nh_black/rc_av$total*100)),
    nh_asian_rate = ifelse((rc_av$nh_asian/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$nh_asian/rc_av$total*100)),
    aian_rate = ifelse((rc_av$aian/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$aian/rc_av$total*100)),
    pacisl_rate = ifelse((rc_av$pacisl/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$pacisl/rc_av$total*100)),
    nh_other_rate = ifelse((rc_av$nh_other/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$nh_other/rc_av$total*100)),
    nh_twoormor_rate = ifelse((rc_av$nh_twoormor/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$nh_twoormor/rc_av$total*100)),
    swana_rate = ifelse((rc_av$swana/rc_av$total*100) < raw_rate_threshold, NA, (rc_av$swana/rc_av$total*100)),
    
    
    total_raw = ifelse(rc_av$total < raw_rate_threshold, NA, rc_av$total),
    latino_raw = ifelse(rc_av$latino < raw_rate_threshold, NA, rc_av$latino),
    nh_white_raw = ifelse(rc_av$nh_white < raw_rate_threshold, NA, rc_av$nh_white),
    nh_black_raw = ifelse(rc_av$nh_black < raw_rate_threshold, NA, rc_av$nh_black),
    nh_asian_raw = ifelse(rc_av$nh_asian < raw_rate_threshold, NA, rc_av$nh_asian),
    aian_raw = ifelse(rc_av$aian < raw_rate_threshold, NA, rc_av$aian),
    pacisl_raw = ifelse(rc_av$pacisl < raw_rate_threshold, NA, rc_av$pacisl),
    nh_other_raw = ifelse(rc_av$nh_other < raw_rate_threshold, NA, rc_av$nh_other),
    nh_twoormor_raw = ifelse(rc_av$nh_twoormor < raw_rate_threshold, NA, rc_av$nh_twoormor),
    swana_raw = ifelse(rc_av$swana < raw_rate_threshold, NA, rc_av$swana),
    
  )


d <- screened %>% dplyr::select(geoname, ends_with("_raw"), ends_with("_rate"))
  