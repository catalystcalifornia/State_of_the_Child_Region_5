## Calculate AV Eviction Filings Rates  ##
# Install packages if not already installed
list.of.packages <- c("data.table", "dplyr", "srvyr", "tidyr", "readxl", "janitor","naniar")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load libraries

library(data.table)
library(dplyr)
library(srvyr)
library(tidyr)
library(readxl)
library(janitor)
library(naniar)  # used for replace_with_na_at
options(scipen=999)


#set source for RC Functions script
source("W:/Project/RDA Team/Region 5 State of the Child/R/Tract_Wt_Avg_Functions.R")

##### GET INDICATOR DATA ######

# Load the data & prep data for weighted average function
#### data in the 'valid' file has already been screened for large year-on-year fluctuations by Evictions Lab
# original data saved to W from https://data-downloads.evictionlab.org/#estimating-eviction-prevalance-across-us/
df_orig <- fread("W:/Data/Housing/Eviction Lab/2000-2018/tract_proprietary_valid_2000_2018.csv", header = TRUE, data.table = FALSE)

# get the eviction data from 2016-2020 in this case 2014-2018 since those are the available data years, before that might be too far back
eviction_data <- df_orig %>% dplyr::filter((state == "California") & grepl("2013|2014|2015|2016|2017|2018", year)) %>% #select specific data years from tracts in CA
  mutate(tract_id = paste0("0", cofips), tract_name = gsub(" tract", "", tract), fips = paste0("0", fips)) %>% # format Census tract info 
  select(fips, tract_id, tract_name, year, filings) #select only neccesary columns
 ##view(eviction_data)
eviction_data$tract_name <- gsub('^Census Tract ', '', eviction_data$tract_name ) #reformat name
eviction_data$tract_name <- as.numeric(eviction_data$tract_name)
eviction_data <- eviction_data %>% mutate(tract_id = paste0(tract_id, tract_name)) %>%   #create tract id using the decimal numbers from the name and the numbers from the previous tract_id
select(-tract_name)

#load in the DP04 Antelope Valley population by census tract by Lucy Wilkerson (2016-2020)
av_data <- read_excel("W:/Project/RDA Team/Region 5 State of the Child/Data/ACS 2016-2020 WORKBOOK - for AV report.xlsx", sheet = "DP04") %>% 
  select("NEIGHBORHOOD", #select only useful columns
        "GEO_ID") %>% drop_na() # renter occupied is going to be our base for renter data. conversely for foreclosures, we would use owner occupied
av_data$tract_id <- as.character(gsub('^(.{9})(.*)$', '\\1.\\2', av_data$GEO_ID)) #reformat geoid for later combining
av_data <- av_data %>% filter(NEIGHBORHOOD == "LANCASTER" | NEIGHBORHOOD == "PALMDALE" |  #filtering out la county, california etc because it won't work for the weighed average process
                                NEIGHBORHOOD == "QUARTZ HILL" | NEIGHBORHOOD == "EAST ANTELOPE VALLEY" | 
                                NEIGHBORHOOD == "WEST ANTELOPE VALLEY" | NEIGHBORHOOD == "LAKE LOS ANGELES") %>% rename("tract_name" = "NEIGHBORHOOD")

# join data from the two sources to later get the total raw and pop by census tract
df <- av_data %>% left_join(eviction_data, by="tract_id") %>% 
  select(-c(GEO_ID))

######### Screening / Data Exploration ##########

# get median # of non-na filing values grouped by tract/year. then calc diff from median #. then calc (%) diff from median.
med <- na.omit(df) %>% group_by(tract_id, tract_name) %>% # 
  dplyr::count(tract_id, year) %>% 
  dplyr::rename(non_na_count = n) %>% 
  dplyr::mutate(med_non_na_count = median(non_na_count)) %>% 
  dplyr::mutate(diff_from_med = non_na_count - med_non_na_count) %>% 
  dplyr::mutate(pct_diff_from_med = diff_from_med / med_non_na_count * 100)

# round numeric values.
med <- med %>% mutate_if(is.numeric, ~round(., 1))

# get count of data yrs with non-na filing_rate by tract.    
data_yrs <- filter(med, !year == 2018)  # remove 2018 data since they only report it for 1 tract in San Francisco
data_yrs <- data_yrs %>% group_by(tract_id, tract_name) %>% 
  mutate(num_yrs = n()) %>% 
  distinct(tract_id, tract_name, .keep_all = TRUE) %>% 
  select(tract_id, tract_name, num_yrs)

# add the calculated number of years of data per census tract to later calculate the avg number of evictions across years
df_join <- df %>% inner_join(data_yrs, by = c("tract_id", "tract_name")) 

screened <- filter(df_join, num_yrs > 0) # suppress data for counties with fewer than 1 years of data. Before it was set to 3 but that would have gotten rid of too much data for such a small geography.


###############################
#get target names 
targetgeo_names <- av_data %>% select(tract_name, GEO_ID) %>% rename("target_id"="tract_name", "sub_id"="GEO_ID")

#calculate sum and avg evictions for the AV
df_wide <- screened %>% group_by(fips, tract_name) %>%
  mutate(sum_eviction = sum(filings, na.rm = TRUE)) %>%  # total number of evictions over all data yrs available
  mutate(avg_eviction = sum_eviction / num_yrs) %>%  # avg annual number of evictions
  distinct(fips, tract_id, tract_name, sum_eviction, avg_eviction, .keep_all = FALSE)

df_wide <- filter(df_wide, sum_eviction != 0) # screen out tracts (n = 341) where all filings for all data years = NA, since these should be NA not 0's. there are no 0's in orig. data.


df_wide <- df_wide %>% plyr::rename(c("fips" = "sub_id")) %>% left_join(targetgeo_names, by="sub_id")

# rename to ind_df for WA functions script
ind_df <- df_wide  

############# tract CALCS ##################

###### DEFINE VALUES FOR FUNCTIONS ######

# set values for weighted average functions - You may need to update these
year <- c(2017)                   # define your pop data vintage
subgeo <- c('tract')              # define your sub geolevel: tract (unless the WA functions are adapted for a different subgeo)
targetgeolevel <- c('tract')     # define your target geolevel: tract (state is handled separately)
survey <- "acs5"                  # define which Census survey you want
pop_threshold <- 0               # define minimum pop threshold for pop screen #it was originally set to 200

########################################
##### CREATE tract GEOID & NAMES TABLE ######  You will NOT need this chunk if your indicator data table has target geolevel names already

# EXTRA STEP: Must define different vars_list than what's in WA functions, bc basis here is renter households by race, not population by race.
# select race/eth owner household variables: total, black, aian, asian, pacisl, other, twoormor, nh_white, latinx (All except Two+ and Latinx are 1 race alone)
vars_list_custom <- c("B25003_003", "B25003B_003", "B25003C_003", "B25003D_003", "B25003E_003", "B25003F_003", "B25003G_003", "B25003H_003", "B25003I_003")

targetgeo_names <- targetgeo_names %>% mutate(target_name = target_id)

##### GET SUB GEOLEVEL POP DATA ######
pop <- update_detailed_table(vars = vars_list_custom, yr = year, srvy = survey)  # subgeolevel pop

# transform pop data to wide format 
pop_wide <- lapply(pop, to_wide)
#### add target_id field, you may need to update this bit depending on the sub and target_id's in the data you're using
pop_wide <- as.data.frame(pop_wide) %>% 
  dplyr::rename(sub_id = GEOID) %>%  # rename to generic column name for WA functions
  right_join(targetgeo_names, by="sub_id") %>% drop_na(NAME)
##view(pop_wide)                        


############### CUSTOMIZED VERSION OF TARGETGEO_POP FUNCTION HERE THAT WORKS WITH RENTER HOUSEHOLDS AS POP BASIS #######

# select pop estimate columns and rename to RC names
b <- select(pop_wide, sub_id, target_id, ends_with("e"), -NAME, target_name)

# aggregate sub geolevel pop to target geolevel
c <- b %>% group_by(target_id) %>% summarise_if(is.numeric, sum)
colnames(c) <- c('target_id', 'total_target_pop', 'black_target_pop', 'aian_target_pop', 'asian_target_pop', 'pacisl_target_pop', 'other_target_pop', 'twoormor_target_pop', 'nh_white_target_pop', 'latino_target_pop')

# count number of sub geolevels  per target geolevel and join to target geolevel pop
d <- b %>% dplyr::count(target_id)
c <- c %>% left_join(d, by = "target_id")

# join target geolevel pop and sub geolevel counts to df, drop margin of error cols, rename tract pop cols
e <- select(pop_wide, sub_id, target_id, geolevel, ends_with("e"), -NAME, -target_name) 
names(e) <- c('sub_id', 'target_id', 'geolevel', 'total_sub_pop', 'black_sub_pop', 'aian_sub_pop', 'asian_sub_pop', 'pacisl_sub_pop', 'other_sub_pop', 'twoormor_sub_pop', 'nh_white_sub_pop', 'latino_sub_pop')
pop_df <- e %>% left_join(c, by = "target_id")

###################################


##### EXTRA STEP: Calc avg annual evictions per 100 renter hh's (rate) by tract bc WA avg should be calc'd using this, not avg # of evictions (raw)
ind_df <- ind_df %>% left_join(pop_df %>% select(sub_id, total_sub_pop), by = "sub_id") %>% 
  mutate(indicator = (avg_eviction / total_sub_pop) * 100)
ind_df <- ind_df %>% ungroup() %>% select(sub_id, indicator) 
# #view(ind_df)

##### tract WEIGHTED AVG CALCS ######

pct_df <- pop_pct(pop_df)   # calc pct of target geolevel pop in each sub geolevel
wa <- wt_avg(pct_df)        # calc weighted average and apply reliability screens
wa <- rename(wa, geoname = target_id)   # rename columns
wa <- wa %>% 
  replace_with_na_at(.vars = c("total_rate","black_rate", "asian_rate", "aian_rate", "pacisl_rate", "other_rate", "twoormor_rate", "nh_white_rate", "latino_rate"),
                     condition = ~.x == 0.00000000) %>% relocate(total_rate, .after = twoormor_rate) %>% relocate(total_pop, .after = twoormor_pop)

#visualizations ---

# calculate raw values (sum of evictions) by neighborhood then add it back into the total table
av_raw <- df_wide %>%
  ungroup() %>% 
  select(target_id, sum_eviction) %>% 
  group_by(target_id) %>% 
  mutate(raw = sum(sum_eviction, na.rm = TRUE)) %>% select(-sum_eviction) %>% distinct(target_id, raw) %>% rename("geoname"="target_id")

# create a totals table by neighborhood
av_count <- wa %>% select(geoname, total_rate, total_pop) %>% 
  left_join(av_raw, by="geoname") %>% remove_empty()

#calculate the total for AV Best Start Region 5 using the same process
av_tot <- screened %>%
  mutate(sum_eviction = sum(filings, na.rm = TRUE)) %>%  # total number of evictions over all data yrs available
  mutate(avg_eviction = sum_eviction / num_yrs) %>%  # avg annual number of evictions
  distinct(fips, tract_id, tract_name, sum_eviction, avg_eviction, .keep_all = FALSE) %>% 
  select(sum_eviction) %>%
  mutate(avg = sum(mean(sum_eviction)),
        raw = sum(as.numeric(av_count$raw)),
        total_pop = sum(as.numeric(av_count$total_pop))) %>% 
  select(-sum_eviction) %>% 
  distinct() %>%
  mutate(geoname = "ANTELOPE VALLEY BEST START REGION 5",
         total_rate = avg/total_pop*100) %>%
  select(-avg)

av_count2 <- rbind(av_count, av_tot) %>% 
  rename("Geography" = "geoname", "Total Rate Per 100 People (%)" = "total_rate", 
         "Total Households" = "total_pop", "Eviction Count" = 'raw')
av_count2 <- av_count2[order(sub("ANTELOPE VALLEY BEST START REGION 5", " ", av_count2$Geography)), ]


# create rates table by race. Please note that its calculated with the average number of evictions instead of the sum of evictions
av_table <- wa %>% select(geoname, ends_with("_rate")) %>%
  mutate_at(vars(-geoname), funs(round(., 2))) %>% 
  rename("Geography" = "geoname", "Total" = "total_rate", 
         "AIAN"="aian_rate",	"Asian"="asian_rate",	
         "Black"="black_rate", "Latinx"="latino_rate",	
         "White"="nh_white_rate",	"Other"="other_rate",
         "NHPI"=	"pacisl_rate","Two or More"=	"twoormor_rate") %>% 
  remove_empty(which = c("rows", "cols"), quiet = TRUE)

av_table<-av_table[!(av_table$Geography=="EAST ANTELOPE VALLEY"),]

av_count2 <- av_count2 %>% 
  mutate_at(vars(-c(Geography, `Total Households`, `Eviction Count`)), funs(round(., 2))) 
