# Functions related to calculating weighted averages using ACS 5yr pop data at COUNTY / STATE level, as used in RACE COUNTS and other projects
# NOTE: These functions use population as the basis for WA. For some indicators, you would need to define your own vars_list in the indicator script as appropriate.
# Ex. In Foreclosure script, we need to use owner households by race, not pop by race as the basis.
# Could be copied and adapted for other geolevels like PUMA, City, etc. Those would require spatial joins to link subgeos and target geos.

# Target Geolevel is the geography we are calculating a weighted avg for; Sub Geolevel is the smaller geography we are using to calculate the weighted average.
# Sub Geolevel = usually census tract or ZIP/ZCTA that should provide total coverage of Target Geolevel. Target Geolevel = usually city, county, state, etc.


##### Set Up Workspace #####
library(dplyr)
library(tidycensus)
library(sf)
library(RPostgreSQL)
library(tidyr)
options(scipen=999)

source("W:\\RDA Team\\R\\credentials_source.R") #protect API credentials so they do not end up on GitHub
# create connection for rda database
con_zcta <- connect_to_db("rda_shared_data")


# get array of zctas in CA ---- Set to 2020 ZCTAs but you need to change that if you are pulling a different data year
#### get latest cbf ZCTA shapefile here - https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.2020.html . 
#Then clip to CA only using DP05 ZCTA list.
list_ca_zctas <- st_read(con_zcta, query = "select zcta5ce20 from geographies_ca.cb_2020_ca_zcta520_500k")
list_ca_zctas <- list_ca_zctas$zcta5ce20

# select race/eth pop variables (in this order): total, nh_white, nh_black, all aian, nh_asian, all pacisl, nh_other, nh_twoormor, latinx
vars_list <- c("DP05_0001", "DP05_0077", "DP05_0078", "DP05_0066", "DP05_0080", "DP05_0068", "DP05_0082", "DP05_0083", "DP05_0071")



### Get CA County FIPS and Names ####
county_names <- function(vars, yr, srvy){
  
  list(get_acs(geography = targetgeolevel, state = "CA", variables = vars, year = yr, survey = srvy, cache_table = TRUE))
  
}


### Get Sub Geolevel Pop ####
update_detailed_table <- function(vars, yr, srvy){
  if (subgeo == 'tract') {
    
    list(
      get_acs(geography = "tract", state = "CA", variables = vars, year = yr, survey = srvy, cache_table = TRUE) %>% 
        mutate(geolevel = "tract"))
  } else if (val$subgeo == 'zcta') {
    
    list(
      get_acs(geography = "zcta", variables = vars, year = yr, survey = srvy) %>% 
        mutate(geolevel = "zcta", zipname = gsub("ZCTA5 ", "", NAME)) %>%
        filter(zipname %in% list_ca_zctas))
  } 
  
}

### Make Sub Geolevel Pop Wide and Rename Columns ###
to_wide <- function(x) {
  as.data.frame(pop) %>%
    dplyr::rename(e = estimate, m = moe) %>%       # rename estimate and moe to be better for wide format
    # make wide
    pivot_wider(id_cols = c(GEOID, NAME, geolevel),
                names_from = variable,
                values_from = c(e, m),
                names_glue = "{variable}{.value}")   # specify the order of col names
  
}


##### count sub geos per target geo and join both to sub geolevel pop data #####
targetgeo_pop <- function(x) {
  # select pop estimate columns and rename to RC names
  b <- select(x, sub_id, target_id, ends_with("e"), -NAME)
  
  # aggregate sub geolevel pop to target geolevel
  c <- b %>% group_by(target_id) %>% summarise_if(is.numeric, sum)
  colnames(c) <- c('target_id', 'total_target_pop', 'aian_target_pop', 'pacisl_target_pop', 'latino_target_pop', 'nh_white_target_pop', 'nh_black_target_pop', 'nh_asian_target_pop', 'nh_other_target_pop', 'nh_twoormor_target_pop')
  
  # count number of sub geolevels  per target geolevel and join to target geolevel pop
  d <- b %>% dplyr::count(target_id)
  c <- c %>% left_join(d, by = "target_id")
  
  # join target geolevel pop and sub geolevel counts to df, drop margin of error cols, rename tract pop cols
  e <- select(x, sub_id, target_id, geolevel, ends_with("e"), -NAME) 
  names(e) <- c('sub_id', 'target_id', 'geolevel', 'total_sub_pop', 'aian_sub_pop', 'pacisl_sub_pop', 'latino_sub_pop', 'nh_white_sub_pop', 'nh_black_sub_pop', 'nh_asian_sub_pop', 'nh_other_sub_pop', 'nh_twoormor_sub_pop')
  x <- e %>% left_join(c, by = "target_id")
  
  return(x)
}  


##### calc sub geolevel pop as pct of each target geolevel pop #####
pop_pct <- function(x) {
  subgeo <- select(x, sub_id, target_id, n, ends_with("sub_pop"))
  targetgeo <- select(x, sub_id, ends_with("target_pop"))
  
  subgeo_long <- pivot_longer(subgeo, 4:ncol(subgeo), names_to="raceeth", values_to="sub_pop")       #pivot wide table to long
  targetgeo_long <- pivot_longer(targetgeo, 2:ncol(targetgeo), names_to="raceeth", values_to="target_pop")   #pivot wide table to long
  subgeo_long$raceeth <- gsub("_sub_pop","-",as.character(subgeo_long$raceeth))              # update to generic raceeth names
  targetgeo_long$raceeth <- gsub("_target_pop","-",as.character(targetgeo_long$raceeth))     # update to generic raceeth names
  
  # combine sub pop and target pop in long format
  pcts_long <- subgeo_long %>% left_join(targetgeo_long, by = c("sub_id" = "sub_id", "raceeth" = "raceeth"))  # join target and sub pops in long form
  pcts_long <- pcts_long %>% mutate(pct = ifelse(target_pop < pop_threshold, NA, (sub_pop / target_pop)), # calc pcts of each target geolevel pop per sub geolevel pop
                                    measure_pct=sub("-", "_pct_target_pop", raceeth))                              # create new column names
  pcts_wide <- pcts_long %>% select(sub_id, measure_pct, pct) %>%                   # pivot long table back to wide keeping only new columns
    pivot_wider(names_from=measure_pct, values_from=pct)
  x <- x %>% left_join(pcts_wide, by="sub_id")                    
  
  return(x)
}


#### You MUST HAVE ind_df table (with cols: sub_id and indicator) to run this function: Calc weighted averages at target geolevel and apply screens #####
wt_avg <- function(x) {
  
  a <- dplyr::select(x, target_id, sub_id, n, ends_with("_pct_target_pop"))
  a <- a %>% dplyr::left_join(select(ind_df, sub_id, indicator), by = "sub_id")               # join indicator data to pop data
  a <- a %>% dplyr::relocate(indicator, .after = n)                                           # move indicator column
  a_long <- pivot_longer(a, 5:ncol(a), names_to="raceeth", values_to="pct_pop") %>%           # pivot wide table to long
    dplyr::mutate(wa= indicator*pct_pop, raceeth=sub("_pct_target_pop", "-", raceeth))     # calc wa by tract, make raceeth names generic
  a_long <- dplyr::select(a_long, target_id, n, wa, raceeth)      
  grp_long = a_long %>% dplyr::group_by(target_id, raceeth, n) %>%                            # calc wa at 1 target geolevel                         
    dplyr::summarise(wtavg = sum(wa, na.rm = TRUE), .groups = 'drop')               
  
  targ_pop <- dplyr::select(x, target_id, ends_with("target_pop"), -ends_with("pct_target_pop"))      # get target pop
  targ_pop <- dplyr::distinct(targ_pop, .keep_all = FALSE)                                            # drop duplicate rows
  targ_pop_long <- pivot_longer(targ_pop, cols = 2:ncol(targ_pop), names_to = "raceeth", values_to = "target_pop") %>% 
    dplyr::mutate(raceeth = gsub("_target_pop", "-", raceeth))                           # make raceeth names generic 
  
  grp_long <- grp_long %>% dplyr::left_join(targ_pop_long, by = c("target_id", "raceeth"))     # join target pop data to wt avgs
  grp_long <- grp_long %>% dplyr::mutate(wtavg = ifelse(target_pop < pop_threshold | n < 2, NA, wtavg),  # screen out wt avg values where target geo level has < pop_threshold people of a raceeth OR it has <2 subgeos
                                         raceeth = sub("-", "_rate", raceeth))                                            # update column names
  
  grp_wide <- grp_long %>% dplyr::select(target_id, raceeth, wtavg) %>%                        # pivot long table back to wide keeping only WA columns
    pivot_wider(names_from=raceeth, values_from=wtavg)
  grp_wide_pop <- grp_long %>% dplyr::select(target_id, raceeth, target_pop) %>%               # pivot long table back to wide keeping only target pop columns
    pivot_wider(names_from=raceeth, values_from=target_pop)
  colnames(grp_wide_pop) = gsub("_rate", "_pop", colnames(grp_wide_pop))                       # update target pop column names                            
  grp_wide <- grp_wide %>% left_join(grp_wide_pop, by = "target_id")                           # join target pop columns to WA table
  n_table <- select(a, target_id, n)
  grp_wide <- grp_wide %>% left_join(n_table, by = "target_id")
  grp_wide <- distinct(grp_wide, .keep_all = FALSE)                                            # remove dupe rows
  
  return(grp_wide)                        
}

###### STATE CALCS ######################

##################### USE ONLY IF YOUR SUBGEOS COVER THE WHOLE STATE: Calc weighted averages at STATE level and apply screens #####################

##### Get state pop ##### 
state_pop <- function(vars, yr, srvy){
  
  ca_pop <- do.call(rbind.data.frame, list(
    get_acs(
      geography = "state", state = "CA", variables = vars, year = yr, survey = srvy, cache_table = TRUE)))
  ca_pop <- ca_pop %>%  
    mutate(geolevel = "state")
  ca_pop_wide <- select(ca_pop, GEOID, NAME, variable, estimate) %>% pivot_wider(names_from=variable, values_from=estimate)
  colnames(ca_pop_wide) <- c('target_id', 'target_name', 'total_target_pop', 'aian_target_pop', 'pacisl_target_pop', 'latino_target_pop', 'nh_white_target_pop', 'nh_black_target_pop', 'nh_asian_target_pop', 'nh_other_target_pop', 'nh_twoormor_target_pop')
  
  return(ca_pop_wide)
  
}


##### Calc pct of each group's state pop within each sub geolevel #####
ca_pop_pct <- function(x) {
  subpop <- select(pop_wide, sub_id, ends_with("e"), target_id, -NAME)
  subpop$target_id <- '06'                                        # replace county target_id values w/ state-level target_id value
  colnames(subpop) <- c('sub_id', 'total_sub_pop', 'aian_sub_pop', 'pacisl_sub_pop', 'latino_sub_pop', 'nh_white_sub_pop', 'nh_black_sub_pop', 'nh_asian_sub_pop', 'nh_other_sub_pop', 'nh_twoormor_sub_pop', 'target_id')
  subpop <- subpop %>% relocate(target_id, .after = sub_id)     # move sub_id column
  subpop_long <- pivot_longer(subpop, 3:ncol(subpop), names_to="raceeth", values_to="sub_pop")
  subpop_long$raceeth <- gsub("_sub_pop","-",as.character(subpop_long$raceeth))              # update to generic raceeth names
  
  ca_pop_long <- pivot_longer(x, 3:ncol(x), names_to="raceeth", values_to="target_pop")
  ca_pop_long$raceeth <- gsub("_target_pop","-",as.character(ca_pop_long$raceeth))           # update to generic raceeth names
  
  subpop_long <- subpop_long %>% left_join(ca_pop_long, by=c("target_id" = "target_id", "raceeth" = "raceeth"))  # join target and sub pops in long form
  ca_pcts_long <- subpop_long %>% mutate(pct = ifelse(target_pop < pop_threshold, NA, (sub_pop / target_pop)),   # calc pcts of each target geolevel pop per sub geolevel pop
                                         measure_pct=sub("-", "_pct_target_pop", raceeth))           # create new column names
  ca_pcts_wide <- ca_pcts_long %>% select(sub_id, target_id, measure_pct, pct) %>%              # pivot long table back to wide keeping only new columns
    pivot_wider(names_from=measure_pct, values_from=pct)
  
  return(ca_pcts_wide)    
}


##### Calc state-level weighted averages ####
ca_wt_avg <- function(x) {
  
  a <- x %>% dplyr::left_join(select(ind_df, sub_id, indicator), by = "sub_id")               # join indicator data to pop data
  a <- a %>% dplyr::relocate(indicator, .after = target_id)                                        # move indicator column
  a_long <- pivot_longer(a, 4:ncol(a), names_to="raceeth", values_to="pct_pop") %>%           # pivot wide table to long
    dplyr::mutate(wa= indicator*pct_pop, raceeth=sub("_pct_target_pop", "-", raceeth))     # calc wa by tract, make raceeth names generic
  a_long <- dplyr::select(a_long, target_id, raceeth, wa)      
  grp_long = a_long %>% dplyr::group_by(target_id, raceeth) %>%                            # calc wa at 1 target geolevel                            
    dplyr::summarise(wtavg = sum(wa, na.rm = TRUE), .groups = 'drop')
  
  grp_wide <- grp_long %>% pivot_wider(names_from=raceeth, values_from=wtavg)                 # pivot long table back to wide keeping only WA columns
  colnames(grp_wide) = gsub("-", "_rate", colnames(grp_wide))                                 # rename wa columns to _rate 
  ca_grp_wide <- grp_wide %>% left_join(ca_pop_wide, by = "target_id") %>%                    # join state pop columns to WA table
    mutate(target_name = 'California', n=nrow(pop_wide))                         # add target_name and calc n (# of subgeos w/ data in state)
  colnames(ca_grp_wide) = gsub("_target", "", colnames(ca_grp_wide))                          # rename pop columns 
  
  return(ca_grp_wide)                        
}


