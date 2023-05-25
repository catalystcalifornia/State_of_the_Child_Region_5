source("W:\\RDA Team\\R\\credentials_source.R") #protect API credentials so they do not end up on GitHub
library(tidycensus)

cde_race_rename <- function(x){
  #rename variables to fit our naming convention
  x <- dplyr::rename(x, c(
    "year" = "Academic.Year",
    "total" = "Total",
    "nh_black" = "African.American",
    "nh_aian" = "American.Indian.or.Alaska.Native",
    "nh_asian" = "Asian",
    "nh_filipino" = "Filipino",
    "latino" = "Hispanic.or.Latino",
    "nh_pacisl" = "Pacific.Islander",
    "nh_white" = "White",
    "nh_twoormor" = "Two.or.More.Races",
    "not_reported" = "Not.Reported"))
  # drop unpaired rows. Meaning drop the row if it does not have overall enrollment & enrollment by subgroup data for that row.
  races <- x %>% group_by(geoname, year) %>% mutate(n=n())%>% dplyr::filter(n==2) %>% ungroup() %>% dplyr::select(-n)
  return(races)
}


cde_grade_rename <- function(x){
  #rename variables to fit our naming convention
  x <- dplyr::rename(x, c(
    "year" = "Academic.Year",
    "total_grade" = "Total"))
  # drop unpaired rows. Meaning drop the row if it does not have overall enrollment & enrollment by subgroup data
  grades <- x %>% group_by(geoname, year) %>% mutate(n=n())%>% dplyr::filter(n==2) %>% ungroup() %>% dplyr::select(-n)
  return(grades)
}


cde_geoid <- function(x){
  
  #get census geoids
  
  census_api_key(census_key1, overwrite=TRUE) #call API from our password script so it is not on our repo
  
  unified <- get_acs(geography = "school district (unified)", #unified school districts data
                     variables = c("B01001_001"), 
                     state = "CA", 
                     year = 2020)
  
  unified <- unified[,1:2]
  unified$NAME <- gsub(", California", "", unified$NAME)
  names(unified) <- c("geoid", "geoname")
  # View(unified)
  
  elementary <- get_acs(geography = "school district (elementary)", #elementary school districts data
                        variables = c("B01001_001"), 
                        state = "CA", 
                        year = 2020)
  
  elementary <- elementary[,1:2]
  elementary$NAME <- gsub(", California", "", elementary$NAME)
  names(elementary) <- c("geoid", "geoname")
  # View(elementary)
  
  secondary <- get_acs(geography = "school district (secondary)", #high school districts data
                       variables = c("B01001_001"), 
                       state = "CA", 
                       year = 2020)
  
  secondary <- secondary[,1:2]
  secondary$NAME <- gsub(", California", "", secondary$NAME)
  names(secondary) <- c("geoid", "geoname")
  # View(secondary)
  
  #combine them so you have geoids for all school district types
  school_district <- rbind(unified,elementary,secondary)
  # View(school_district)
  
  #add geoids to the dataframe
  x <- left_join(x, school_district,by="geoname")
  
  
  df_wide <- x %>%
    pivot_wider(names_from=type, values_from=c(total,nh_black,nh_aian,nh_asian,nh_filipino,latino,nh_pacisl,    
                                               nh_white,nh_twoormor,not_reported,total_grade,Grade.K,Grade.1,     
                                               Grade.2,Grade.3,Grade.4,Grade.5,Grade.6,Grade.7,Grade.8,Ungr.Elem,   
                                               Grade.9,Grade.10,Grade.11,Grade.12,Ungr.Sec), names_glue = "{type}_{.value}")
  
  return(df_wide)
}


####### clean and transform the raw csv data ----


cde_race_clean <- function(x){
  df_race_wide <- x %>% 
    mutate( total_raw = ifelse(indicator_total < raw_threshold, NA, indicator_total),   # pop screen for _raw columns where a group's enrollment is less than 0, it was originally 50 f0r the county level
            nh_black_raw = ifelse(indicator_total < raw_threshold, NA, indicator_nh_black),
            nh_aian_raw = ifelse(indicator_nh_aian < raw_threshold, NA, indicator_nh_aian),
            nh_asian_raw = ifelse(indicator_nh_asian < raw_threshold, NA, indicator_nh_asian),
            nh_filipino_raw = ifelse(indicator_nh_filipino < raw_threshold, NA, indicator_nh_filipino),
            latino_raw = ifelse(indicator_latino < raw_threshold, NA, indicator_latino),
            nh_pacisl_raw = ifelse(indicator_nh_pacisl < raw_threshold, NA, indicator_nh_pacisl),
            nh_white_raw = ifelse(indicator_nh_white < raw_threshold, NA, indicator_nh_white),
            nh_twoormor_raw = ifelse(indicator_nh_twoormor < raw_threshold, NA, indicator_nh_twoormor),
            
            
            # create pop columns from existing data
            total_pop = enroll_total,
            nh_black_pop = enroll_nh_black,
            nh_aian_pop = enroll_nh_aian,
            nh_asian_pop = enroll_nh_asian,
            nh_filipino_pop = enroll_nh_filipino,
            latino_pop = enroll_latino,
            nh_pacisl_pop = enroll_nh_pacisl,
            nh_white_pop = enroll_nh_white,
            nh_twoormor_pop = enroll_nh_twoormor,
            
            # calculate _rate column if neither _raw nor _pop columns equals NA or _pop is zero. The formula is raw/pop *100   
            total_rate = ifelse(is.na(total_raw) | is.na(total_pop) | total_pop == 0, NA, (total_raw / total_pop)*100),
            nh_black_rate = ifelse(is.na(nh_black_raw) | is.na(nh_black_pop) | nh_black_pop == 0, NA, (nh_black_raw / nh_black_pop)*100),
            nh_aian_rate = ifelse(is.na(nh_aian_raw) | is.na(nh_aian_pop) |  nh_aian_pop == 0, NA, (nh_aian_raw / nh_aian_pop)*100),
            nh_asian_rate = ifelse(is.na(nh_asian_raw) | is.na(nh_asian_pop) | nh_asian_pop == 0, NA, (nh_asian_raw / nh_asian_pop)*100),
            nh_filipino_rate = ifelse(is.na(nh_filipino_raw) | is.na(nh_filipino_pop) | nh_filipino_pop == 0, NA, (nh_filipino_raw / nh_filipino_pop)*100),
            latino_rate = ifelse(is.na(latino_raw) | is.na(latino_pop) | latino_pop == 0, NA, (latino_raw / latino_pop)*100),
            nh_pacisl_rate = ifelse(is.na(nh_pacisl_raw) | is.na(nh_pacisl_pop) | nh_pacisl_pop == 0, NA, (nh_pacisl_raw / nh_pacisl_pop)*100),
            nh_white_rate = ifelse(is.na(nh_white_raw) | is.na(nh_white_pop) | nh_white_pop == 0, NA, (nh_white_raw / nh_white_pop)*100),
            nh_twoormor_rate = ifelse(is.na(nh_twoormor_raw) | is.na(nh_twoormor_pop) | nh_twoormor_pop == 0, NA, (nh_twoormor_raw / nh_twoormor_pop)*100)
            

    )
  
  ########## create a df with only the newly calculated columns ----
  race_subset <- dplyr::select(df_race_wide, year, geoid, geoname, ends_with("_pop"), ends_with("_raw"), ends_with("_rate")) %>% 
    drop_na(geoid)
  race_subset <- race_subset[order(race_subset$geoname),]
  return(race_subset)
  
}



cde_grade_clean <- function(x){
  df_grade_wide <- x  %>% 
    mutate( total_raw = ifelse(indicator_total_grade < raw_threshold, NA, indicator_total_grade),   #pop screen for _raw columns where a group's enrollment is less than 0, it was originally 50 f0r the county level
            kinder_raw = ifelse(indicator_Grade.K < raw_threshold, NA, indicator_Grade.K),
            grade1_raw = ifelse(indicator_Grade.1 < raw_threshold, NA, indicator_Grade.1),
            grade2_raw = ifelse(indicator_Grade.2 < raw_threshold, NA, indicator_Grade.2),
            grade3_raw = ifelse(indicator_Grade.3 < raw_threshold, NA, indicator_Grade.3),
            grade4_raw = ifelse(indicator_Grade.4 < raw_threshold, NA, indicator_Grade.4),
            grade5_raw = ifelse(indicator_Grade.5 < raw_threshold, NA, indicator_Grade.5),
            grade6_raw = ifelse(indicator_Grade.6 < raw_threshold, NA, indicator_Grade.6),
            grade7_raw = ifelse(indicator_Grade.7 < raw_threshold, NA, indicator_Grade.7),
            grade8_raw = ifelse(indicator_Grade.8 < raw_threshold, NA, indicator_Grade.8),
            grade9_raw = ifelse(indicator_Grade.9 < raw_threshold, NA, indicator_Grade.9),
            grade10_raw = ifelse(indicator_Grade.10 < raw_threshold, NA, indicator_Grade.10),
            grade11_raw = ifelse(indicator_Grade.11 < raw_threshold, NA, indicator_Grade.11),
            grade12_raw = ifelse(indicator_Grade.12 < raw_threshold, NA, indicator_Grade.12),
            ungr_raw = ifelse(indicator_Ungr.Sec < raw_threshold, NA, indicator_Ungr.Sec),
            
            
            # create pop columns
            total_pop = enroll_total_grade,
            kinder_pop = enroll_Grade.K,
            grade1_pop = enroll_Grade.1,
            grade2_pop = enroll_Grade.2,
            grade3_pop = enroll_Grade.3,
            grade4_pop = enroll_Grade.4,
            grade5_pop = enroll_Grade.5,
            grade6_pop = enroll_Grade.6,
            grade7_pop = enroll_Grade.7,
            grade8_pop = enroll_Grade.8,
            grade9_pop = enroll_Grade.9,
            grade10_pop = enroll_Grade.10,
            grade11_pop = enroll_Grade.11,
            grade12_pop = enroll_Grade.12,
            ungr_pop = enroll_Ungr.Sec,
            
            # calculate _rate column if neither _raw nor _pop columns equals NA or _pop is zero. The formula is raw/pop *100        
            total_rate = ifelse(is.na(total_raw) | is.na(total_pop) | total_raw == 0 | total_pop == 0, NA, (total_raw / total_pop)*100),
            kinder_rate = ifelse(is.na(kinder_raw) | is.na(kinder_pop) |  kinder_pop == 0, NA, (kinder_raw / kinder_pop)*100),
            grade1_rate = ifelse(is.na(grade1_raw) | is.na(grade1_pop) |  grade1_pop == 0, NA, (grade1_raw / grade1_pop)*100),
            grade2_rate = ifelse(is.na(grade2_raw) | is.na(grade2_pop) |  grade2_pop == 0, NA, (grade2_raw / grade2_pop)*100),
            grade3_rate = ifelse(is.na(grade3_raw) | is.na(grade3_pop) |  grade3_pop == 0, NA, (grade3_raw / grade3_pop)*100),
            grade4_rate = ifelse(is.na(grade4_raw) | is.na(grade4_pop) |  grade4_pop == 0, NA, (grade4_raw / grade4_pop)*100),
            grade5_rate = ifelse(is.na(grade5_raw) | is.na(grade5_pop) |  grade5_pop == 0, NA, (grade5_raw / grade5_pop)*100),
            grade6_rate = ifelse(is.na(grade6_raw) | is.na(grade6_pop) |  grade6_pop == 0, NA, (grade6_raw / grade6_pop)*100),
            grade7_rate = ifelse(is.na(grade7_raw) | is.na(grade7_pop) |  grade7_pop == 0, NA, (grade7_raw / grade7_pop)*100),
            grade8_rate = ifelse(is.na(grade8_raw) | is.na(grade8_pop) |  grade8_pop == 0, NA, (grade8_raw / grade8_pop)*100),
            grade9_rate = ifelse(is.na(grade9_raw) | is.na(grade9_pop) |  grade9_pop == 0, NA, (grade9_raw / grade9_pop)*100),
            grade10_rate = ifelse(is.na(grade10_raw) | is.na(grade10_pop) |  grade10_pop == 0, NA, (grade10_raw / grade10_pop)*100),
            grade11_rate = ifelse(is.na(grade11_raw) | is.na(grade11_pop) |  grade11_pop == 0, NA, (grade11_raw / grade11_pop)*100),
            grade12_rate = ifelse(is.na(grade12_raw) | is.na(grade12_pop) |  grade12_pop == 0, NA, (grade12_raw / grade12_pop)*100),
            ungr_rate = ifelse(is.na(ungr_raw) | is.na(ungr_pop) | ungr_pop == 0, NA, (ungr_raw / ungr_pop)*100)
            

            
    )
  
  ########## create a df with only the newly calculated columns ----
  grade_subset <- dplyr::select(df_grade_wide, year, geoid, geoname, ends_with("_pop"), ends_with("_raw"), ends_with("_rate")) %>% 
    drop_na(geoid)
  grade_subset <- grade_subset[order(grade_subset$geoname),]
  return(grade_subset)
  
}

# Send tables to postgres. It references Leila's rc_functions script function to_postgres
send_to_postgres <- function(x){
  # Postgres connection setup
  con <- connect_to_db("region5stateofthechild")
  #indicator_table
  indicator_table <- as.data.frame(indicator_table)
  
  # make character vector for field types in postgresql db
  charvect = rep('numeric', dim(indicator_table)[2])
  
  # change data type for first three columns
  charvect[1:2] <- "varchar" # first two are characters for the geoid and names
  
  # add names to the character vector
  names(charvect) <- colnames(indicator_table)
  
  dbWriteTable(con, c("data", indicator_table_name), indicator_table,
               overwrite = FALSE, row.names = FALSE)
  
  #comment on table and columns
  comment <- paste0("COMMENT ON TABLE data.", indicator_table_name,  " IS '", indicator, " from ", source, ".';
                                                                          COMMENT ON COLUMN data.", indicator_table_name, ".indicator_id IS 'indicator fips';")
  print(comment)
  dbSendQuery(con, comment)

  dbDisconnect(con)
  return(x)
}

# Replace String with Another String 
replace_total_race <- function(x){
  
  # # change the data type to numeric if it is not numeric
  
  x <- x %>% mutate(across(ends_with("_rate") | ends_with("_raw") | ends_with("_pop"),
                           ~ as.numeric((.))))

  # Replace String with Another String
  x$geoname[x$geoid== 'Total'] <- 'Antelope Valley Best Start Region 5'
  
  # calculate total rates for AV
  # This function does says the following: If _raw, _pop, or _rate (where geoid is 'Total') is NA or _pop or _rate is zero (where geoid is 'Total') then new _rate is NA, 
  # else new _rate is calculated using _raw / _pop (where geoid is 'Total') *100 (to make it a percent).

  x$total_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$total_raw[x$geoid == 'Total']) || 
                                                is.na(x$total_pop[x$geoid == 'Total']) || 
                                                is.na(x$total_rate[x$geoid == 'Total'])|| 
                                                x$total_pop[x$geoid == 'Total']==0|| 
                                                x$total_rate[x$geoid == 'Total']==0, NA, 
                                              x$total_raw[x$geoid == 'Total']/x$total_pop[x$geoid == 'Total'] * 100)
  
  x$nh_asian_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$nh_asian_raw[x$geoid == 'Total']) || 
                                                   is.na(x$nh_asian_pop[x$geoid == 'Total']) || 
                                                   is.na(x$nh_asian_rate[x$geoid == 'Total'])|| 
                                                   x$nh_asian_pop[x$geoid == 'Total']==0|| 
                                                   x$nh_asian_rate[x$geoid == 'Total']==0, NA, 
                                                 x$nh_asian_raw[x$geoid == 'Total']/x$nh_asian_pop[x$geoid == 'Total'] * 100)
  
  x$nh_black_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$nh_black_raw[x$geoid == 'Total']) || 
                                                   is.na(x$nh_black_pop[x$geoid == 'Total']) || 
                                                   is.na(x$nh_black_rate[x$geoid == 'Total'])|| 
                                                   x$nh_black_pop[x$geoid == 'Total']==0|| 
                                                   x$nh_black_rate[x$geoid == 'Total']==0, NA, 
                                                 x$nh_black_raw[x$geoid == 'Total']/x$nh_black_pop[x$geoid == 'Total'] * 100)
  
  x$nh_white_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$nh_white_raw[x$geoid == 'Total']) || 
                                                   is.na(x$nh_white_pop[x$geoid == 'Total']) || 
                                                   is.na(x$nh_white_rate[x$geoid == 'Total'])|| 
                                                   x$nh_white_pop[x$geoid == 'Total']==0|| 
                                                   x$nh_white_rate[x$geoid == 'Total']==0, NA, 
                                                 x$nh_white_raw[x$geoid == 'Total']/x$nh_white_pop[x$geoid == 'Total'] * 100)
 
  x$latino_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$latino_raw[x$geoid == 'Total']) || 
                                                 is.na(x$latino_pop[x$geoid == 'Total']) || 
                                                 is.na(x$latino_rate[x$geoid == 'Total'])|| 
                                                 x$latino_pop[x$geoid == 'Total']==0|| 
                                                 x$latino_rate[x$geoid == 'Total']==0, NA, 
                                               x$latino_raw[x$geoid == 'Total']/x$latino_pop[x$geoid == 'Total'] * 100)
  
  x$nh_twoormor_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$nh_twoormor_raw[x$geoid == 'Total']) || 
                                                      is.na(x$nh_twoormor_pop[x$geoid == 'Total']) || 
                                                      is.na(x$nh_twoormor_rate[x$geoid == 'Total'])|| 
                                                      x$nh_twoormor_pop[x$geoid == 'Total']==0|| 
                                                      x$nh_twoormor_rate[x$geoid == 'Total']==0, NA, 
                                                    x$nh_twoormor_raw[x$geoid == 'Total']/x$nh_twoormor_pop[x$geoid == 'Total'] * 100)
  
  x$nh_pacisl_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$nh_pacisl_raw[x$geoid == 'Total']) || 
                                                    is.na(x$nh_pacisl_pop[x$geoid == 'Total']) || 
                                                    is.na(x$nh_pacisl_rate[x$geoid == 'Total'])|| 
                                                    x$nh_pacisl_pop[x$geoid == 'Total']==0|| 
                                                    x$nh_pacisl_rate[x$geoid == 'Total']==0, NA, 
                                                  x$nh_pacisl_raw[x$geoid == 'Total']/x$nh_pacisl_pop[x$geoid == 'Total'] * 100)
  
  x$nh_aian_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$nh_aian_raw[x$geoid == 'Total']) || 
                                                  is.na(x$nh_aian_pop[x$geoid == 'Total']) || 
                                                  is.na(x$nh_aian_rate[x$geoid == 'Total'])|| 
                                                  x$nh_aian_pop[x$geoid == 'Total']==0|| 
                                                  x$nh_aian_rate[x$geoid == 'Total']==0, NA, 
                                                x$nh_aian_raw[x$geoid == 'Total']/x$nh_aian_pop[x$geoid == 'Total'] * 100)
  
  x$nh_filipino_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$nh_filipino_raw[x$geoid == 'Total']) || 
                                                      is.na(x$nh_filipino_pop[x$geoid == 'Total']) || 
                                                      is.na(x$nh_filipino_rate[x$geoid == 'Total'])|| 
                                                      x$nh_filipino_pop[x$geoid == 'Total']==0 || 
                                                      x$nh_filipino_rate[x$geoid == 'Total']==0, NA, 
                                                    x$nh_filipino_raw[x$geoid == 'Total']/x$nh_filipino_pop[x$geoid == 'Total'] * 100)
  

  
  return(x)
}


replace_total_grade <- function(x){
  
  # Convert Nan and Inf values to NA
  x[sapply(x, is.nan)] <- NA
  x[sapply(x, is.infinite)] <- NA
  
  # change the data type to numeric if it is not numeric

  x <- x %>% mutate(across(ends_with("_rate") | ends_with("_raw") | ends_with("_pop"),
                           ~ as.numeric((.))))
  # Replace String with Another String
  
  x$geoname[x$geoid == 'Total'] <- 'Antelope Valley Best Start Region 5'
  
  
  ##### calculate total rates for AV 
  # This function does says the following: If _raw, _pop, or _rate (where geoid is 'Total') is NA or _pop or _rate is zero (where geoid is 'Total') then new _rate is NA, 
  # else new _rate is calculated using _raw / _pop (where geoid is 'Total') *100 (to make it a percent).
  x$total_rate[x$geoid == 'Total'] <-  ifelse(is.na(x$total_raw[x$geoid == 'Total']) || 
                                                is.na(x$total_pop[x$geoid == 'Total']) || 
                                                is.na(x$total_rate[x$geoid == 'Total'])|| 
                                                x$total_pop[x$geoid == 'Total']==0 || #removes the chance of undefined error
                                                x$total_rate[x$geoid == 'Total']==0, NA, 
                                              x$total_raw[x$geoid == 'Total']/x$total_pop[x$geoid == 'Total'] * 100)
  
  x$kinder_rate[x$geoid == 'Total'] <- ifelse(is.na(x$kinder_raw[x$geoid == 'Total']) || 
                                                is.na(x$kinder_pop[x$geoid == 'Total']) || 
                                                is.na(x$kinder_rate[x$geoid == 'Total'])|| 
                                                x$kinder_pop[x$geoid == 'Total']==0 || 
                                                x$kinder_rate[x$geoid == 'Total']==0, NA, 
                                              x$kinder_raw[x$geoid == 'Total']/x$kinder_pop[x$geoid == 'Total'] * 100)
  
  x$grade1_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade1_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade1_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade1_rate[x$geoid == 'Total'])|| 
                                                x$grade1_pop[x$geoid == 'Total']==0 || 
                                                x$grade1_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade1_raw[x$geoid == 'Total']/x$grade1_pop[x$geoid == 'Total'] * 100)
  
  x$grade2_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade2_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade2_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade2_rate[x$geoid == 'Total']) || 
                                                x$grade2_pop[x$geoid == 'Total']==0 || 
                                                x$grade2_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade2_raw[x$geoid == 'Total']/x$grade2_pop[x$geoid == 'Total'] * 100)
  
  x$grade3_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade3_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade3_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade3_rate[x$geoid == 'Total']) || 
                                                x$grade3_pop[x$geoid == 'Total']==0 || 
                                                x$grade3_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade3_raw[x$geoid == 'Total']/x$grade3_pop[x$geoid == 'Total'] * 100)
  
  x$grade4_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade4_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade4_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade4_rate[x$geoid == 'Total']) || 
                                                x$grade4_pop[x$geoid == 'Total']==0 || 
                                                x$grade4_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade4_raw[x$geoid == 'Total']/x$grade4_pop[x$geoid == 'Total'] * 100)
  
  x$grade5_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade5_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade5_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade5_rate[x$geoid == 'Total']) || 
                                                x$grade5_pop[x$geoid == 'Total']==0 || 
                                                x$grade5_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade5_raw[x$geoid == 'Total']/x$grade5_pop[x$geoid == 'Total'] * 100)
  
  x$grade6_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade6_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade6_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade6_rate[x$geoid == 'Total']) || 
                                                x$grade6_pop[x$geoid == 'Total']==0 || 
                                                x$grade6_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade6_raw[x$geoid == 'Total']/x$grade6_pop[x$geoid == 'Total'] * 100)
  
  x$grade7_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade7_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade7_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade7_rate[x$geoid == 'Total']) || 
                                                x$grade7_pop[x$geoid == 'Total']==0 || 
                                                x$grade7_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade7_raw[x$geoid == 'Total']/x$grade7_pop[x$geoid == 'Total'] * 100)
  
  x$grade8_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade8_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade8_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade8_rate[x$geoid == 'Total']) || 
                                                x$grade8_pop[x$geoid == 'Total']==0 || 
                                                x$grade8_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade8_raw[x$geoid == 'Total']/x$grade8_pop[x$geoid == 'Total'] * 100)
  
  x$grade9_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade9_raw[x$geoid == 'Total']) || 
                                                is.na(x$grade9_pop[x$geoid == 'Total']) || 
                                                is.na(x$grade9_rate[x$geoid == 'Total']) || 
                                                x$grade9_pop[x$geoid == 'Total']==0 || 
                                                x$grade9_rate[x$geoid == 'Total']==0, NA, 
                                              x$grade9_raw[x$geoid == 'Total']/x$grade9_pop[x$geoid == 'Total'] * 100)
  
  x$grade10_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade10_raw[x$geoid == 'Total']) || 
                                                 is.na(x$grade10_pop[x$geoid == 'Total']) || 
                                                 is.na(x$grade10_rate[x$geoid == 'Total']) || 
                                                 x$grade10_pop[x$geoid == 'Total']==0 || 
                                                 x$grade10_rate[x$geoid == 'Total']==0, NA, 
                                               x$grade10_raw[x$geoid == 'Total']/x$grade10_pop[x$geoid == 'Total'] * 100) 
  
  x$grade11_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade11_raw[x$geoid == 'Total']) || 
                                                 is.na(x$grade11_pop[x$geoid == 'Total']) || 
                                                 is.na(x$grade11_rate[x$geoid == 'Total']) || 
                                                 x$grade11_pop[x$geoid == 'Total']==0 || 
                                                 x$grade11_rate[x$geoid == 'Total']==0, NA, 
                                               x$grade11_raw[x$geoid == 'Total']/x$grade11_pop[x$geoid == 'Total'] * 100)
  
  x$grade12_rate[x$geoid == 'Total'] <- ifelse(is.na(x$grade12_raw[x$geoid == 'Total']) || 
                                                 is.na(x$grade12_pop[x$geoid == 'Total']) || 
                                                 is.na(x$grade12_rate[x$geoid == 'Total']) || 
                                                 x$grade12_pop[x$geoid == 'Total']==0 || 
                                                 x$grade12_rate[x$geoid == 'Total']==0, NA, 
                                               x$grade12_raw[x$geoid == 'Total']/x$grade12_pop[x$geoid == 'Total'] * 100)
  
  x$ungr_rate[x$geoid == 'Total'] <- ifelse(is.na(x$ungr_raw[x$geoid == 'Total']) || 
                                              is.na(x$ungr_rate[x$geoid == 'Total'])|| 
                                              is.na(x$ungr_pop[x$geoid == 'Total']) ||
                                              x$ungr_pop[x$geoid == 'Total']==0 ||
                                              x$ungr_rate[x$geoid == 'Total']==0, NA,
                                            x$ungr_raw[x$geoid == 'Total']/x$ungr_pop[x$geoid == 'Total'] * 100)

  
  return(x)
}


