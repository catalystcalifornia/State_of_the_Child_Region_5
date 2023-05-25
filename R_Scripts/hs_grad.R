# High School Graduation Rates 2021-22 School Year

# source: CDE 2020-21 https://www.cde.ca.gov/ds/ad/filesacgr.asp. Five-year adjusted cohort graduation rate.

#install packages if not already installed
list.of.packages <- c("readr","tidyr","dplyr","janitor")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(readr)
library(dplyr)
library(tidyr)
library(janitor)



############### PREP DATA ########################

#Get HS Grad, handle nas, ensure DistrictCode reads in right
# Data Dictionary: https://www.cde.ca.gov/ds/ad/filesacgr.asp

filepath = "https://www3.cde.ca.gov/demo-downloads/fycgr/fycgr22-v2.txt"

df <- read_delim(file = filepath, delim = "\t", na = c("*", ""),
                  col_types = cols(DistrictCode = col_character()))

#remove special characters from names
names(df) <- gsub("\\(", "", names(df))
names(df) <- gsub("\\)", "", names(df))
names(df) <- gsub(" ", "", names(df))

#dplyr::filter for county and state rows, all types of schools, and racial categories
df_subset <- df %>% dplyr::filter(AggregateLevel %in% c("D") & CountyCode == "19" & CharterSchool == "All" & DASS == "All" & 
                             ReportingCategory %in% c("TA", "RB", "RI", "RA", "RF", "RH", "RP", "RT", "RW","GM", # no data for non-binary "GX"
                                                      "GF","SE","SD","SS","SM","SF","SH" )) %>%

dplyr::filter(  DistrictCode=="64246"|
          DistrictCode=="64857"|
         DistrictCode=="75309"|
         DistrictCode=="64642") %>%
  
  #select just the fields we need
  dplyr::select(DistrictName, ReportingCategory, CohortStudents, RegularHSDiplomaGraduatesCount, RegularHSDiplomaGraduatesRate)
# View(df_subset)

df_subset$ReportingCategory <- gsub("TA", "total", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("RB", "nh_black", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("RI", "nh_aian", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("RA", "nh_asian", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("RF", "nh_filipino", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("RH", "latino", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("RP", "nh_pacisl", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("RT", "nh_twoormor", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("RW", "nh_white", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("SE", "ell", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("SD", "disabilities", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("SS", "socioeconomically_disadvantaged", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("SM", "migrant", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("SF", "foster", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("SH", "homeless", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("GM", "male", df_subset$ReportingCategory)
df_subset$ReportingCategory <- gsub("GF", "female", df_subset$ReportingCategory)

df_subset <- rename(df_subset,c("geoname" = "DistrictName")) %>% 
  dplyr::filter(!is.na(df_subset$RegularHSDiplomaGraduatesCount) & !is.na(df_subset$CohortStudents) & !is.na(df_subset$RegularHSDiplomaGraduatesRate) )

#format for column headers
df_subset <- rename(df_subset,
                    raw = RegularHSDiplomaGraduatesCount,
                    pop = CohortStudents,
                    rate = RegularHSDiplomaGraduatesRate)
# #View(df_subset)


#pop screen. suppress raw/rate for groups with fewer than 0 graduating students.
threshold = 0
df_subset <- df_subset %>%
              mutate(rate = ifelse(raw < threshold, NA, rate), raw = ifelse(raw < threshold, NA, raw))

#pivot
df_wide <- df_subset %>% pivot_wider(names_from = ReportingCategory, names_glue = "{ReportingCategory}_{.value}", 
                                     values_from = c(raw, pop, rate))

# sum all numerical rows to get sums for raw and pop for the overall Antelope Valley (all school districts)
df_wide <- df_wide %>% adorn_totals("row")


replace_total_race <- function(x){
  
  # # change the data type to numeric if it is not numeric
  
  x <- x %>% mutate(across(ends_with("_rate") | ends_with("_raw") | ends_with("_pop"),
                           ~ as.numeric((.))))

  
  # calculate total rates for AV
  
  x$total_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$total_raw) || is.na(x$total_rate), NA, x$total_raw[x$geoname == 'Total']/x$total_pop[x$geoname == 'Total'] * 100)
  x$nh_asian_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$nh_asian_raw) || is.na(x$nh_asian_rate), NA, x$nh_asian_raw[x$geoname == 'Total']/x$nh_asian_pop[x$geoname == 'Total'] * 100)
  x$nh_black_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$nh_black_raw) || is.na(x$nh_black_rate), NA, x$nh_black_raw[x$geoname == 'Total']/x$nh_black_pop[x$geoname == 'Total'] * 100)
  x$nh_white_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$nh_white_raw) || is.na(x$nh_white_rate), NA, x$nh_white_raw[x$geoname == 'Total']/x$nh_white_pop[x$geoname == 'Total'] * 100)
  x$latino_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$latino_raw) || is.na(x$latino_rate), NA, x$latino_raw[x$geoname == 'Total']/x$latino_pop[x$geoname == 'Total'] * 100)
  x$nh_twoormor_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$nh_twoormor_raw) || is.na(x$nh_twoormor_rate), NA, x$nh_twoormor_raw[x$geoname == 'Total']/x$nh_twoormor_pop[x$geoname == 'Total'] * 100)
  x$nh_pacisl_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$nh_pacisl_raw) || is.na(x$nh_pacisl_rate), NA, x$nh_pacisl_raw[x$geoname == 'Total']/x$nh_pacisl_pop[x$geoname == 'Total'] * 100)
  x$nh_filipino_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$nh_filipino_raw) || is.na(x$nh_filipino_rate), NA, x$nh_filipino_raw[x$geoname == 'Total']/x$nh_filipino_pop[x$geoname == 'Total'] * 100)
  x$female_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$female_raw) || is.na(x$female_rate), NA, x$female_raw[x$geoname == 'Total']/x$female_pop[x$geoname == 'Total'] * 100)
  x$male_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$male_raw) || is.na(x$male_rate), NA, x$male_raw[x$geoname == 'Total']/x$male_pop[x$geoname == 'Total'] * 100)
  x$ell_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$ell_raw) || is.na(x$ell_rate), NA, x$ell_raw[x$geoname == 'Total']/x$ell_pop[x$geoname == 'Total'] * 100)
  x$disabilities_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$disabilities_raw) || is.na(x$disabilities_rate), NA, x$disabilities_raw[x$geoname == 'Total']/x$disabilities_pop[x$geoname == 'Total'] * 100)
  x$socioeconomically_disadvantaged_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$socioeconomically_disadvantaged_raw) || is.na(x$socioeconomically_disadvantaged_rate), NA, x$socioeconomically_disadvantaged_raw[x$geoname == 'Total']/x$socioeconomically_disadvantaged_pop[x$geoname == 'Total'] * 100)
  x$migrant_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$migrant_raw) || is.na(x$migrant_rate), NA, x$migrant_raw[x$geoname == 'Total']/x$migrant_pop[x$geoname == 'Total'] * 100)
  x$foster_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$foster_raw) || is.na(x$foster_rate), NA, x$foster_raw[x$geoname == 'Total']/x$foster_pop[x$geoname == 'Total'] * 100)
  x$homeless_rate[x$geoname == 'Total'] <-  ifelse(is.na(x$homeless_raw) || is.na(x$homeless_rate), NA, x$homeless_raw[x$geoname == 'Total']/x$homeless_pop[x$geoname == 'Total'] * 100)
  
  
   # Replace String with Another String
  x$geoname[x$geoname == 'Total'] <- 'Antelope Valley Best Start Region 5'
  return(x)
}


df_wide <- replace_total_race(df_wide) 
# View(df_wide)

av_table1 <- df_wide%>% dplyr::select(geoname, ends_with("_rate"))  %>% 
  mutate_at(vars(-geoname), funs(round(., 1)))%>% janitor::remove_empty()%>% dplyr::rename("Total"="total_rate", "Asian"="nh_asian_rate",
                                                                                           "Black"="nh_black_rate","Filipino"="nh_filipino_rate","Latinx"="latino_rate",
                                                                                           "Two or More Races"="nh_twoormor_rate","White"="nh_white_rate","AIAN"="nh_aian_rate", 
                                                                                           "Disability"="disabilities_rate", "Socioeconomically Disadvantaged"="socioeconomically_disadvantaged_rate", 
                                                                                           "Migrant"="migrant_rate", "Foster"="foster_rate", "ELL"="ell_rate", "Homeless"="homeless_rate", 
                                                                                           "Female" = "female_rate", "Male" = "male_rate")
av_table1 <- av_table1[av_table1$geoname == 'Antelope Valley Best Start Region 5', ]                                                                                           
av_table1$geoname[av_table1$geoname == 'Antelope Valley Best Start Region 5'] <- 'Total Rate'

av_table2 <- df_wide%>% dplyr::select(geoname, ends_with("_raw"))  %>% 
  mutate_at(vars(-geoname), funs(round(., 0)))%>% janitor::remove_empty()%>% dplyr::rename("Total"="total_raw", "Asian"="nh_asian_raw",
                                                                                           "Black"="nh_black_raw","Filipino"="nh_filipino_raw","Latinx"="latino_raw",
                                                                                           "Two or More Races"="nh_twoormor_raw","White"="nh_white_raw", "AIAN"="nh_aian_raw",
                                                                                           "Disability"="disabilities_raw", "Socioeconomically Disadvantaged"="socioeconomically_disadvantaged_raw", 
                                                                                           "Migrant"="migrant_raw", "Foster"="foster_raw", "ELL"="ell_raw", "Homeless"="homeless_raw", 
                                                                                           "Female" = "female_raw", "Male" = "male_raw")
av_table2 <- av_table2[av_table2$geoname == 'Antelope Valley Best Start Region 5', ]
av_table2$geoname[av_table2$geoname == 'Antelope Valley Best Start Region 5'] <- 'Total Count'

av_table <- rbind(av_table1, av_table2)%>% 
  dplyr::rename("Adjusted 5-Year 2020-2021"="geoname")
# View(av_table)

#split AV into separate table and format id, name columns
AV <- df_wide%>% dplyr::select(geoname, ends_with("_rate"))  %>%janitor::remove_empty()
AV <- AV[AV$geoname == 'Antelope Valley Best Start Region 5', ]
# View(AV)



