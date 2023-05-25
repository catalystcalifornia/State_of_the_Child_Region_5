## 3rd Grade Math Proficiency
# source: CAASPP 2021-22 https://caaspp-elpac.cde.ca.gov/caaspp/ResearchFileListSB?ps=true&lstTestYear=2021&lstTestType=B&lstCounty=00&lstDistrict=00000. Students scoring proficient or better on 3rd grade math (%).


# #install packages if not already installed ------------------------------
list.of.packages <- c("readr", "tidyr","dplyr", "janitor")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages)
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
# metadata: https://caaspp-elpac.cde.ca.gov/caaspp/research_fixfileformatSB21
#Data dictionary: https://caaspp-elpac.ets.org/caaspp/ResearchFileFormatSB?ps=true&lstTestYear=2022&lstTestType=B
############### PREP DATA ########################

#changed 2021 to 2022 & v2 to v1
url = "https://caaspp-elpac.cde.ca.gov/caaspp/researchfiles/sb_ca2022_all_ascii_v1.zip"
zipfile = "W:\\Data\\Education\\CAASPP\\2021-22\\sb_ca2022_all_ascii_v1.zip"
file = "W:\\Data\\Education\\CAASPP\\2021-22\\sb_ca2022_all_ascii_v1.txt"
exdir = "W:\\Data\\Education\\CAASPP\\2021-22"


# #Download and unzip data ------------------------------------------------
if(!file.exists(zipfile)) { download.file(url=url, destfile=zipfile) } 
if(!file.exists(file)) { unzip(zipfile, exdir = exdir) } 

#Read in data
all_student_groups <- read_fwf(file, na = c("*", ""),
                               fwf_widths(c(14,4,4,3,1,7,7,2,2,7,7,6,6,6,6,6,6,7,6,6,6,6,6,6,6,6,6,6,6,6,2),
                                          col_names = c("cdscode","filler","test_year","student_grp_id",
                                                        "test_type","total_tested_at_reporting_level","total_tested_with_scores",	
                                                        "grade","test_id","students_enrolled","students_tested","mean_scale_score",	
                                                        "percentage_standard_exceeded","percentage_standard_met","percentage_standard_met_and_above",
                                                        "percentage_standard_nearly_met","percentage_standard_not_met","students_with_scores",	
                                                        "area_1_percentage_above_standard","area_1_percentage_near_standard","area_1_percentage_below_standard",
                                                        "area_2_percentage_above_standard","area_2_percentage_near_standard","area_2_percentage_below_standard",
                                                        "area_3_percentage_above_standard","area_3_percentage_near_standard","area_3_percentage_below_standard",
                                                        "area_4_percentage_above_standard","area_4_percentage_near_standard","area_4_percentage_below_standard",
                                                        "type_id")))

all_student_groups <- all_student_groups %>% dplyr::select(-c(filler))

#Download and unzip school entities
url = "https://caaspp-elpac.cde.ca.gov/caaspp/researchfiles/sb_ca2022entities_ascii.zip"
zipfile2 = "W:\\Data\\Education\\CAASPP\\2021-22\\sb_ca2022entities_ascii.zip"
file2 = "W:\\Data\\Education\\CAASPP\\2021-22\\sb_ca2022entities_ascii.txt"

if(!file.exists(zipfile2)) { download.file(url=url, destfile=zipfile2) } 
if(!file.exists(file2)) { unzip(zipfile2, exdir = exdir) } 

#Read in entities
entities <- read_fwf(file2, fwf_widths(c(14,4,4,2,50), col_names = c("cdscode","filler","test_year","type_id", "geoname")))
entities <- entities %>% dplyr::select(-c(filler))

df <-left_join(x=all_student_groups,y=entities,by= c("cdscode", "test_year", "type_id")) %>%
  dplyr::select(cdscode, everything())
df <- df %>% dplyr::relocate(geoname, .after = cdscode)

# #join data to entities --------------------------------------------------
df <- rename(df, rate = percentage_standard_met_and_above, pop = students_with_scores, indicator = student_grp_id)

# Filter for 3rd grade, Math test, indicator/ethnicity subgroups, county/state level 
df_subset <- df %>% filter(grade == "03" & test_id == "02" & indicator %in% c("001","074","075","076","077","078","079","080","144","128","031","160","028","240","003","004","052")# see codes here: https://caaspp-elpac.ets.org/caaspp/ResearchFileFormatSB?ps=true&lstTestYear=2022&lstTestType=B
                           & type_id %in% c("06")) %>%    


# # calc raw/rate and screen ---------------------------------------------------------
#calculate raw
mutate(raw = round(pop * rate / 100, 0)) 
  
#pop screen
threshold = 0
df_subset <- df_subset %>%mutate(raw = ifelse(pop < threshold, NA, raw), rate = ifelse(pop < threshold, NA, rate))

#dplyr::select just fields we need
df_subset <- df_subset %>% dplyr::select(cdscode, type_id, indicator, rate, raw, pop) #removed geoname
#rename indicator/eth categories
df_subset$indicator <- gsub("001", "total", df_subset$indicator)
df_subset$indicator <- gsub("074", "nh_black", df_subset$indicator)
df_subset$indicator <- gsub("075", "nh_aian", df_subset$indicator)
df_subset$indicator <- gsub("076", "nh_asian", df_subset$indicator)
df_subset$indicator <- gsub("077", "nh_filipino", df_subset$indicator)
df_subset$indicator <- gsub("078", "latino", df_subset$indicator)
df_subset$indicator <- gsub("079", "nh_pacisl", df_subset$indicator)
df_subset$indicator <- gsub("080", "nh_white", df_subset$indicator)
df_subset$indicator <- gsub("144", "nh_twoormor", df_subset$indicator)
df_subset$indicator <- gsub("128", "disability", df_subset$indicator)
df_subset$indicator <- gsub("031", "disadvantaged", df_subset$indicator)
df_subset$indicator <- gsub("160", "ell", df_subset$indicator)
df_subset$indicator <- gsub("028", "migrant", df_subset$indicator)
df_subset$indicator <- gsub("240", "foster", df_subset$indicator)
df_subset$indicator <- gsub("003", "male", df_subset$indicator)
df_subset$indicator <- gsub("004", "female", df_subset$indicator) # didn't have non-binary option
df_subset$indicator <- gsub("052", "homeless", df_subset$indicator)
#filter for AV
df_subset <- df_subset %>% filter(cdscode=="19648570000000"| cdscode=="19651510000000"| 
                                    cdscode=="19646670000000"| cdscode=="19642460000000"| 
                                    cdscode=="19644770000000"| cdscode=="19753090000000"| 
                                    cdscode=="19646420000000"|cdscode=="19646260000000"| 
                                    cdscode=="19651020000000"|cdscode=="19101990000000")
df_subset$geoname <- NA

df_subset$geoname[df_subset$cdscode=="19648570000000"] <- 'Palmdale Elementary School District'
df_subset$geoname[df_subset$cdscode=="19651510000000"] <-  'Wilsona Elementary School District'
df_subset$geoname[df_subset$cdscode=="19646670000000"] <- 'Lancaster Elementary School District'
df_subset$geoname[df_subset$cdscode=="19642460000000"] <- 'Antelope Valley Union High School District'
df_subset$geoname[df_subset$cdscode=="19644770000000"] <- 'Eastside Union Elementary School District'
df_subset$geoname[df_subset$cdscode=="19753090000000"] <- 'Acton-Agua Dulce Unified School District'
df_subset$geoname[df_subset$cdscode=="19646420000000"] <- 'Keppel Union Elementary School District'
df_subset$geoname[df_subset$cdscode=="19646260000000"] <- 'Hughes-Elizabeth Lakes Union Elementary	School District'
df_subset$geoname[df_subset$cdscode=="19651020000000"] <- 'Westside Union Elementary School District'



# View(df_subset)

#pivot
df_wide <- df_subset %>% pivot_wider(names_from = indicator, names_glue = "{indicator}_{.value}", values_from = c(raw, pop, rate))

d <- df_wide %>% filter(!is.na(total_raw)) %>% dplyr::select(-c(cdscode, type_id)) %>% 
  mutate(across(ends_with("_rate") | ends_with("_raw") | ends_with("_pop"),
                ~ as.numeric((.)))) %>% as.data.frame() %>% adorn_totals("row")



# calculate total rates for AV

d$total_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$total_raw[d$geoname == 'Total']) || is.na(d$total_pop[d$geoname == 'Total'])|| (d$total_rate[d$geoname == 'Total']==0), NA, d$total_raw[d$geoname == 'Total']/d$total_pop[d$geoname == 'Total'] * 100)
d$nh_asian_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$nh_asian_raw[d$geoname == 'Total']) || is.na(d$nh_asian_pop[d$geoname == 'Total'])|| (d$nh_asian_rate[d$geoname == 'Total']==0), NA, d$nh_asian_raw[d$geoname == 'Total']/d$nh_asian_pop[d$geoname == 'Total'] * 100)
d$nh_black_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$nh_black_raw[d$geoname == 'Total']) || is.na(d$nh_black_pop[d$geoname == 'Total'])|| (d$nh_black_rate[d$geoname == 'Total']==0), NA, d$nh_black_raw[d$geoname == 'Total']/d$nh_black_pop[d$geoname == 'Total'] * 100)
d$nh_white_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$nh_white_raw[d$geoname == 'Total']) || is.na(d$nh_white_pop[d$geoname == 'Total'])|| (d$nh_white_rate[d$geoname == 'Total']==0), NA, d$nh_white_raw[d$geoname == 'Total']/d$nh_white_pop[d$geoname == 'Total'] * 100)
d$latino_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$latino_raw[d$geoname == 'Total']) || is.na(d$latino_pop[d$geoname == 'Total'])|| (d$latino_rate[d$geoname == 'Total']==0), NA, d$latino_raw[d$geoname == 'Total']/d$latino_pop[d$geoname == 'Total'] * 100)
d$nh_twoormor_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$nh_twoormor_raw[d$geoname == 'Total']) || is.na(d$nh_twoormor_pop[d$geoname == 'Total'])|| (d$nh_twoormor_rate[d$geoname == 'Total']==0), NA, d$nh_twoormor_raw[d$geoname == 'Total']/d$nh_twoormor_pop[d$geoname == 'Total'] * 100)
d$nh_pacisl_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$nh_pacisl_raw[d$geoname == 'Total']) || is.na(d$nh_pacisl_pop[d$geoname == 'Total'])|| (d$nh_pacisl_rate[d$geoname == 'Total']==0), NA, d$nh_pacisl_raw[d$geoname == 'Total']/d$nh_pacisl_pop[d$geoname == 'Total'] * 100)
d$nh_aian_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$nh_aian_raw[d$geoname == 'Total']) || is.na(d$nh_aian_pop[d$geoname == 'Total'])|| (d$nh_aian_rate[d$geoname == 'Total']==0), NA, d$nh_aian_raw[d$geoname == 'Total']/d$nh_aian_pop[d$geoname == 'Total'] * 100)
d$nh_filipino_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$nh_filipino_raw[d$geoname == 'Total']) || is.na(d$nh_filipino_pop[d$geoname == 'Total'])|| (d$nh_filipino_rate[d$geoname == 'Total']==0), NA, d$nh_filipino_raw[d$geoname == 'Total']/d$nh_filipino_pop[d$geoname == 'Total'] * 100)
d$disability_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$disability_raw[d$geoname == 'Total']) || is.na(d$disability_pop[d$geoname == 'Total'])|| (d$disability_rate[d$geoname == 'Total']==0), NA, d$disability_raw[d$geoname == 'Total']/d$disability_pop[d$geoname == 'Total'] * 100)
d$male_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$male_raw[d$geoname == 'Total']) || is.na(d$male_pop[d$geoname == 'Total'])|| (d$male_rate[d$geoname == 'Total']==0), NA, d$male_raw[d$geoname == 'Total']/d$male_pop[d$geoname == 'Total'] * 100)
d$female_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$female_raw[d$geoname == 'Total']) || is.na(d$female_pop[d$geoname == 'Total'])|| (d$female_rate[d$geoname == 'Total']==0), NA, d$female_raw[d$geoname == 'Total']/d$female_pop[d$geoname == 'Total'] * 100)
d$homeless_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$homeless_raw[d$geoname == 'Total']) || is.na(d$homeless_pop[d$geoname == 'Total'])|| (d$homeless_rate[d$geoname == 'Total']==0), NA, d$homeless_raw[d$geoname == 'Total']/d$homeless_pop[d$geoname == 'Total'] * 100)
d$migrant_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$migrant_raw[d$geoname == 'Total']) || is.na(d$migrant_pop[d$geoname == 'Total'])|| (d$migrant_rate[d$geoname == 'Total']==0), NA, d$migrant_raw[d$geoname == 'Total']/d$migrant_pop[d$geoname == 'Total'] * 100)
d$disadvantaged_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$disadvantaged_raw[d$geoname == 'Total']) || is.na(d$disadvantaged_pop[d$geoname == 'Total'])|| (d$disadvantaged_rate[d$geoname == 'Total']==0), NA, d$disadvantaged_raw[d$geoname == 'Total']/d$disadvantaged_pop[d$geoname == 'Total'] * 100)
d$ell_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$ell_raw[d$geoname == 'Total']) || is.na(d$ell_pop[d$geoname == 'Total'])|| (d$ell_rate[d$geoname == 'Total']==0), NA, d$ell_raw[d$geoname == 'Total']/d$ell_pop[d$geoname == 'Total'] * 100)
d$foster_rate[d$geoname == 'Total'] <-  ifelse(is.na(d$foster_raw[d$geoname == 'Total']) || is.na(d$foster_pop[d$geoname == 'Total'])|| (d$foster_rate[d$geoname == 'Total']==0), NA, d$foster_raw[d$geoname == 'Total']/d$foster_pop[d$geoname == 'Total'] * 100)


# Replace String with Another String
d$geoname[d$geoname == 'Total'] <- 'Antelope Valley Best Start Region 5'

av_table <- d %>% 
  dplyr::select(geoname, ends_with("_rate")) %>%  
  dplyr::rename("Black" = "nh_black_rate", "AIAN" = "nh_aian_rate", "NHPI" = "nh_pacisl_rate",
    "Asian" = "nh_asian_rate", "Filipino" ="nh_filipino_rate", "Latinx" ="latino_rate",
    "White" ="nh_white_rate", "Two or More indicators" ="nh_twoormor_rate", "Total" = "total_rate", 
    "Disability"="disability_rate", "Economically Disadvantaged"="disadvantaged_rate", 
    "Migrant"="migrant_rate", "Foster"="foster_rate", "ELL"="ell_rate", "Homeless"="homeless_rate", 
    "Female" = "female_rate", "Male" = "male_rate") %>% 
  mutate_at(vars(-c(geoname)), funs(round(., 1)))%>% 
  janitor::remove_empty(which = c("rows", "cols"), quiet = TRUE)

av_table <- av_table[order(sub("Antelope Valley Best Start Region 5", " ", av_table$geoname)), ]



# View(av_table)
AV <- av_table
AV <- AV[AV$geoname == 'Antelope Valley Best Start Region 5', ]

