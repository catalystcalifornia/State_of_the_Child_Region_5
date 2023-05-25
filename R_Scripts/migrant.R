# Students in the Migrant Program
# Source: CDE 2021-22 https://dq.cde.ca.gov/dataquest/. Migrant Students rates 2021-22. Subgroups with total enrollment under 10 are excluded from the calculations. Migrant Students rate calculated as percent of enrollment for each subgroup.

#install packages if not already installed
list.of.packages <- c("openxlsx","dplyr","janitor") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## CDE Migrant Students at the School District level##
library(openxlsx)
library(dplyr)
library(janitor)

############### PREP DATA ########################

#get data for student indicator by race/ethnicity 

av_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Antelope Valley Union High School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17)) %>% 
  mutate(geoname = "Antelope Valley Union Joint High School District", type = "enroll")
av_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Antelope Valley Union High School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Antelope Valley Union Joint High School District",type = "indicator")
av_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Antelope Valley Union High School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Antelope Valley Union Joint High School District",type = "enroll")
av_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Antelope Valley Union High School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Antelope Valley Union Joint High School District",type = "indicator")

eu_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Eastside Union Elementary School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Eastside Union Elementary School District",type = "enroll")
eu_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Eastside Union Elementary School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Eastside Union Elementary School District",type = "indicator")
eu_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Eastside Union Elementary School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Eastside Union Elementary School District",type = "enroll")
eu_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Eastside Union Elementary School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Eastside Union Elementary School District",type = "indicator")

wu_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Westside Union Elementary School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Westside Union Elementary School District",type = "enroll")
wu_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Westside Union Elementary School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Westside Union Elementary School District",type = "indicator")
wu_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Westside Union Elementary School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Westside Union Elementary School District",type = "enroll")
wu_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Westside Union Elementary School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Westside Union Elementary School District",type = "indicator")

palm_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Palmdale Elementary School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Palmdale Elementary School District",type = "enroll")
palm_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Palmdale Elementary School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Palmdale Elementary School District",type = "indicator")
palm_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Palmdale Elementary School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Palmdale Elementary School District",type = "enroll")
palm_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Palmdale Elementary School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Palmdale Elementary School District",type = "indicator")

lan_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Lancaster Elementary School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Lancaster Elementary School District",type = "enroll")
lan_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Lancaster Elementary School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Lancaster Elementary School District",type = "indicator")
lan_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Lancaster Elementary School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Lancaster Elementary School District",type = "enroll")
lan_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Lancaster Elementary School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11)) %>% 
  mutate(geoname = "Lancaster Elementary School District",type = "indicator")

acton_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Acton-Agua Dulce Unified School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Acton-Agua Dulce Unified School District",type = "enroll")
acton_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Acton-Agua Dulce Unified School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Acton-Agua Dulce Unified School District",type = "indicator")
acton_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Acton-Agua Dulce Unified School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Acton-Agua Dulce Unified School District",type = "enroll")
acton_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Acton-Agua Dulce Unified School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11)) %>% 
  mutate(geoname = "Acton-Agua Dulce Unified School District",type = "indicator")

wilsona_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Wilsona Elementary School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Wilsona Elementary School District",type = "enroll")
wilsona_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Wilsona Elementary School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Wilsona Elementary School District",type = "indicator")
wilsona_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Wilsona Elementary School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Wilsona Elementary School District",type = "enroll")
wilsona_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Wilsona Elementary School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11)) %>% 
  mutate(geoname = "Wilsona Elementary School District",type = "indicator")

keppel_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Keppel Union Elementary School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
 mutate(geoname = "Keppel Union Elementary School District",type = "enroll")
keppel_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Keppel Union Elementary School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
 mutate(geoname = "Keppel Union Elementary School District",type = "indicator")
keppel_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Keppel Union Elementary School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
 mutate(geoname = "Keppel Union Elementary School District",type = "enroll")
keppel_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Keppel Union Elementary School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11)) %>% 
 mutate(geoname = "Keppel Union Elementary School District",type = "indicator")

hughes_gr_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Hughes-Elizabeth Lakes Union Elementary School District by Grade.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Hughes-Elizabeth Lakes Union Elementary School District",type = "enroll")
hughes_gr_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Hughes-Elizabeth Lakes Union Elementary School District by Grade.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:17))%>% 
  mutate(geoname = "Hughes-Elizabeth Lakes Union Elementary School District",type = "indicator")
hughes_race_enrollment = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Hughes-Elizabeth Lakes Union Elementary School District by Race.xlsx", sheet=1, startRow=1, rows=c(1:8), cols=c(1:11))%>% 
  mutate(geoname = "Hughes-Elizabeth Lakes Union Elementary School District",type = "enroll")
hughes_race_indicator = read.xlsx("W:/Project/RDA Team/Region 5 State of the Child/Data/CDE Hughes-Elizabeth Lakes Union Elementary School District by Race.xlsx", sheet=3, startRow=1, rows=c(1:8), cols=c(1:11)) %>% 
  mutate(geoname = "Hughes-Elizabeth Lakes Union Elementary School District",type = "indicator")


races <- rbind(av_race_enrollment, av_race_indicator, eu_race_enrollment, eu_race_indicator, wu_race_enrollment,
               wu_race_indicator, palm_race_enrollment, palm_race_indicator, lan_race_enrollment, lan_race_indicator,
               acton_race_enrollment, acton_race_indicator, wilsona_race_enrollment, wilsona_race_indicator, 
               keppel_race_enrollment, keppel_race_indicator,
               hughes_race_enrollment, hughes_race_indicator)

grades<- rbind(av_gr_enrollment, av_gr_indicator, eu_gr_enrollment, eu_gr_indicator, wu_gr_enrollment,
               wu_gr_indicator, palm_gr_enrollment, palm_gr_indicator, lan_gr_enrollment, lan_gr_indicator,
               acton_gr_enrollment, acton_gr_indicator, wilsona_gr_enrollment, wilsona_gr_indicator, 
               keppel_gr_enrollment, keppel_gr_indicator,
               hughes_gr_enrollment, hughes_gr_indicator)


source("W:/Project/RDA Team/Region 5 State of the Child/R/cde_functions.R")

# rename the categories
races <- cde_race_rename(races)
grades <- cde_grade_rename(grades)

#join races and grades to add geoids
df <- full_join(races, grades, by=c("geoname", "type", "year"))
df_wide <- cde_geoid(df)

####### clean and transform the raw csv data ----
raw_threshold <- 0

race_subset <- cde_race_clean(df_wide)
grade_subset <- cde_grade_clean(df_wide)

## manipulate data to make visuals #####
# race ----

# filter by year

race_2016 <- race_subset %>% dplyr::filter(year=="2015-16") %>% adorn_totals("row") %>% mutate(year = "2015-16", geoid = ifelse(geoid=="-", 'Total', geoid))
race_2017 <- race_subset %>% dplyr::filter(year=="2016-17") %>% adorn_totals("row") %>% mutate(year = "2016-17", geoid = ifelse(geoid=="-", 'Total', geoid))
race_2018 <- race_subset %>% dplyr::filter(year=="2017-18") %>% adorn_totals("row") %>% mutate(year = "2017-18", geoid = ifelse(geoid=="-", 'Total', geoid))
race_2019 <- race_subset %>% dplyr::filter(year=="2018-19") %>% adorn_totals("row") %>% mutate(year = "2018-19", geoid = ifelse(geoid=="-", 'Total', geoid))
race_2020 <- race_subset %>% dplyr::filter(year=="2019-20") %>% adorn_totals("row") %>% mutate(year = "2019-20", geoid = ifelse(geoid=="-", 'Total', geoid))
race_2021 <- race_subset %>% dplyr::filter(year=="2020-21") %>% adorn_totals("row") %>% mutate(year = "2020-21", geoid = ifelse(geoid=="-", 'Total', geoid))
race_2022 <- race_subset %>% dplyr::filter(year=="2021-22") %>% adorn_totals("row") %>% mutate(year = "2021-22", geoid = ifelse(geoid=="-", 'Total', geoid))

# Replace String with Another String 

race_2016 <- replace_total_race(race_2016)
race_2017 <- replace_total_race(race_2017)
race_2018 <- replace_total_race(race_2018)
race_2019 <- replace_total_race(race_2019)
race_2020 <- replace_total_race(race_2020)
race_2021 <- replace_total_race(race_2021)
race_2022 <- replace_total_race(race_2022)

race_all <- rbind(race_2016,race_2017,race_2018,race_2019,race_2020,race_2021,race_2022)
race_all <- race_all %>% mutate_at(vars(-c(year, geoid, geoname)), funs(round(., 1)))

# grade ----

grade_2016 <- grade_subset %>% dplyr::filter(year=="2015-16") %>% adorn_totals("row") %>% mutate(year = "2015-16", geoid = ifelse(geoid=="-", 'Total', geoid))
grade_2017 <- grade_subset %>% dplyr::filter(year=="2016-17") %>% adorn_totals("row") %>% mutate(year = "2016-17", geoid = ifelse(geoid=="-", 'Total', geoid))
grade_2018 <- grade_subset %>% dplyr::filter(year=="2017-18") %>% adorn_totals("row") %>% mutate(year = "2017-18", geoid = ifelse(geoid=="-", 'Total', geoid))
grade_2019 <- grade_subset %>% dplyr::filter(year=="2018-19") %>% adorn_totals("row") %>% mutate(year = "2018-19", geoid = ifelse(geoid=="-", 'Total', geoid))
grade_2020 <- grade_subset %>% dplyr::filter(year=="2019-20") %>% adorn_totals("row") %>% mutate(year = "2019-20", geoid = ifelse(geoid=="-", 'Total', geoid))
grade_2021 <- grade_subset %>% dplyr::filter(year=="2020-21") %>% adorn_totals("row") %>% mutate(year = "2020-21", geoid = ifelse(geoid=="-", 'Total', geoid))
grade_2022 <- grade_subset %>% dplyr::filter(year=="2021-22") %>% adorn_totals("row") %>% mutate(year = "2021-22", geoid = ifelse(geoid=="-", 'Total', geoid))

grade_2016 <- replace_total_grade(grade_2016)
grade_2017 <- replace_total_grade(grade_2017)
grade_2018 <- replace_total_grade(grade_2018)
grade_2019 <- replace_total_grade(grade_2019)
grade_2020 <- replace_total_grade(grade_2020)
grade_2021 <- replace_total_grade(grade_2021)
grade_2022 <- replace_total_grade(grade_2022)
grade_all <- rbind(grade_2016,grade_2017,grade_2018,grade_2019,grade_2020,grade_2021,grade_2022)
grade_all <- grade_all %>% mutate_at(vars(-c(year, geoid, geoname)), funs(round(., 1)))

# visualizations Prep ----

total_grade_only <- grade_all %>% dplyr::filter(geoid == 'Total')

race_2022 <- race_all %>% dplyr::filter(year == '2021-22') %>% dplyr::select(-year)

### show breakdown by race
# av_table <- race_2022 %>%  
#   dplyr::select(geoname, ends_with("_raw")) %>% 
#   dplyr::rename("Geography" = "geoname", "Overall Total" = "total_raw",
#                 "Black" = "nh_black_raw", "AIAN" = "nh_aian_raw",
#                 "Asian" = "nh_asian_raw", "Filipino" ="nh_filipino_raw", "Latinx" ="latino_raw",
#                 "NHPI" ="nh_pacisl_raw", "White" ="nh_white_raw", "Two or More Races" ="nh_twoormor_raw") %>% 
#   mutate_at(vars(-Geography), funs(round(., 1)))
# av_table <- av_table[order(sub("Antelope Valley Best Start Region 5", " ", av_table$Geography)), ]


##just show totals
av_table <- race_2022 %>% 
  dplyr::select(geoname, total_raw, total_rate) %>% 
  dplyr::rename("Geography" = "geoname", "2021-22 Count" = "total_raw", "Rate (%)"="total_rate") %>% 
  mutate_at(vars(-Geography), funs(round(., 1)))
av_table <- av_table[order(sub("Antelope Valley Best Start Region 5", " ", av_table$Geography)), ]
