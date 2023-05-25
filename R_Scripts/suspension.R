#calculate suspension rates by student group, including race, for av districts
library(data.table)
library(dplyr)
library(stringr)

#read text file from webpage https://www.cde.ca.gov/ds/ad/filessd.asp
file <- fread("https://www3.cde.ca.gov/demo-downloads/discipline/suspension22-v2.txt")

# filter for District level enrollment and unduplicated suspensions in LA County (for charters and non-charters)
suspensions <- file %>% dplyr::filter(AggregateLevel=="D" & CountyName=="Los Angeles" & CharterYN=="All") %>%
  dplyr::select(DistrictName, ReportingCategory, CumulativeEnrollment, `Unduplicated Count of Students Suspended (Total)`)

# in av districts
suspensions <- dplyr::filter(suspensions, DistrictName %in%  c('Acton-Agua Dulce Unified', 
                                                        'Antelope Valley Union High', 
                                                        'Eastside Union Elementary',
                                                        'Hughes-Elizabeth Lakes Union Elementary',
                                                        'Keppel Union Elementary',
                                                        'Lancaster Elementary',
                                                        'Palmdale Elementary',
                                                        'Westside Union Elementary',
                                                        'Wilsona Elementary'))

# remove suppression asterisks
suspensions$CumulativeEnrollment <- stringr::str_replace(suspensions$CumulativeEnrollment, '\\*', '')
suspensions$`Unduplicated Count of Students Suspended (Total)` <- stringr::str_replace(suspensions$`Unduplicated Count of Students Suspended (Total)`, '\\*', '')

# change column type
suspensions$CumulativeEnrollment <- as.numeric(suspensions$CumulativeEnrollment)
suspensions$`Unduplicated Count of Students Suspended (Total)` <- as.numeric(suspensions$`Unduplicated Count of Students Suspended (Total)`)

#group by student group and summarize
susp_table <- suspensions %>% group_by(ReportingCategory) %>%
  dplyr::summarize(enrollment = sum(CumulativeEnrollment, na.rm = TRUE),
            suspensions = sum(`Unduplicated Count of Students Suspended (Total)`, na.rm = TRUE))

#calculate suspension rates
susp_table$rate <- round(susp_table$suspensions/susp_table$enrollment*100, 1)

#order rate descending
susp_table <- susp_table %>% arrange((desc(rate)))

# format category names

# Replace multiple strings at a time
rep_str = c('RB' = 'African American',
            'RI' = 'American Indian or Alaska Native',
            'RA' = 'Asian',
            'RF' = 'Filipino',
            'RH' = 'Hispanic or Latino',
            'RD' = 'Not Reported',
            'RP' = 'Pacific Islander',
            'RT' = 'Two or More Races',
            'RW' = 'White',
            'GM' = 'Male',
            'GF' = 'Female',
            'GX' = 'Non-Binary Gender',
            'GZ' = 'Missing Gender',
            'SE' = 'English Learners',
            'SD' = 'Students with Disabilities',
            'SS' = 'Socioeconomically Disadvantaged',
            'SM' = 'Migrant',
            'SF' = 'Foster',
            'SH' = 'Homeless',
            'TA' = 'Total')
susp_table$ReportingCategory <- str_replace_all(susp_table$ReportingCategory, rep_str)

names(susp_table) <- c("Student group", "Student enrollment", "Unduplicated count of students suspended", "Suspension rate")
#View(susp_table)

