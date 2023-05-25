# Get percent rent burdened for AV neighborhoods and Best Start region 5

# Install packages if not already installed
list.of.packages <- c( "dplyr", "readxl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load libraries
library(dplyr)
library(readxl)


options(scipen = 100) # disable scientific notation

######################Data set-up######################

# Load AV Census tract data from CB Census Data Sheet
housing_data <- read_excel("W:/Project/RDA Team/Region 5 State of the Child/Data/ACS 2016-2020 WORKBOOK - for AV report.xlsx", sheet = "DP04") #%>%


# transform data -----

df <- housing_data %>% dplyr::select("NEIGHBORHOOD", #select only useful columns
         "GEO_ID", 
         "Census Tract",
         "HOUSING OCCUPANCY!!Total housing units...5",
         "Renter-occupied", 
         "% Renter-occupied", 
         "Average household size of renter-occupied unit",
         starts_with("% GROSS RENT!!"),
         starts_with("GROSS RENT!"),
         "HOUSING TENURE!!Occupied housing units!!Renter-occupied", 
         starts_with("(GRAPI)!!"), 
         starts_with("% (GRAPI)!!"))


df <- df %>% dplyr::filter(is.na(GEO_ID)) %>% #NA GEOIDs represent the summary rows that Lucy calculated in excel which is why they do not have geoids. Filter to use only those summary rows by neighborhood and AV total
  dplyr::select(-c("GEO_ID", "Census Tract")) %>% 
  dplyr::rename("Plus35" = "% (GRAPI)!!35.0 percent or more", "Plus30" = "% (GRAPI)!!30.0 to 34.9") %>% 
  mutate(Rent_Burdened = (Plus30+Plus35)*100)

av_table <- df %>% dplyr::select("NEIGHBORHOOD", "Rent_Burdened")
av_table <- av_table[order(sub("Antelope Valley Best Start Region 5", " ", av_table$NEIGHBORHOOD)), ]
av_table$Rent_Burdened <-  paste0(as.matrix(av_table$Rent_Burdened), '%')

# View(av_table)

AV <- df %>% dplyr::select("NEIGHBORHOOD", "Rent_Burdened")
