# Get food security data for the AV Congressional District, County, and State,
# and WIC data by Best Start, SPA, Supervisor District, and County

library("tidyverse")
library("readxl")

#food security data downloaded and saved to W drive from https://map.feedingamerica.org/
district_data <- read_excel("W:/Data/Health/FeedingAmerica/Map_the_Meal_Gap_Data/MMG2022_2020-2019Data_ToShare.xlsx", sheet = 3)

district_data <- district_data %>% filter(State == "CA" & Year == 2020 & (District == "Congressional District 25, California" | 
                                            District == "Congressional District 23, California")) %>%
  dplyr::select(District, `Child Food Insecurity Rate (1 Year)`, `# of Food Insecure Children (1 Year)`, ) %>%
  rename(Geography = District)

#12.3% of District 25 children are food insecure.


county_data <- read_excel("W:/Data/Health/FeedingAmerica/Map_the_Meal_Gap_Data/MMG2022_2020-2019Data_ToShare.xlsx", sheet = 2)

county_data <- county_data %>% filter(State == "CA" & Year == 2020 & `County, State` == "Los Angeles County, California") %>%
  dplyr::select(`County, State`, `Child Food Insecurity Rate (1 Year)`, `# of Food Insecure Children (1 Year)`, ) %>% 
  rename(Geography = `County, State`)

#1 in 5 LA County children (20.1%) are food insecure.


state_data <- read_excel("W:/Data/Health/FeedingAmerica/Map_the_Meal_Gap_Data/MMG2022_2020-2019Data_ToShare.xlsx", sheet = 4)

state_data <- state_data %>% filter(State == "CA" & Year == 2020) %>%
  dplyr::select(`State Name`, `Child Food Insecurity Rate (1 Year)`, `# of Food Insecure Children (1 Year)`, ) %>% 
  rename(Geography = `State Name`)

#13% of California children are food insecure.


combined_data <- rbind(district_data, county_data, state_data)



# Get WIC data https://lawicdata.org/data-research/by-region/
wic_data <- read_excel("W:/Project/RDA Team/Region 5 State of the Child/Data/WIC.xlsx", sheet = 1) %>%
  dplyr::select(-geo_type)

names(wic_data) <- c("Geography", 
                     "Under age 5", 
                     "White (%)", 
                     "Asian (%)",
                     "Other (%)", 
                     "Hispanic - Spanish speaking (%)",
                     "Hispanic - English speaking (%)",
                     "Black (%)")
#View(wic_data)