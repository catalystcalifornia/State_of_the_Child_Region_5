# install packages if not already installed ----
list.of.packages <- c("dplyr", "gt", "showtext", "tidyr") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

#### Loading Libraries ####
library(dplyr)
library(tidyr)
library(gt)
library(showtext)

# postgres credentials
source("W:\\RDA Team\\R\\credentials_source.R") 

con <- connect_to_db("region5stateofthechild")

ece_access_it_2020 <- st_read(con, query = "SELECT zipcode, neighborhood, unmet_need_children_it, pct_unmet_need_children_it FROM data.ece_access_it_2020")

ece_access_it_2020 <- ece_access_it_2020 %>% rename(
  "ZIP code" = zipcode,
  "Neighborhood" = neighborhood,
  "Unmet need for infant/toddler care" = unmet_need_children_it,
  "% of infant/toddlers with unmet need" = pct_unmet_need_children_it
)

ece_access_it_2020$Neighborhood <- gsub('[{]', '', ece_access_it_2020$Neighborhood)
ece_access_it_2020$Neighborhood <- gsub('[}]', '', ece_access_it_2020$Neighborhood)
ece_access_it_2020$Neighborhood <- gsub('["]', '', ece_access_it_2020$Neighborhood)
ece_access_it_2020$Neighborhood <- gsub('[,]', ', ', ece_access_it_2020$Neighborhood)

ece_access_prek_2020 <- st_read(con, query = "SELECT zipcode, neighborhood, unmet_need_children_prek, pct_unmet_need_children_prek FROM data.ece_access_prek_2020")

ece_access_prek_2020 <- ece_access_prek_2020 %>% rename(
  "ZIP code" = zipcode,
  "Neighborhood" = neighborhood,
  "Unmet need for preschool" = unmet_need_children_prek,
  "% of preschoolers with unmet need" = pct_unmet_need_children_prek
)

ece_access_prek_2020$Neighborhood <- gsub('[{]', '', ece_access_prek_2020$Neighborhood)
ece_access_prek_2020$Neighborhood <- gsub('[}]', '', ece_access_prek_2020$Neighborhood)
ece_access_prek_2020$Neighborhood <- gsub('["]', '', ece_access_prek_2020$Neighborhood)
ece_access_prek_2020$Neighborhood <- gsub('[,]', ', ', ece_access_prek_2020$Neighborhood)
