# install packages -----
list.of.packages <- c("ggplot2", "sf", "raster", "sysfonts", "showtext", "ggtext")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

# spas from https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view/explore?location=34.025999%2C-118.300941%2C8.00 
# Load packages
library(ggplot2)
library(sf)
library(raster)
library(sysfonts)
library(showtext)
library(ggtext)
options(tigris_use_cache = TRUE)



# postgres credentials
source("W:\\RDA Team\\R\\credentials_source.R") 

#### First 5 LA Style Guide ####
## COLORS## Taken from W:\Project\RDA Team\Region 5 State of the Child\Documentation\F5LA_BrandGuidelines_COLORS.pdf
#primary
lightblue <- "#009CDB"
darkblue <- "#332985"
tealblue <- "#22BCB8"
black <- "#000000"
textgrey <- "#919191"
#secondary 
green <- "#54B847"
orange <- "#F58326"
hotpink <- "#EC098C"
red <- "#EF4034"

## FONTS ##
# Step 1: Download fonts (Gotham Bold and Gotham Book) 
# Step 2: Load the downloaded fonts into the R script:
# General: See tutorial here, under "THE SHOWTEXT PACKAGE" section: https://r-coder.com/custom-fonts-r/#The_showtext_package 
# Step 3: Run the code below in each R script

font_add(family = "GothamBold", regular = "C:/Windows/Fonts/Gotham-Bold 700.otf")
font_add(family = "GothamBook", regular = "C:/Windows/Fonts/Gotham-Book 325.otf")

showtext_auto()

# define fonts in chart
font_title <- "GothamBold"
font_subtitle <- "GothamBook"
font_caption <- "GothamBook"
font_bar_label <- "GothamBold"
font_axis_label <- "GothamBook"
font_table_text<-"GothamBook"


# load AV data -----
source("W:/Project/RDA Team/Region 5 State of the Child/R/population.R")

#load neighborhoods data
hpsas <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Data/Health_Professional_Shortage_Area_Mental_Health/Health_Professional_Shortage_Area_Mental_Health.shp", quiet=TRUE)
# View(hpsas)
#plot(hpsas$geometry)
hpsas <- st_transform(hpsas, crs = 3310)

lac_spas <- st_transform(lac_spas, crs = 3310)

#calculate spatial intersection of tracts in av (SPA 1)
lac_hpsas <- st_intersection(lac_spas, hpsas)
lac_hpsas <- st_transform(lac_hpsas, crs = 3310)
# lac_hpsas <- lac_hpsas %>% unique()
# View(lac_hpsas)
#plot(lac_hpsas$geometry)
# st_crs(lac_hpsas)
cropped_hpsas <- subset(lac_hpsas, MSSA_NAME != "Avalon")



lac_spas_sngl <- st_cast(lac_spas, "POLYGON")
lac_spas_sngl <- lac_spas_sngl[c(-9, -10),] # 9 and 10 are islands
# plot(lac_spas_sngl["SPA_NAME"])

dev.off() #something was blocking the ggplot from mapping so I used dev.off() to shut down the current plotting device

# visualize school districts and SPAs -----
hpsas_map <- ggplot(cropped_hpsas) + 
  geom_sf(data = lac_spas_sngl, color = black, size = 5)+
  geom_sf(data = av, color = black, size = 16)+
  geom_sf(data = cropped_hpsas, color = black, fill = lightblue, size = 0.5)+
  geom_sf_text(data = lac_spas, aes(label = SPA_NAME), size = 18, color = black, position = position_dodge(0.9),
               check_overlap = TRUE, family = font_bar_label)+
  labs(title="The Entire Antelope Valley is a <span style='color:#009CDB;'>Health Professional<br>Shortage Area</span> for Mental Health Professionals",
       subtitle="LA County Mental Health Professional Shortage Area Designations",
       caption = "Source: Health Professional Shortage Areas, 2022, California State Geoportal.") +#https://gis.data.ca.gov/datasets/CHHSAgency::health-professional-shortage-area-mental-health/explore.
  coord_sf()+
  theme_void()
hpsas_map <- hpsas_map + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  plot.title = element_markdown(hjust = 0, family = font_title, size = 76, face = "bold", lineheight = 0.3, margin=margin(3,0,0,-120)),
  plot.subtitle = element_markdown(hjust = -0.7, family = font_title, size = 64, face = "bold", lineheight = 0.3, margin=margin(5,0,0,-165)),
  plot.caption = element_text(hjust = 5, family = font_caption, color = black, size = 40, face = "bold", lineheight = 0.3), 
  plot.title.position = "plot",
  plot.caption.position = "plot",
  legend.position="none",
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)

hpsas_map

ggsave(hpsas_map, file="hpsas_map.png",
       path="W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/English/",
       bg='transparent', width = 10, height=8)

#Spanish Version
hpsas_map <- ggplot(cropped_hpsas) + #lac_hpsas
  geom_sf(data = lac_spas_sngl, color = black, size = 5)+
  geom_sf(data = av, color = black, size = 16)+
  geom_sf(data = cropped_hpsas, color = black, fill = lightblue, size = 0.5)+
  geom_sf_text(data = lac_spas, aes(label = SPA_NAME), size = 18, color = black, position = position_dodge(0.9), 
               check_overlap = TRUE, family = font_bar_label)+
  labs(title="Todo el Antelope Valley es <span style='color:#009CDB;'>un área de escasez de<br>profesionales de la salud</span> para profesionales de la salud<br>mental", 
       subtitle="Designaciones del Área de Escasez de Profesionales de Salud<br>Mental del Condado de Los Ángeles",
       caption = "Fuente: Áreas de Escasez de Profesionales de Salud, 2022, Geoportal Del Estado de California.") +#https://gis.data.ca.gov/datasets/CHHSAgency::health-professional-shortage-area-mental-health/explore.
  coord_sf()+
  theme_void()
hpsas_map <- hpsas_map + theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  plot.title = element_markdown(hjust = 0, family = font_title, size = 76, face = "bold", lineheight = 0.3, margin=margin(3,0,0,-145)),
  plot.subtitle = element_markdown(hjust = 0, family = font_title, size = 64, face = "bold", lineheight = 0.3, margin=margin(5,0,0,-145)),#5 for the first one and -59 for the last one
  plot.caption = element_text(hjust = 0.7, family = font_caption, color = black, size = 40, face = "bold", lineheight = 0.3),  
  plot.title.position = "plot",
  plot.caption.position = "plot",
  legend.position="none",
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)

hpsas_map

ggsave(hpsas_map, file="hpsas_map_esp.png",
       path="W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/Spanish/", 
       bg='transparent', width = 10, height=8)