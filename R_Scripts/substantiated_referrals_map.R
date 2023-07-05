# install packages -----
list.of.packages <- c("ggplot2", "sf", "raster", "data.table", "sysfonts", "showtext", "ggtext", "ggspatial") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 


# spas from https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view/explore?location=34.025999%2C-118.300941%2C8.00 
# loading our packages-----
library(ggplot2)
library(sf)
library(raster)
library(data.table)
library(sysfonts)
library(showtext)
library(ggtext)
library(ggspatial)

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

#load substantiated referrals data ------
dv <- st_read("W:/Project/RDA Team/Region 5 State of the Child/Data/DCFS_All_Substantiated_Referrals_2021/dcfs_2021_csa.shp")
# View(dv)
#plot(dv$geometry)
dv <- st_transform(dv, crs = 3310)

#load spas data ------
lac_spas <- st_transform(lac_spas, crs = 3310)

#calculate spatial intersection of tracts in LA County (SPA 1)
lac_dv <- st_intersection(lac_spas, dv)
lac_dv <- st_transform(lac_dv, crs = 3310)
lac_dv <- lac_dv %>% unique()
# lac_dv <- subset(lac_dv, MSSA_NAME != "Avalon")
# View(lac_dv)
#plot(lac_dv$geometry)
# st_crs(lac_dv)


lac_dv$Rate_per_1 <- ifelse(lac_dv$Rate_per_1==0,NA,lac_dv$Rate_per_1)
lac_dv <- subset(lac_dv, lac_dv$CSA!="City of Avalon") #remove SPA for South Bay so it doesn't extend the map to include the islands

lac_spas_alt <- subset(lac_spas, SPA_NAME!="South Bay") #remove SPA for South Bay so it doesn't extend the map to include the islands
#visualize substantiated referrals and SPAs -----


dv_map <- ggplot(lac_dv) +
  geom_sf(aes(fill = `Rate_per_1`), color = textgrey, size = 0.5)+ #set what the fill is
  scale_fill_gradient2( #create the gradient scale for the data
                      low = "#1FA6DD",
                      high = lightblue,
                      na.value = "white"
                      ) +
  geom_sf(data = av, color = black, size = 20, fill = NA)+ #try to create a thicker border for the AV
  geom_sf(data = lac_spas_alt, color = black, size = 20, fill = NA)+ #create SPA borders
  geom_sf_text(data = lac_spas, aes(label = SPA_NAME), size = 18, family = font_bar_label)+ #label the SPAs
  labs(title="<span style='color:#009CDB;'>Substantiated Referral Rates</span> are Higher in the Antelope<br>Valley", #use html language to change the color of part of the title
       subtitle=("LA County Substantiated Referral Rate Area Designations"),
       caption = "Source: DCFS All Substantiated Referrals 2021, Los Angeles County Data.") + #, https://data/lacounty.gov/datasets/lacounty::dcfs-all-substantiated-referrals-2021/explore
  coord_sf(crs = st_crs(3310))+
  guides(fill=guide_legend(title="Rate per 1000 Children"))+ #rewrite the legend title
  theme_void()

dv_map <- dv_map + theme(  #customize font styling
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  plot.title = element_markdown(hjust = 0, family = font_title, size = 72, face = "bold", lineheight = 0.2, margin=margin(0,0,0,-20)),
  plot.subtitle = element_markdown(hjust = 0, family = font_title, size = 64, face = "bold", lineheight = 0.2, margin=margin(5,0,0,-20)),
  plot.caption = element_text(hjust = -0.99, family = font_caption, color = black, size = 40, face = "bold", lineheight = 0.3),
  legend.text = element_text(size = 56),
  legend.title = element_text(size = 56)
)

dv_map

ggsave(dv_map, file="substantiated_referrals_map.png",
       path="W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/English/",
       bg='transparent',
       width = 10, height=8)

# Spanish version ----
# View(lac_dv)

#maybe translate west and east and south in the map
lac_dv$SPA_NAME <- gsub("West","Oeste",lac_dv$SPA_NAME)
lac_dv$SPA_NAME <- gsub("East","Este",lac_dv$SPA_NAME)
lac_dv$SPA_NAME <- gsub("South","Sur",lac_dv$SPA_NAME)

dv_map <- ggplot(lac_dv) + 
  geom_sf(aes(fill = `Rate_per_1`), color = textgrey, size = 0.5)+ #set what the fill is
  scale_fill_gradient2( #create the gradient scale for the data
    low = "#1FA6DD", 
    high = lightblue,
    na.value = "white"
  ) +
  geom_sf(data = av, color = black, size = 20, fill = NA)+ #try to create a thicker border for the AV
  geom_sf(data = lac_spas_alt, color = black, size = 20, fill = NA)+ #create SPA borders
  geom_sf_text(data = lac_spas, aes(label = SPA_NAME), size = 18, family = font_bar_label)+ #label the SPAs
  labs(title="<span style='color:#009CDB;'>Las tasas de referencia fundamentadas</span> son más altas en<br>Antelope Valley", #use html language to change the color of part of the title
       subtitle=("Designaciones de área de tasa de referencia justificada del condado<br>de Los Ángeles"),
       caption = "Fuente: Departamento de Niños y Familias (DCFS por sus siglas en inglés) del Condado de Los 
Ángeles, todas referencias fundamentadas 2021.") + #, https://data/lacounty.gov/datasets/lacounty::dcfs-all-substantiated-referrals-2021/explore
  coord_sf(crs = st_crs(3310))+ 
  guides(fill=guide_legend(title="Tasa por 1000 niños"))+ #rewrite the legend title
  theme_void()

dv_map <- dv_map + theme(  #customize font styling
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  plot.caption.position = "panel",
  plot.title = element_markdown(hjust = 0, family = font_title, size = 72, face = "bold", lineheight = 0.3, margin=margin(0,0,0,-40)),
  plot.subtitle = element_markdown(hjust = 0, family = font_title, size = 52, face = "bold", lineheight = 0.3, margin=margin(5,0,0,-40)),
  plot.caption = element_text(hjust = 0, family = font_caption, color = black, size = 40, face = "bold", lineheight = 0.3), #, margin=margin(0,0,0,-30)
  legend.text = element_text(size = 56),
  legend.title = element_text(size = 56)
)

dv_map

ggsave(dv_map, file="substantiated_referrals_map_esp.png",
       path="W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/Spanish/", 
       bg='transparent',
       width = 10, height=8)