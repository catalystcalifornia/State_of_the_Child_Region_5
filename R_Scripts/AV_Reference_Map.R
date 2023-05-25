# Map of the Antelope Valley

# spas from https://egis-lacounty.hub.arcgis.com/datasets/service-planning-areas-2022-view/explore?location=34.025999%2C-118.300941%2C8.00 

# install packages if not already installed-----
list.of.packages <- c("sf", "raster", "showtext", "ggplot2", "ggspatial", "sysfonts") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

# Load libraries

library(sf)
library(raster)
library(showtext)
library(ggplot2)
library(ggspatial)
library(sysfonts)
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
#from https://s3-us-west-2.amazonaws.com/mappingla.com/downloads/neighborhoods/la_county.shp.zip
lac_neighborhoods <- st_read("W:/Data/Geographies/LA County/LA Times/la-county-neighborhoods-current 2020/l.a. county neighborhood (current).shp", quiet=TRUE)
# View(lac_neighborhoods)
#plot(lac_neighborhoods$geometry)

#check and convert crs
#st_crs(lac_neighborhoods)
lac_neighborhoods <- st_transform(lac_neighborhoods, crs = 3310)
#st_crs(lac_neighborhoods)

#calculate spatial intersection of tracts in av (SPA 1)
av_neighborhoods <- st_intersection(av, lac_neighborhoods)
# View(av_neighborhoods)
#plot(av_neighborhoods$geometry)


#ggplot------
# delete these labels so they do not overlap in the image
av_neighborhoods$name[av_neighborhoods$name=="Desert View Highlands"] <- NA
av_neighborhoods$name[av_neighborhoods$name=="Castaic Canyons"] <- NA
av_neighborhoods$name[av_neighborhoods$name=="Santa Clarita"] <- NA
av_neighborhoods$name[av_neighborhoods$name=="Northwest Palmdale"] <- NA

#visualize school districts and SPAs -----
av_neighborhood_map <- ggplot(av_neighborhoods) +
  annotation_map_tile(type ="hotstyle", zoom = 11) + #this is the line that was trying add the topographical element
  # spatial-aware automagic scale bar
  geom_sf(data = av_neighborhoods, color = black, fill = NA)+ 
  geom_sf(data = av, color = black, fill = NA)+
  ggrepel::geom_label_repel(aes(label = name, geometry = geometry), stat = "sf_coordinates",
                             min.segment.length = 0,  colour = black, family = font_axis_label, size = 5, segment.colour = darkblue) +
  coord_sf(crs = st_crs(3310))+
theme_void()+
theme(panel.grid.major=element_line(color="transparent"), legend.position="none",
      plot.title = element_text(hjust = 0, family = font_title, size = 32, lineheight = 0.5),
      plot.caption = element_text(hjust = 0, family = font_caption, color = black, size = 20, lineheight = 0.35),
      plot.caption.position = "plot", plot.title.position = "plot",  
)+
labs(title="Antelope Valley by Neighborhood",
     caption = "Source: Los Angeles Times calculated Los Angeles County Neighborhoods Boundaries, 2020. 
Los Angeles County Service Planning Area ArcGIS Hub, Department of Public Health (DHS) Service Planning 
Area (SPA) boundaries, 2022.") # https://hub.arcgis.com/datasets/e9134f735c0c473d8156f4703a687ce9/explore
av_neighborhood_map

ggsave(av_neighborhood_map, file="AV_Reference_Map.png",
       path="W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/", bg='transparent', 
       width = 5, height=4)