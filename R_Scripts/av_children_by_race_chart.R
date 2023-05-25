# install packages if not already installed -----
list.of.packages <- c("ggalt", "forcats", "gt","plyr", "devtools",  "rlang",  "openxlsx", "tidyr", "dplyr", "stringr", "RPostgreSQL", "data.table", "janitor", "ggplot2", "kableExtra", "extrafont", "tigris", "ggtext", "flextable", "highcharter", "tidycensus", "srvyr", "sysfonts", "showtext", "kableExtra", "janitor", "plotly", "leaflet", "rmarkdown", "RPostgreSQL", "rpostgis", "naniar", "dbplyr", "DT", "sf", "formattable", "knitr", "rgdal", "RColorBrewer", "scales", "colorspace", "broom", "reshape2", "sp", "forcats", "ggrepel", "sp", "svglite", "readxl", "htmltools", "usethis", "webshot", "gt", "rvest", "cowplot") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 
if(length(new.packages)) install.packages(new.packages) 


#### Loading Libraries ####
library(tidyr)
library(showtext)
library(ggplot2)
library(ggtext)
set.seed(1)
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

#### Loading Database from PgAdmin/Postgres #### 
source("W:\\RDA Team\\R\\credentials_source.R")
#### Static Visual Functions ####

#Reference: W:\Project\RDA Team\LAFed\R\LAFed\visual_functions_static.R


# Race/ethnicity of Antelope Valley Children Under Age 18 ----
source("W:/Project/RDA Team/Region 5 State of the Child/R/under18_by_race.R")

under18race_wide  <- d  %>% 
  dplyr::select(ends_with("_rate")) %>% 
  as.data.frame() %>% 
  dplyr::rename("Latinx"="latino_rate",
                "White"="nh_white_rate", "Black"="nh_black_rate",    
                "Asian"="nh_asian_rate", "AIAN"="aian_rate",        
                "NHPI"="pacisl_rate", "Other"="nh_other_rate",    
                "Two or More"="nh_twoormor_rate", "SWANA"="swana_rate") 

under18race <- pivot_longer(under18race_wide, cols = c("Latinx","White", "Black",    
                                                       "Asian", "AIAN", "NHPI", "Other",    
                                                       "Two or More", "SWANA"), 
                            names_to = "Race_ethnicity", values_to = "Percent") 

df <- na.omit(under18race)             # drop NA's
df$Percent <- as.numeric(df$Percent)
df$Percent <- round(df$Percent, digits=1)


age_plot <-
  ggplot(df,aes(x= reorder(Race_ethnicity, Percent), y=Percent)) +     # set up manual fill using 'test', add "-" before value to order bars when MAX is best  
  labs(title = "The Majority of Children in the Antelope Valley are Latinx", 
       subtitle = "Race/Ethnicity of Antelope Valley Children Under Age 18", 
       caption = "Source: Catalyst California calculations of children under the age of 18 from US Census Bureau American 
Community Survey 5-year PUMS data (2017-2021). 
Note: The sum of racial-ethnic group percentages will exceed 100% because American Indian and Alaska
Native (AIAN), Southwest Asian and North African (SWANA), and Native Hawaiian and Pacific Islander (NHPI)
include data alone or in combination with other races and ethnicities. Latinx includes any mention of Latinx 
across races.") +  
  geom_bar(stat="identity", position = position_dodge(0.7), show.legend = FALSE) +
  geom_col(fill = c(lightblue)) +         
  geom_text(aes(label = paste0(Percent, "%")), family = font_table_text, size=8, hjust = -.1) + # format data labels, adjust hjust to avoid overlap w/ total line
  theme_void()+
  xlab("") +
  ylab("") +
  coord_flip()+
  expand_limits(y = c(0,58))

age_plot <- age_plot + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 24, family= font_axis_label, hjust=0),
  axis.ticks = element_blank(),
  plot.title = element_markdown(family = font_title, face = "bold", size = 32, hjust = 0, lineheight=.5), 
  plot.subtitle = element_markdown(family = font_title, face = "bold", size = 26, hjust = 0, lineheight=.5),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(family = font_caption, size = 20, hjust = 0, lineheight=0.35),
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)

age_plot

ggsave(file="race_under_18_chart.png", 
       path="W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/", 
       plot=age_plot, bg='transparent', width = 5, height=4)

