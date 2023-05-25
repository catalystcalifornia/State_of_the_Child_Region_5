# install packages if not already installed -----
list.of.packages <- c("stringr","showtext","ggplot2","ggtext") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages) 


# Prep and Set Up 
#### Loading Libraries ####
library(stringr)
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



# Rent Burden Totals By Neighborhood ----
source("W:/Project/RDA Team/Region 5 State of the Child/R/rent_burden_totals.R")

# View(av_table)
av_table$Rent_Burdened<-gsub("%$","",av_table$Rent_Burdened)
av_table$Rent_Burdened <- as.numeric(av_table$Rent_Burdened)
av_table$Rent_Burdened <- round(av_table$Rent_Burdened, digits = 2)

av_df <- av_table[av_table$NEIGHBORHOOD != 'ANTELOPE VALLEY BEST START REGION 5', ]
av_df$NEIGHBORHOOD <- str_to_title(av_df$NEIGHBORHOOD)
av_df$Total <- 59.2
engtot_stat <- paste0("Overall Rate = ", min(av_df$Total), "%")   # Total line annotation
annotate_y = 1 * (av_df$Total)

# # ggplot method that should export as .svg ##
rent_plot <-
  ggplot(av_df, aes(x= reorder(NEIGHBORHOOD, Rent_Burdened), y=Rent_Burdened)) +     # set up manual fill using 'test', add "-" before value to order bars when MAX is best
  labs(title= "Nearly Three in Five Antelope Valley Renters are Rent Burdened", 
       subtitle = "Percentage of Renter Households Spending 30% or More of Their Income on Rent", 
       caption = "Source: Lucy Wilkerson's calculations based on American Community Survey 2016-2020 5-year estimates.") + #, https://public.tableau.com/app/profile/luz3725/viz/2020CensusData-AVBESTSTARTREGION5/GRAPI. 
  geom_bar(stat="identity", position = position_dodge(0.7), show.legend = FALSE, fill = lightblue) +
  geom_hline(aes(yintercept = round(Total, 1)), color = darkblue, size = .5) +
  annotate("text", x=6.25, y = annotate_y, label = str_wrap(engtot_stat, width = 20), hjust=-0.05, 
           vjust = 38, family= font_table_text, color = darkblue, size = 8) +
  geom_text(aes(label = paste0(Rent_Burdened, "%")), family = font_table_text, hjust = -.1, size = 8) +       # format data labels, adjust hjust to avoid overlap w/ total line
  scale_x_discrete(labels = function(NEIGHBORHOOD) str_wrap(NEIGHBORHOOD, width = 15)) +            # wrap long labels
  theme_void()+
  xlab("") +
  ylab("") +
  expand_limits(y = c(0,82))+
  coord_flip()
rent_plot

rent_plot <- rent_plot + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y =element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 20, family= font_axis_label, lineheight = 0.3, hjust=0),
  axis.ticks = element_blank(),
  plot.title = element_markdown(family = font_title, face = "bold", size =32, hjust = 0, margin=margin(0,0,4,0)),
  plot.subtitle = element_markdown(family = font_title, face = "bold", size =26, hjust = 0, margin=margin(0,0,4,0)),
  plot.caption = element_text(family = font_caption, size = 20, hjust = 0, lineheight = 0.35), 
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)

rent_plot

ggsave(rent_plot, filename="rent_burdened_chart.png",
       path=("W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/"), 
       bg='transparent', width = 5, height=4) 
