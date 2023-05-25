## Comparing Students and Teacher/Staff Diversity by Race in the Antelope Valley Grouped Bar Chart

# install packages if not already installed -----
list.of.packages <- c("dplyr","tidyr","showtext","ggplot2","forcats","ggtext") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 


# Prep and Set Up 
#### Loading Libraries ####
library(dplyr)
library(tidyr)
library(showtext)
library(ggplot2)
library(forcats)
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


# Teacher vs Student Diversity Grouped Bars Bar Chart -----



con <- connect_to_db("region5stateofthechild")

av_staff_student_race <- st_read(con, query = "SELECT * FROM av_staff_student_race")

student_teacher_diff <- av_staff_student_race %>% dplyr::select(-c(enrollment, staff, rate_difference)) %>% 
  dplyr::rename("Students" = "enrollment_rate", "Staff" = "staff_rate") %>%
  pivot_longer(c(Students, Staff), names_to = "Names", values_to = "Rate")

student_teacher_diff$race_ethnicity <- gsub("American Indian or Alaska Native, not Hispanic","AIAN",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Pacific Islander, not Hispanic","Pacific Islander",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("White, not Hispanic","White",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Filipino, not Hispanic","Filipinx",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Two or More Races, not Hispanic","Two or More Races",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("African American, not Hispanic","Black",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Asian, not Hispanic","Asian",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Hispanic or Latino","Latinx",student_teacher_diff$race_ethnicity)

student_teacher_diff <- student_teacher_diff %>% mutate(race_ethnicity = fct_relevel(race_ethnicity, "Pacific Islander", "AIAN", "Filipinx", "Asian", "Two or More Races", "Black", "Latinx", "White"))

student_staff_plot <-
  ggplot(student_teacher_diff, aes(x=race_ethnicity, y=Rate, fill=Names)) +     # set up manual fill using 'test', add "-" before value to order bars when MAX is best #fill = Names, 
  labs(title="Most <span style='color:#332985;'>Students</span> are Latinx Whereas Most <span style='color:#009CDB;'>School Staff</span> are White", 
       subtitle = "Percentages of Student Enrollment and Teachers and Staff in the Antelope Valley", 
       caption = "Source: California Department of Education, 2018-19.
Note: American Indian and Alaska Native (AIAN), White, Two or More Races, Asian, Filipino, and Pacific 
Islander are Non-Hispanic.")+ #, https://www.cde.ca.gov/ds/. 
  geom_text(aes(race_ethnicity, label = paste0(Rate,"%")), colour = black,   
            stat = "identity", position = position_dodge(0.9), family = font_table_text, size=8, check_overlap = TRUE, hjust = -0.1)+ 
  geom_bar(stat="identity", position = position_dodge(0.9), show.legend = FALSE) + 
  scale_fill_manual(values = c("Students" = darkblue,"Staff" = lightblue))+
  theme_void()+
  xlab("") +
  ylab("") +
  expand_limits(y = c(0,73))+
  coord_flip()

student_staff_plot <- student_staff_plot + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y =element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 20, family= font_axis_label, face = "bold", hjust = 0),
  axis.ticks = element_blank(),
  plot.title = element_markdown(family = font_title, face = "bold", size = 32, hjust = 0, lineheight = 0.5),
  plot.subtitle = element_markdown(family = font_title, face = "bold", size = 26, hjust = 0, lineheight = 0.5),
  plot.title.position = "plot",
  plot.caption = element_text(family = font_caption, size = 20, lineheight = 0.35, hjust = 0),
  plot.caption.position = "plot",
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)
student_staff_plot

ggsave(student_staff_plot, filename = "student_v_staff_chart.png", 
       path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/", 
       bg='transparent', width = 5, height=4) #,  width=8, height=5.5



