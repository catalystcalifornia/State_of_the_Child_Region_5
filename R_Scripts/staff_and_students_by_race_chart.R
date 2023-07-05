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
            stat = "identity", position = position_dodge(0.9), family = font_table_text, size=16, check_overlap = TRUE, hjust = -0.1)+
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
  axis.text.y = element_text(size = 40, family= font_axis_label, face = "bold", hjust = 0),
  axis.ticks = element_blank(),
  plot.title = element_markdown(family = font_title, face = "bold", size = 64, hjust = 0, lineheight = 0.25),
  plot.subtitle = element_markdown(family = font_title, face = "bold", size = 52, hjust = 0, lineheight = 0.25),
  plot.title.position = "plot",
  plot.caption = element_text(family = font_caption, size = 40, lineheight = 0.3, hjust = 0),
  plot.caption.position = "plot",
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)
student_staff_plot

ggsave(student_staff_plot, filename = "student_v_staff_chart.png",
       path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/English/",
       bg='transparent', width = 10, height=8) 

# Spanish version --------

student_teacher_diff$race_ethnicity <- gsub("Pacific Islander","Isleñx del Pacífico (Isleño del Pacífico)",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("White","Blancx (Blanco)",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Filipinx","Filipinx (Filipino)",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Two or More Races","Dos o más razas",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Black","Negrx (Negro)",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Asian","Asiáticx (Asiático)",student_teacher_diff$race_ethnicity)
student_teacher_diff$race_ethnicity <- gsub("Latinx","Latinx (Latino)",student_teacher_diff$race_ethnicity)

student_teacher_diff <- student_teacher_diff %>% 
  mutate(race_ethnicity = fct_relevel(race_ethnicity, "Isleñx del Pacífico (Isleño del Pacífico)",
                                      "AIAN", "Filipinx (Filipino)", "Asiáticx (Asiático)", "Dos o más razas", 
                                      "Negrx (Negro)", "Latinx (Latino)", "Blancx (Blanco)"))


student_staff_plot <-
  ggplot(student_teacher_diff, aes(x=race_ethnicity, y=Rate, fill=Names)) +     # set up manual fill using 'test', add "-" before value to order bars when MAX is best #fill = Names, 
  labs(title="La mayoría de <span style='color:#332985;'>los estudiantes</span> son latinxs, mientras que la mayoría<br><span style='color:#009CDB;'>del personal escolar</span> es blancx", 
       subtitle = "Porcentajes de matrícula estudiantil, maestros y personales en Antelope Valley", 
       caption = "Fuente: Departamento de Educación de California, 2018-19.
Nota: Los Indixs Americanxs y Nativxs de Alaska (Los Indios Americanos y Nativos de Alaska o AIAN por sus 
siglas en inglés), Blancos, dos o más razas, Asiáticxs (Asiáticos), Filipinxs (Filipinos) e Isleñxs del 
Pacífico (Isleños del Pacífico) no son hispanos.")+ #, https://www.cde.ca.gov/ds/. 
  geom_text(aes(race_ethnicity, label = paste0(Rate,"%")), colour = black,   
            stat = "identity", position = position_dodge(0.9), family = font_table_text, size=16, check_overlap = TRUE, hjust = -0.1)+ 
  geom_bar(stat="identity", position = position_dodge(0.9), show.legend = FALSE) + 
  scale_fill_manual(values = c("Students" = darkblue,"Staff" = lightblue))+
  theme_void()+
  scale_x_discrete(labels = function(race_ethnicity) str_wrap(race_ethnicity, width = 20)) +            # wrap long labels
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
  axis.text.y = element_text(size = 52, family= font_axis_label, face = "bold", hjust = 0, lineheight = 0.3),
  axis.ticks = element_blank(),
  plot.title = element_markdown(family = font_title, face = "bold", size = 64, hjust = 0, lineheight = 0.25),
  plot.subtitle = element_markdown(family = font_title, face = "bold", size = 52, hjust = 0, lineheight = 0.25),
  plot.title.position = "plot",
  plot.caption = element_text(family = font_caption, size = 40, lineheight = 0.3, hjust = 0),
  plot.caption.position = "plot",
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)
student_staff_plot

ggsave(student_staff_plot, filename = "student_v_staff_char_esp.png", 
       path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/Spanish/", 
       bg='transparent', width = 10, height=8)

