# install packages if not already installed -----
list.of.packages <- c("dplyr","janitor","tidyr","stringr","showtext","ggplot2","tigris","forcats","ggtext","hrbrthemes") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 


# Prep and Set Up 
#### Loading Libraries ####
library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
library(showtext)
library(ggplot2)
library(tigris)
library(forcats)
library(ggtext)
library(hrbrthemes)
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
font_table_text <- "GothamBook"

#### Loading Database from PgAdmin/Postgres #### 
source("W:\\RDA Team\\R\\credentials_source.R")
#### Static Visual Functions ####
#Reference: W:\Project\RDA Team\LAFed\R\LAFed\visual_functions_static.R

## HS Grad by Sub Group Bar Chart ----
source("W:/Project/RDA Team/Region 5 State of the Child/R/hs_grad.R")
# View(av_table)
hs_grad <- av_table %>% dplyr::filter(`Adjusted 5-Year 2020-2021`=="Total Rate") %>% 
  dplyr::select(-c(`Adjusted 5-Year 2020-2021`, Male, Female, Migrant, ELL)) %>% 
  mutate_if(is.numeric, round, digits = 2) %>% dplyr::rename("Filipinx"="Filipino")

hs_grad$`Adjusted 5-Year 2020-2021` = "Antelope Valley"

s <- hs_grad 
tot = subset(s, select=c(Total)) # pull out Total
s = subset(s, select=-c(Total)) # pull out raced rates


# create english / spanish labels
engvar = c("Asian","Black","Filipinx","Latinx","AIAN",
           "Two or More Races","White","Disability","Foster","Homeless",
           "Socioconomically Disadvantaged")
esvar = c("Asiático", "Negro", "Filipino", "Latino", "AIAN", "Dos o Más Razas", "Blanco", "Discapacidad", "Adoptivo", "Sin Hogar", "Socioeconómicamente Desfavorecido" )
labels <- data.frame(engvar)
# labels


# convert df to long format
s_long <- reshape2::melt(s, id.vars=c("Adjusted 5-Year 2020-2021"))

# Round 'value' field to 1 decimal, rename 'variable' field, add Spanish labels
s_long <- s_long %>%
  dplyr::rename(engvar = variable) 

df <- s_long
df$Total <- 67.2
df <- na.omit(df)             # drop NA's
df$value <- as.numeric(df$value)
df$Total <- as.numeric(df$Total)


engtot_stat <- paste("Overall Rate =", min(df$Total))   # Total line annotation
estot_stat <- paste("Tasa general =", min(df$Total))   # Total line annotation


# Define chart height as max y value varies by indicator. Set vertical position for total line label - may need to be customized for each chart.
max_y = 1.15 * max(df$value)
annotate_y = 1.12 * (df$Total)

# # ggplot method that should export as .svg ##
hs_plot <-
  #English
  ggplot(df, aes(x= reorder(engvar, -value), y=value)) +     # set up manual fill using 'test', add "-" before value to order bars when MAX is best
  annotate("text", x=10, y =68, label = str_wrap(paste0(engtot_stat, "%"), width = 15), hjust=0, lineheight = 0.3,
           vjust = 0.35, family= font_table_text, color = darkblue, size = 20) +
  labs(title = "**High School Graduation Rates by Student Subgroup, <br>2021-22 School Year, Antelope Valley School Districts**",
       caption = "Source: California Department of Education, Adjusted Cohort Graduation Rate and Outcome Data,
2021-2022. Note: Rates are out of 100 students. AIAN stands for American Indian and Alaskan Native.") + #, https://www.cde.ca.gov/ds/ad/filesacgr.asp.
  # rest of chart
  geom_bar(stat="identity", position = position_dodge(0.7), show.legend = FALSE) +
  geom_col(fill = lightblue) +
  geom_hline(aes(yintercept = round(Total, 1)), color = darkblue, size = .5) +
  geom_text(aes(label = paste0(value, "%")), family = font_table_text, hjust = -0.2, size = 20) +       # format data labels, adjust hjust to avoid overlap w/ total line
  scale_x_discrete(labels = function(engvar) str_wrap(engvar, width = 20)) +            # wrap long labels
  theme_void()+
  xlab("") +
  ylab("") +
  expand_limits(y = c(0,91))+
  coord_flip()

hs_plot <- hs_plot + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y =element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 52, family= font_axis_label, lineheight = 0.3, hjust=0),
  axis.ticks = element_blank(),
  plot.title= element_markdown(family = font_title, face = "bold", size = 72, hjust = 0, lineheight = 0.4, margin=margin(0,0,4,-155)),
  plot.caption = element_text(family = font_caption, size = 40, hjust = 0, lineheight = 0.3),
  plot.caption.position = "plot",
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)

hs_plot

ggsave(hs_plot, filename="hs_grad_chart.png",
       path=("W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/English/"),
       bg='transparent', width = 10, height=8)

#Spanish version -----
df <- df %>% dplyr::rename("esvar" = "engvar")
df$esvar <- gsub("Foster","Adoptivo",df$esvar)
df$esvar <- gsub("Black","Negrx (Negro)",df$esvar)
df$esvar <- gsub("Disability","Estudiantes con discapacidades",df$esvar)
df$esvar <- gsub("Pacific Islander","Isleñx del Pacífico (Isleño del Pacífico)",df$esvar)
df$esvar <- gsub("Male","Hombre",df$esvar)
df$esvar <- gsub("Socioeconomically Disadvantaged","Desfavorecido socioeconómicamente",df$esvar)
df$esvar <- gsub("Two or More Races","Dos o más razas",df$esvar)
df$esvar <- gsub("Homeless" ,"Sin Hogar",df$esvar)
df$esvar <- gsub("Migrant","Migrante",df$esvar)
df$esvar <- gsub("English Learners","Estudiantes de inglés",df$esvar)
df$esvar <- gsub("Latinx","Latinx (Latino)",df$esvar)
df$esvar <- gsub("Female","Mujer",df$esvar)
df$esvar <- gsub("White","Blancx (Blanco)",df$esvar)
df$esvar <- gsub("Filipinx","Filipinx (Filipino)",df$esvar)
df$esvar <- gsub("Asian" ,"Asiáticx (Asiático)",df$esvar)



hs_plot_esp <-

  ggplot(df, aes(x= reorder(esvar, -value), y=value)) +     # set up manual fill using 'test', add "-" before value to order bars when MAX is best
  annotate("text", x=10, y =68, label = str_wrap(paste0(estot_stat, "%"), width = 14), hjust=0, lineheight = 0.3,
           vjust = 0.35, family= font_table_text, color = darkblue, size = 20) +
  labs(title = "Tasas de graduación de la escuela secundaria por subgrupo de<br>estudiantes en el año escolar 2021-22, distritos escolares<br>de Antelope Valley", 
       caption = "Fuente: Departamento de Educación de California, Tasa de graduación de grupo ajustada y datos de
resultados, 2021-2022. 
Nota: Las tasas son de 100 estudiantes. AIAN (por sus siglas en inglés) significa Indix Americanx 
o Nativx de Alaska (Indio Americano o Nativo de Alaska).") + #, https://www.cde.ca.gov/ds/ad/filesacgr.asp. 
  # rest of chart
  geom_bar(stat="identity", position = position_dodge(0.9), show.legend = FALSE) +
  geom_col(fill = lightblue) +         
  geom_hline(aes(yintercept = round(Total, 1)), color = darkblue, size = .5) +
  geom_text(aes(label = paste0(value, "%")), family = font_table_text, hjust = -0.3, size = 20) +       # format data labels, adjust hjust to avoid overlap w/ total line
  scale_x_discrete(labels = function(esvar) str_wrap(esvar, width = 20)) +            # wrap long labels
  theme_void()+
  xlab("") +
  ylab("") +
  expand_limits(y = c(0,92))+
  coord_flip()

hs_plot_esp <- hs_plot_esp + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y =element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 52, family= font_axis_label, lineheight = 0.25, hjust=0),
  axis.ticks = element_blank(),
  plot.title= element_markdown(family = font_title, face = "bold", size = 64, hjust = 0, lineheight = 0.4, margin=margin(0,0,4,-185)),
  plot.caption = element_text(family = font_caption, size = 40, hjust = 0, lineheight = 0.3),
  plot.caption.position = "plot",
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)

hs_plot_esp

ggsave(hs_plot_esp, filename="hs_grad_chart_esp.png",
       path=("W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/Spanish/"), 
       bg='transparent', width = 10, height=8) 
