# California Department of Education Table

# install packages if not already installed ----
list.of.packages <- c("dplyr", "gt", "showtext", "tidyr", "scales") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

#### Loading Libraries ####
library(dplyr)
library(tidyr)
library(gt)
library(showtext)
library(scales)
library(forcats)
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

#Reference: W:\Project\RDA Team\LAFed\R\LAFed\visual_functions_static.R


#Load data ----

#load foster data
source("W:/Project/RDA Team/Region 5 State of the Child/R/foster.R")
av_table <- av_table %>% 
  mutate(across(c(`2021-22 Count`), comma)) %>% 
  rename("Count"="2021-22 Count", "School District"="Geography")
av_table$Name <- "Foster"
cde_foster <- av_table


#load socioeconomically disadvantaged data
source("W:/Project/RDA Team/Region 5 State of the Child/R/socioeconomically_disadvantaged.R")
av_table <- av_table %>% 
  mutate(across(c(`2021-22 Count`), comma)) %>% 
  rename("Count"="2021-22 Count", "School District"="Geography")
av_table$Name <- "Socioeconomically Disadvantaged"
cde_socieconomically_disadvantaged <- av_table 

# Student Homelessness
source("W:/Project/RDA Team/Region 5 State of the Child/R/student_homelessness.R")
av_table <- av_table %>% 
  mutate(across(c(`2021-22 Count`), comma)) %>% 
  rename("Count"="2021-22 Count", "School District"="Geography")
av_table$Name <- "Homeless"
cde_student_homelessness <- av_table

# Students with Disabilities
source("W:/Project/RDA Team/Region 5 State of the Child/R/disabilties.R")
av_table <- av_table %>% 
  mutate(across(c(`2021-22 Count`), comma)) %>% 
  rename("Count"="2021-22 Count", "School District"="Geography")
av_table$Name <- "Disability"
cde_disability <- av_table


# combine data ----
cde <- rbind(cde_foster, cde_socieconomically_disadvantaged, cde_student_homelessness, cde_disability)

# prep data for visualization ----
cde$`School District` <- gsub("Antelope Valley Best Start Region 5","TOTAL", cde$`School District`)
cde$`School District` <- gsub(" School District","", cde$`School District`)

# visualize in English ----
cde_eng <- cde %>% mutate(`School District` = fct_relevel(`School District`, 
                                                      "Acton-Agua Dulce Unified School District", "Antelope Valley Union Joint High School District", 
                                                      "Eastside Union Elementary School District", "Hughes-Elizabeth Lakes Union Elementary School District", 
                                                      "Keppel Union Elementary School District", "Lancaster Elementary School District", 
                                                      "Palmdale Elementary School District", "Westside Union Elementary School District", 
                                                      "Wilsona Union Elementary School District", "TOTAL"))

cde_eng <- cde_eng %>% 
  pivot_wider(names_from = Name, names_glue = "{Name}_{.value}", values_from = c(Count, `Rate (%)`)) %>% gt() %>% 
  tab_spanner(label = "Foster", columns = c(`Foster_Count`, `Foster_Rate (%)`)) %>% 
  tab_spanner(label = "Socioeconomically Disadvantaged", columns = c(`Socioeconomically Disadvantaged_Count`, `Socioeconomically Disadvantaged_Rate (%)`)) %>% 
  tab_spanner(label = "Homeless", columns = c(`Homeless_Count`, `Homeless_Rate (%)`)) %>% 
  tab_spanner(label = "Disability", columns = c(`Disability_Count`, `Disability_Rate (%)`)) %>% opt_all_caps() %>% 
  tab_header(title = md("**Student Population by Subgroup in the Antelope Valley, 2020-2021 School Year**")) %>% 
  tab_footnote (footnote = md("Source: Catalyst California calculations based on the California Department of Education data, 2021-22.<br>
                              Note: Antelope Valley schools are schools from the following districts: Acton-Agua Dulce Unified, Antelope Valley 
                              Union High, Eastside Union Elementary, Hughes-Elizabeth Lakes Union Elementary, Keppel Union Elementary, 
                              Lancaster Elementary, Palmdale Elementary, Westside Union Elementary, and Wilsona Elementary. 
                              Socioeconomically disadvantaged means the student is federally eligible for free and reduced meals based 
                              on household income requirements, the migrant program, or the foster program. Students can also be categorized 
                              as socioeconomically disadvantaged if the student's parents do not have a high school diploma or the student 
                              was homeless, directly certified, a tribal foster youth, or enrolled in a juvenile court school. The darkest 
                              color in the chart represents the highest percentage."))%>% #, https://www.cde.ca.gov/ds/ 
  data_color(columns = c(`Foster_Rate (%)`, `Socioeconomically Disadvantaged_Rate (%)`, `Homeless_Rate (%)`, `Disability_Rate (%)`), colors = scales::col_numeric(palette = c("white", lightblue), domain = NULL,
                                                                                                                                                                  na.color = textgrey)) %>% 
  tab_options(table.font.names = font_table_text,
              column_labels.background.color = "white",
              table.border.top.width = px(3),
              table.border.top.color = "transparent",
              table.border.bottom.color = "transparent",
              table.border.bottom.width = px(3),
              column_labels.border.top.width = px(3),
              column_labels.border.top.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = black,
              data_row.padding = px(3),
              source_notes.font.size = 30,
              table.font.size = 50,
              heading.align = "left",
              heading.title.font.weight = "bolder") %>% 
  opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts())) %>% 
  cols_align(
    align = c("left"),
    columns = everything()
  )%>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = `School District`,
              rows = `School District` == "TOTAL"))%>% 
  cols_label(
    `Foster_Count` = "Count", 
    `Foster_Rate (%)` = "Rate (%)",
    `Socioeconomically Disadvantaged_Count` = "Count", 
    `Socioeconomically Disadvantaged_Rate (%)` = "Rate (%)",
    `Homeless_Count` = "Count", 
    `Homeless_Rate (%)` = "Rate (%)",
    `Disability_Count` = "Count", 
    `Disability_Rate (%)` = "Rate (%)")

#save the table
gtsave(cde_eng, "cde_table.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/English/", vwidth = 3000, vheight = 2000) #, vwidth = 1500, vheight = 1000

#Visualize in Spanish ------
cde_es <- cde %>% dplyr::rename("Distrito Escolar"="School District")
cde_es$`Distrito Escolar` <- gsub("Acton-Agua Dulce Unified", 
                              "Acton-Agua Dulce Unificado", 
                              cde_es$`Distrito Escolar`)

cde_es$`Distrito Escolar` <- gsub("Antelope Valley Union Joint High", 
                              "Escuela preparatoria del Antelope Valley Union Joint Unificado", 
                              cde_es$`Distrito Escolar`)

cde_es$`Distrito Escolar` <- gsub("Eastside Union Elementary", 
                              "Escuela Primaria del Eastside Union", 
                              cde_es$`Distrito Escolar`)

cde_es$`Distrito Escolar` <- gsub("Hughes-Elizabeth Lakes Union Elementary", 
                              "Primaria Hughes-Elizabeth Lakes Union", 
                              cde_es$`Distrito Escolar`)

cde_es$`Distrito Escolar` <- gsub("Keppel Union Elementary",  
                              "Primaria Keppel Union", 
                              cde_es$`Distrito Escolar`)

cde_es$`Distrito Escolar` <- gsub("Lancaster Elementary", 
                              "Primaria de Lancaster", 
                              cde_es$`Distrito Escolar`)

cde_es$`Distrito Escolar` <- gsub("Palmdale Elementary", 
                                  "Primaria de Palmdale", 
                                  cde_es$`Distrito Escolar`)

cde_es$`Distrito Escolar` <- gsub("Wilsona Elementary", 
                              "Primaria Wilsona", 
                              cde_es$`Distrito Escolar`)

cde_es$`Distrito Escolar` <- gsub("Westside Union Elementary", 
                                  "Primaria Westside Union", 
                                  cde_es$`Distrito Escolar`)

cde_es <- cde_es %>% mutate(`Distrito Escolar` = fct_relevel(`Distrito Escolar`, 
                                                      "Acton-Agua Dulce Unificado",  
                                                      "Escuela preparatoria del Antelope Valley Union Joint Unificado", 
                                                      "Escuela Primaria del Eastside Union", 
                                                      "Primaria Hughes-Elizabeth Lakes Union", 
                                                      "Primaria Keppel Union", 
                                                      "Primaria de Lancaster" , 
                                                      "Primaria Wilsona", "Primaria Westside Union",
                                                      "TOTAL" ))

cde_es <- cde_es %>% 
  pivot_wider(names_from = Name, names_glue = "{Name}_{.value}", values_from = c(Count, `Rate (%)`)) %>% gt() %>% 
  tab_spanner(label = "Niños de Crianza", columns = c(`Foster_Count`, `Foster_Rate (%)`)) %>% 
  tab_spanner(label = "Desfavorecido socioeconómicamente", columns = c(`Socioeconomically Disadvantaged_Count`, `Socioeconomically Disadvantaged_Rate (%)`)) %>% 
  tab_spanner(label = "Personas sin hogar", columns = c(`Homeless_Count`, `Homeless_Rate (%)`)) %>% 
  tab_spanner(label = "Personas con discapacidades", columns = c(`Disability_Count`, `Disability_Rate (%)`)) %>% opt_all_caps() %>% 
  tab_header(title = md("**Población estudiantil por subgrupo en el Antelope Valley, año escolar 2020-2021**")) %>% 
  tab_footnote (footnote = md("Fuente: Cálculos de Catalyst California de datos del Departamento de Educación de California, 
                2021-22.<br>
                Nota: Las escuelas de  Antelope Valley son escuelas de los siguientes distritos: Acton-Agua Dulce 
                Unificado, Antelope Valley Union High, Primaria Eastside Union, Primaria Hughes-Elizabeth Lakes Union, 
                Primaria Keppel Union, Primaria de Lancaster, Primaria de Palmdale, Primaria Westside Union
                y Primaria Wilsona. Desfavorecido socioeconómicamente significa que el estudiante es elegible
                a nivel federal para comidas gratuitas o reducidas según los requisitos de ingresos del hogar,
                el programa migrante o el programa de niños adoptivos. Los estudiantes también pueden ser categorizados 
                como socioeconómicamente desfavorecidos si los padres del estudiante no tienen un título de escuela 
                secundaria o si el estudiante no tiene un hogar, estaba certificado directamente, era un joven de adoptiva 
                tribal o estaba inscrito en una Escuela Tribunal Juve. El color más oscuro en el gráfico representa el 
                porcentaje más alto."))%>% #, https://www.cde_es.ca.gov/ds/ 
  data_color(columns = c(`Foster_Rate (%)`, `Socioeconomically Disadvantaged_Rate (%)`, `Homeless_Rate (%)`, `Disability_Rate (%)`), colors = scales::col_numeric(palette = c("white", lightblue), domain = NULL,
                                                                                                                                                                  na.color = textgrey)) %>% 
  tab_options(table.font.names = font_table_text,
              column_labels.background.color = "white",
              table.border.top.width = px(3),
              table.border.top.color = "transparent",
              table.border.bottom.color = "transparent",
              table.border.bottom.width = px(3),
              column_labels.border.top.width = px(3),
              column_labels.border.top.color = "transparent",
              column_labels.border.bottom.width = px(3),
              column_labels.border.bottom.color = black,
              data_row.padding = px(3),
              source_notes.font.size = 30,
              table.font.size = 50,
              heading.align = "left",
              heading.title.font.weight = "bolder") %>% 
  opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts())) %>% 
  cols_align(
    align = c("left"),
    columns = everything()
  )%>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = `Distrito Escolar`,
              rows = `Distrito Escolar` == "TOTAL"))%>% 
  cols_label(
    `Foster_Count` = "Conteo", 
    `Foster_Rate (%)` = "Tasa (%)",
    `Socioeconomically Disadvantaged_Count` = "Conteo", 
    `Socioeconomically Disadvantaged_Rate (%)` = "Tasa (%)",
    `Homeless_Count` = "Conteo", 
    `Homeless_Rate (%)` = "Tasa (%)",
    `Disability_Count` = "Conteo", 
    `Disability_Rate (%)` = "Tasa (%)")

#save the table
gtsave(cde_es, "cde_table_esp.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/Spanish/", vwidth = 3000, vheight = 2000)

