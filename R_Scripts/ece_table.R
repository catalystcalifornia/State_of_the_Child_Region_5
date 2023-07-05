# install packages if not already installed ----
list.of.packages <- c("dplyr", "gt", "showtext", "tidyr") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

#### Loading Libraries ####
library(dplyr)
library(tidyr)
library(gt)
library(showtext)
library(RPostgreSQL)
library(sf)
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


# ECE ----

source("W:/Project/RDA Team/Region 5 State of the Child/R/ece.R") 

ece_access_it_2020 <- ece_access_it_2020 %>% dplyr::rename("With Unmet Need"="Unmet need for infant/toddler care", "% with Unmet Need"="% of infant/toddlers with unmet need") %>% mutate(Name = "Infants & Toddlers")

ece_access_prek_2020 <- ece_access_prek_2020 %>% dplyr::rename("With Unmet Need"="Unmet need for preschool", "% with Unmet Need"="% of preschoolers with unmet need") %>% mutate(Name = "Preschoolers")

ece <- rbind(ece_access_it_2020,ece_access_prek_2020)%>% group_by(Name) %>% mutate(across(c(`With Unmet Need`), comma)) %>% mutate(across(where(is.numeric), ~ round(., 1))) %>%
  gt(
    groupname_col = "Name") %>% 
  tab_row_group(label = "numbered", rows = matches("^[0-3]")) %>% 
  opt_all_caps() %>% 
  tab_header(title = md("**Top Six Antelope Valley ZIP Codes for Unmet Need for Infants and Toddlers and Preschool Age Children, 2020**")) %>% 
  tab_footnote (footnote = md("Source: Catalyst California's calculations based on the American Institutes of Research, 2020.<br>
                              Note: Unmet need is the estimated number of children in families earning less than 85% of the State median 
                              income minus the total number of children enrolled in publicly subsidized early learning and care programs. 
                              Total enrollment in publicly subsidized programs includes enrollment in Title 5 State Preschool Program, 
                              Title 5 Migrant Child Care Program, Title 5 Center-Based, Title 5 Family Child Care Home Network, Head Start/Early 
                              Head Start, CalWORKs Stage 2, CalWORKs Stage 3, estimated Transitional Kindergarten eligibility for Title 5, and 
                              the Alternative Payment program."))%>% # https://elneedsassessment.org/
  data_color(
    columns = c(`% with Unmet Need`),
    rows = (Name == "Infants & Toddlers"),
    colors = scales::col_numeric(
      palette = c("white", lightblue),
      domain = NULL,
      na.color = textgrey
    )
  ) %>% 
  data_color(
    columns = c(`% with Unmet Need`),
    rows = (Name == "Preschoolers"),
    colors = scales::col_numeric(
      palette = c("white", lightblue),
      domain = NULL,
      na.color = textgrey
    )
  ) %>%   cols_width(
    Neighborhood ~ px(150),
  ) %>% 
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
              source_notes.font.size = 8,
              table.font.size = 12,
              heading.align = "left",
              container.width = 500
  ) %>% 
  opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts()))
gtsave(ece, "ece_table.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/English/", vwidth = 4000, vheight = 6000)


# Spanish Version ------
source("W:/Project/RDA Team/Region 5 State of the Child/R/ece.R") 

ece_access_it_2020 <- ece_access_it_2020 %>% 
  dplyr::rename("Con necesidad insatisfecha"="Unmet need for infant/toddler care", 
                "% con necesidad insatisfecha"="% of infant/toddlers with unmet need", "Vecindario" = "Neighborhood", "Código Postal" = "ZIP code") %>%
  mutate(Name = "Bebés y Niños Pequeños")

ece_access_prek_2020 <- ece_access_prek_2020 %>% 
  dplyr::rename("Con necesidad insatisfecha"="Unmet need for preschool", 
                "% con necesidad insatisfecha"="% of preschoolers with unmet need", "Vecindario" = "Neighborhood", "Código Postal" = "ZIP code") %>% 
  mutate(Name = "Preescolares")

ece <- rbind(ece_access_it_2020,ece_access_prek_2020)

ece$Vecindario <- gsub("Palmdale, Leona Valley, Southeast Antelope Valley","Palmdale, Leona Valley, Sudeste de Antelope Valley", ece$Vecindario)
ece$Vecindario <- gsub("Northwest Antelope Valley, Lancaster","Noroeste de Antelope Valley, Lancaster", ece$Vecindario)
ece$Vecindario <- gsub("Palmdale, Angeles Crest, Southeast Antelope Valley","Palmdale, Angeles Crest, Sudeste de Antelope Valley", ece$Vecindario)
ece$Vecindario <- gsub("Tujunga Canyons, Palmdale, Angeles Crest","Tujunga Canyons, Palmdale, Angeles Crest", ece$Vecindario)
ece$Vecindario <- gsub("Lancaster, Northeast Antelope Valley","Lancaster, Noreste del Antelope Valley", ece$Vecindario)
ece$Vecindario <- gsub("Northeast Antelope Valley, Lancaster","Noreste de Antelope Valley, Lancaster", ece$Vecindario)

ece <- ece %>%   group_by(Name) %>% 
  mutate(across(c(`Con necesidad insatisfecha`), comma)) %>% 
  mutate(across(where(is.numeric), ~ round(., 1))) %>%
  gt(groupname_col = "Name") %>% 
  tab_row_group(label = "numbered", rows = matches("^[0-3]")) %>% 
  opt_all_caps() %>% 
  tab_header(title = md("**Los seis códigos postales principales de Antelope Valley por Necesidades Insatisfechas de Bebes y Niños Pequeños y Niños en Edad Preescolar, 2020**")) %>% 
  tab_footnote (footnote = md("Fuente: Cálculos de Catalyst California basados en Institutos Americano de Investigación, 2020.<br>
                              Nota: La necesidad insatisfecha es el número estimado de niños en familias que ganan menos del 
                              85% del Estado mediano ingresos menos el número total de niños matriculados en programas de 
                              aprendizaje y cuidado temprano subsidiados públicamente. La inscripción total en programas subsidiados 
                              con fondos públicos incluye la inscripción en el Programa Preescolar Estatal del Título 5, Titulo 5 de 
                              Cuidado Infantil para Migrantes, Título 5 Basado en el Centro, Título 5 Red de Hogares de Cuidado 
                              Infantil Familiar, Head Start/Early Head Start, CalWORKs Etapa 2, CalWORKs Etapa 3, elegibilidad 
                              estimada de Kindergarten de transición para el Título 5, y el programa de Pago Alternativo."))%>% # https://elneedsassessment.org/
  data_color(
    columns = c(`% con necesidad insatisfecha`),
    rows = (Name == "Bebés y Niños Pequeños"),
    colors = scales::col_numeric(
      palette = c("white", lightblue),
      domain = NULL,
      na.color = textgrey
    )
  ) %>% 
  data_color(
    columns = c(`% con necesidad insatisfecha`),
    rows = (Name == "Preescolares"),
    colors = scales::col_numeric(
      palette = c("white", lightblue),
      domain = NULL,
      na.color = textgrey
    )
  ) %>% 
  cols_width(
    Vecindario ~ px(150),
  ) %>% 
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
              source_notes.font.size = 8,
              table.font.size = 12,
              heading.align = "left",
              container.width = 500
  ) %>% 
  opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts()))
gtsave(ece, "ece_table_esp.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/Spanish/", vwidth = 4000, vheight = 6000)

