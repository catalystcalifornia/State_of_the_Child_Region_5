# install packages if not already installed ----
list.of.packages <- c("dplyr", "gt", "showtext", "tidyr") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 

#### Loading Libraries ####
library(dplyr)
library(tidyr)
library(gt)
library(showtext)
library(scales)

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

# Suspensions ----

#load in data
source("W:/Project/RDA Team/Region 5 State of the Child/R/suspension.R")

#rename to what we want to use in the visuals
susp_table$`Student group` <- gsub("Total", "TOTAL",susp_table$`Student group`)
susp_table$`Student group` <- gsub("African American", "Black",susp_table$`Student group`)
susp_table$`Student group` <- gsub("Hispanic or Latino", "Latinx",susp_table$`Student group`)
susp_table$`Student group` <- gsub("American Indian or Alaska Native", "AIAN",susp_table$`Student group`)
susp_table$`Student group` <- gsub("Filipino", "Filipinx",susp_table$`Student group`)

suspensions <- susp_table %>%
  dplyr::filter(`Student group`== "Foster" | `Student group`== "Black" |
          `Student group`== "Students with Disabilities"| `Student group`== "AIAN"| 
           `Student group`=="Pacific Islander"| `Student group`=="Male"| `Student group`=="Socioeconomically Disadvantaged"| 
           `Student group`=="Two or More Races"| `Student group`=="TOTAL"| `Student group`=="Homeless"| 
           `Student group`=="Migrant"| `Student group`=="English Learners"| `Student group`=="Latinx"| 
           `Student group`=="Female"| `Student group`=="White"| `Student group`=="Filipinx"| `Student group`=="Asian") %>%
  mutate(across(c(`Student enrollment`, `Unduplicated count of students suspended`), comma)) %>% 
  gt() %>% opt_all_caps() %>% 
  tab_header(title = md("**Suspension Rates by Student Group, Antelope Valley School Districts, 2021-22**")) %>% 
    tab_footnote (footnote = md("Source: Catalyst California calculations of California Department of Education data, 2021-22.<br>
                                Note: The student group category for non-binary students was unavailable because of a lack of data. 
                                Antelope Valley schools are from the following districts: Acton-Agua Dulce Unified, 
                                Antelope Valley Union High, Eastside Union Elementary, Hughes-Elizabeth Lakes Union Elementary, 
                                Keppel Union Elementary, Lancaster Elementary, Palmdale Elementary, Westside Union Elementary, 
                                and Wilsona Elementary.  AIAN stands for American Indian Alaskan Native."))%>% #, https://www.cde.ca.gov/ds/ 
  cols_align(
    align = c("left"),
    columns = everything()
  )%>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(
              columns = `Student group`,
              rows = `Student group` == "TOTAL"))%>% 
  data_color(
    columns = c(`Suspension rate`),
    colors = scales::col_numeric(
      palette = c("white", lightblue),
      domain = NULL,
      na.color = textgrey
    )
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
gtsave(suspensions, "suspensions_table_old.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/English/")

# # ECE ----
# 
# source("W:/Project/RDA Team/Region 5 State of the Child/R/ece.R") 
# 
# ece_access_it_2020 <- ece_access_it_2020 %>% dplyr::rename("With Unmet Need"="Unmet need for infant/toddler care", "% with Unmet Need"="% of infant/toddlers with unmet need") %>% mutate(Name = "Infants & Toddlers")
# 
# ece_access_prek_2020 <- ece_access_prek_2020 %>% dplyr::rename("With Unmet Need"="Unmet need for preschool", "% with Unmet Need"="% of preschoolers with unmet need") %>% mutate(Name = "Preschoolers")
# 
# ece <- rbind(ece_access_it_2020,ece_access_prek_2020)%>% group_by(Name) %>% mutate(across(c(`With Unmet Need`), comma)) %>% mutate(across(where(is.numeric), ~ round(., 1))) %>%
#   gt(
#      groupname_col = "Name") %>% 
#   tab_row_group(label = "numbered", rows = matches("^[0-3]")) %>% 
#  opt_all_caps() %>% 
#   tab_header(title = md("**Top Six Antelope Valley ZIP Codes for Unmet Need for Infants and Toddlers and Preschool Age Children, 2020**")) %>% 
#   tab_footnote (footnote = md("Source: Catalyst California's calculations based on the American Institutes of Research, 2020.<br>
#                               Note: Unmet need is the estimated number of children in families earning less than 85% of the State median 
#                               income minus the total number of children enrolled in publicly subsidized early learning and care programs. 
#                               Total enrollment in publicly subsidized programs includes enrollment in Title 5 State Preschool Program, 
#                               Title 5 Migrant Child Care Program, Title 5 Center-Based, Title 5 Family Child Care Home Network, Head Start/Early 
#                               Head Start, CalWORKs Stage 2, CalWORKs Stage 3, estimated Transitional Kindergarten eligibility for Title 5, and 
#                               the Alternative Payment program."))%>% # https://elneedsassessment.org/
#   data_color(
#     columns = c(`% with Unmet Need`),
#     rows = (Name == "Infants & Toddlers"),
#     colors = scales::col_numeric(
#       palette = c("white", lightblue),
#       domain = NULL,
#       na.color = textgrey
#     )
#   ) %>% 
#   data_color(
#     columns = c(`% with Unmet Need`),
#     rows = (Name == "Preschoolers"),
#     colors = scales::col_numeric(
#       palette = c("white", lightblue),
#       domain = NULL,
#       na.color = textgrey
#     )
#   ) %>% 
#   tab_options(table.font.names = font_table_text,
#               column_labels.background.color = "white",
#               table.border.top.width = px(3),
#               table.border.top.color = "transparent",
#               table.border.bottom.color = "transparent",
#               table.border.bottom.width = px(3),
#               column_labels.border.top.width = px(3),
#               column_labels.border.top.color = "transparent",
#               column_labels.border.bottom.width = px(3),
#               column_labels.border.bottom.color = black,
#               data_row.padding = px(3),
#               source_notes.font.size = 8,
#               table.font.size = 12,
#               heading.align = "left",
#               container.width = 500
#   ) %>% 
#   opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts()))
# gtsave(ece, "ece_table.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/")
# 
# 
# #CDE table-----
# 
# #load foster data
# source("W:/Project/RDA Team/Region 5 State of the Child/R/foster.R")
# av_table <- av_table %>% mutate(across(c(`2021-22 Count`), comma))%>% rename("Count"="2021-22 Count", "School District"="Geography")
# av_table$Name <- "Foster"
# cde_foster <- av_table
# 
# 
# #load socioeconomically disadvantaged data
# source("W:/Project/RDA Team/Region 5 State of the Child/R/socioeconomically_disadvantaged.R")
# av_table <- av_table %>% mutate(across(c(`2021-22 Count`), comma)) %>% rename("Count"="2021-22 Count", "School District"="Geography")
# av_table$Name <- "Socioeconomically Disadvantaged"
# cde_socieconomically_disadvantaged <- av_table 
# 
# # Student Homelessness
# source("W:/Project/RDA Team/Region 5 State of the Child/R/student_homelessness.R")
# av_table <- av_table %>% mutate(across(c(`2021-22 Count`), comma))%>% rename("Count"="2021-22 Count", "School District"="Geography")
# av_table$Name <- "Homeless"
# cde_student_homelessness <- av_table
# 
# # Students with Disabilities
# source("W:/Project/RDA Team/Region 5 State of the Child/R/disabilties.R")
# av_table <- av_table %>% mutate(across(c(`2021-22 Count`), comma))%>% rename("Count"="2021-22 Count", "School District"="Geography")
# av_table$Name <- "Disability"
# cde_disability <- av_table
# 
# 
# # combine them
# cde <- rbind(cde_foster, cde_socieconomically_disadvantaged, cde_student_homelessness, cde_disability)
# 
# cde$`School District` <- gsub("Antelope Valley Best Start Region 5","TOTAL", cde$`School District`)
# cde$`School District` <- gsub(" School District","", cde$`School District`)
# cde <- cde %>% mutate(`School District` = fct_relevel(`School District`, 
#                                               "Acton-Agua Dulce Unified School District", "Antelope Valley Union Joint High School District", 
#                                               "Eastside Union Elementary School District", "Hughes-Elizabeth Lakes Union Elementary School District", 
#                                               "Keppel Union Elementary School District", "Lancaster Elementary School District", 
#                                               "Palmdale Elementary School District", "Westside Union Elementary School District", 
#                                               "Wilsona Union Elementary School District", "TOTAL"))
# 
# cde <- cde %>% 
#   pivot_wider(names_from = Name, names_glue = "{Name}_{.value}", values_from = c(Count, `Rate (%)`)) %>% gt() %>% 
#   tab_spanner(label = "Foster", columns = c(`Foster_Count`, `Foster_Rate (%)`)) %>% 
#   tab_spanner(label = "Socioeconomically Disadvantaged", columns = c(`Socioeconomically Disadvantaged_Count`, `Socioeconomically Disadvantaged_Rate (%)`)) %>% 
#   tab_spanner(label = "Homeless", columns = c(`Homeless_Count`, `Homeless_Rate (%)`)) %>% 
#   tab_spanner(label = "Disability", columns = c(`Disability_Count`, `Disability_Rate (%)`)) %>% opt_all_caps() %>% 
#   tab_header(title = md("**Student Population by Subgroup in the Antelope Valley, 2020-2021 School Year**")) %>% 
#   tab_footnote (footnote = md("Source: Catalyst California calculations based on the California Department of Education data, 2021-22.<br>
#                               Note: Antelope Valley schools are schools from the following districts: Acton-Agua Dulce Unified, Antelope Valley 
#                               Union High, Eastside Union Elementary, Hughes-Elizabeth Lakes Union Elementary, Keppel Union Elementary, 
#                               Lancaster Elementary, Palmdale Elementary, Westside Union Elementary, and Wilsona Elementary. 
#                               Socioeconomically disadvantaged means the student is federally eligible for free and reduced meals based 
#                               on household income requirements, the migrant program, or the foster program. Students can also be categorized 
#                               as socioeconomically disadvantaged if the student's parents do not have a high school diploma or the student 
#                               was homeless, directly certified, a tribal foster youth, or enrolled in a juvenile court school. The darkest 
#                               color in the chart represents the highest percentage."))%>% #, https://www.cde.ca.gov/ds/ 
#   data_color(columns = c(`Foster_Rate (%)`, `Socioeconomically Disadvantaged_Rate (%)`, `Homeless_Rate (%)`, `Disability_Rate (%)`), colors = scales::col_numeric(palette = c("white", lightblue), domain = NULL,
#                                                                                                                                                                   na.color = textgrey)) %>% 
#   tab_options(table.font.names = font_table_text,
#               column_labels.background.color = "white",
#               table.border.top.width = px(3),
#               table.border.top.color = "transparent",
#               table.border.bottom.color = "transparent",
#               table.border.bottom.width = px(3),
#               column_labels.border.top.width = px(3),
#               column_labels.border.top.color = "transparent",
#               column_labels.border.bottom.width = px(3),
#               column_labels.border.bottom.color = black,
#               data_row.padding = px(3),
#               source_notes.font.size = 8,
#               table.font.size = 12,
#               heading.align = "left",
#               heading.title.font.weight = "bolder") %>% 
#   opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts())) %>% 
#   cols_align(
#     align = c("left"),
#     columns = everything()
#   )%>%
#   tab_style(style = cell_text(weight = "bold"),
#             locations = cells_body(
#               columns = `School District`,
#               rows = `School District` == "TOTAL"))%>% 
#   cols_label(
#     `Foster_Count` = "Count", 
#     `Foster_Rate (%)` = "Rate (%)",
#     `Socioeconomically Disadvantaged_Count` = "Count", 
#     `Socioeconomically Disadvantaged_Rate (%)` = "Rate (%)",
#     `Homeless_Count` = "Count", 
#     `Homeless_Rate (%)` = "Rate (%)",
#     `Disability_Count` = "Count", 
#     `Disability_Rate (%)` = "Rate (%)")
# 
# #save the table
# gtsave(cde, "cde_table.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/")
# 
# # Age table----
# 
# 
# under18race <- data.frame(Race_ethnicity = c(
#   "AIAN",
#   "Asian",
#   "Black",
#   "Latinx",
#   "Non-Hispanic White",
#   "NHPI",
#   "Other",
#   "Multiracial",
#   "TOTAL"),
#   Count = c(
#     "1943",
#     "3580",
#     "17529",
#     "72301",
#     "22111",
#     "454",
#     "26529",
#     "16617",
#     "120525"),
#   Percentage = c(
#     "1.6",
#     "3.0",
#     "14.5",
#     "60.0",
#     "18.3",
#     "0.4",
#     "22.0",
#     "13.8", 
#     NA))
# 
# under18race$Count <- as.numeric(under18race$Count)
# under18race$Percentage <- as.numeric(under18race$Percentage)
# 
# under18race <- under18race  %>%
#   mutate(across(c(Count), comma)) %>% 
#   gt() %>% opt_all_caps() %>% 
#   tab_header(title = md("**Race/ethnicity of Antelope Valley Children Under Age 18**")) %>% 
#   tab_footnote (footnote = md("Source: Catalyst California calculations based on the American Community Survey 2021 5-year estimates table B01001 and B01001B-I.<br>Note: Rates are out of 100 students, American Indian and Alaska Native (AIAN), Native Hawaiian and  Pacific Islander (NHPI). Numbers and percentages of racial-ethnic groups will exceed the totals because Hispanics are double counted in every racial-ethnic group, except for non-Hispanic Whites."))%>% 
#   data_color(
#     columns = c(`Percentage`),
#     colors = scales::col_numeric(
#       palette = c("white", lightblue),
#       domain = NULL,
#       # na.color = textgrey
#       na.color = "#D3D3D3" #code for light grey
#     )
#   ) %>% 
#   tab_options(table.font.names = font_table_text,
#               column_labels.background.color = "white",
#               table.border.top.width = px(3),
#               table.border.top.color = "transparent",
#               table.border.bottom.color = "transparent",
#               table.border.bottom.width = px(3),
#               column_labels.border.top.width = px(3),
#               column_labels.border.top.color = "transparent",
#               column_labels.border.bottom.width = px(3),
#               column_labels.border.bottom.color = black,
#               data_row.padding = px(3),
#               source_notes.font.size = 8,
#               table.font.size = 12,
#               heading.align = "left",
#               container.width = 500
#   ) %>% 
#   opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts()))
# gtsave(under18race, "race_under_18_table.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/")
# 
# 
# #hs_grad -----
# source("W:/Project/RDA Team/Region 5 State of the Child/R/hs_grad.R")
# # View(av_table)
# hs_grad <- t(av_table) %>% as.data.frame() 
# hs_grad <- cbind(rownames(hs_grad), hs_grad)
# rownames(hs_grad) <- NULL
# colnames(hs_grad) <- c("Subgroups", "Rate", "Count")
# 
# hs_grad <- hs_grad[-1,]
# hs_grad$Rate <- as.numeric(hs_grad$Rate)
# hs_grad$Count <- as.numeric(gsub(",", "", hs_grad$Count))
# hs_grad <- hs_grad %>% arrange(desc(Rate)) %>% 
#   mutate(across(c(Count), comma)) %>% 
#   gt() %>% opt_all_caps() %>% cols_move(columns = Rate, after = Count) %>% 
#   tab_header(title = md("**High School Graduation Rates by Student Group, Antelope Valley School Districts, 2021-22**")) %>% 
#   tab_footnote (footnote = md("Source: Catalyst California calculations based on the California Department of Education data, 2021-22.<br>Note: The category for Non-Binary Students was unavailable because of a lack of data. Antelope Valley schools are schools from the following districts: Acton-Agua Dulce Unified, Antelope Valley Union High, Eastside Union Elementary, Hughes-Elizabeth Lakes Union Elementary, Keppel Union Elementary, Lancaster Elementary, Palmdale Elementary, Westside Union Elementary, and Wilsona Elementary. "))%>% #, https://www.cde.ca.gov/ds/
#   data_color(columns = c(Rate), colors = scales::col_numeric(palette = c("white", lightblue), domain = NULL,
#                                                              na.color = textgrey)) %>% 
#   tab_options(table.font.names = font_table_text,
#               column_labels.background.color = "white",
#               table.border.top.width = px(3),
#               table.border.top.color = "transparent",
#               table.border.bottom.color = "transparent",
#               table.border.bottom.width = px(3),
#               column_labels.border.top.width = px(3),
#               column_labels.border.top.color = "transparent",
#               column_labels.border.bottom.width = px(3),
#               column_labels.border.bottom.color = black,
#               data_row.padding = px(3),
#               source_notes.font.size = 8,
#               table.font.size = 12,
#               heading.align = "left",
#               container.width = 500) %>% 
#   opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts()))
# gtsave(hs_grad, "hs_grad_table.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/")
# 
# # Student vs. Teacher Diversity ------
# con <- connect_to_db("region5stateofthechild")
# 
# av_staff_student_race <- st_read(con, query = "SELECT * FROM av_staff_student_race")
# 
# av_staff_student_race <- av_staff_student_race %>% arrange(desc(enrollment)) %>% 
#   mutate(across(c(enrollment, staff), comma)) %>% gt() %>% 
#   tab_spanner(label = "Students", columns = c(`enrollment`, `enrollment_rate`)) %>% 
#   tab_spanner(label = "School Staff", columns = c(`staff`, `staff_rate`)) %>% 
#   opt_all_caps() %>% 
#   tab_header(title = md("**Comparing Student and Teacher/Staff Diversity, 2018-19**")) %>% 
#   tab_footnote (footnote = md("Source: Catalyst California's calculations based on the California Department of Education, 2018-19. Shares of student enrollment & certificated staff in Antelope Valley schools, 2018-19"))%>% #, https://www.cde.ca.gov/ds/
#   data_color(columns = c(`staff_rate`, `enrollment_rate`), colors = scales::col_numeric(palette = c("white", lightblue), domain = NULL, na.color = NULL)) %>% 
#   tab_options(table.font.names = font_table_text,
#               column_labels.background.color = "white",
#               table.border.top.width = px(3),
#               table.border.top.color = "transparent",
#               table.border.bottom.color = "transparent",
#               table.border.bottom.width = px(3),
#               column_labels.border.top.width = px(3),
#               column_labels.border.top.color = "transparent",
#               column_labels.border.bottom.width = px(3),
#               column_labels.border.bottom.color = black,
#               data_row.padding = px(3),
#               source_notes.font.size = 8,
#               table.font.size = 12,
#               heading.align = "left",
#               heading.title.font.weight = "bolder") %>% 
#   opt_table_font(font = list(google_font(name = font_table_text), font_title, font_caption ,default_fonts())) %>% 
#   cols_label(
#     `enrollment` = "Count", 
#     `staff` = "Count",
#     `enrollment_rate` = "Rate (%)", 
#     `staff_rate` = "Rate (%)",
#     `rate_difference` = "Rate Difference",
#     `race_ethnicity` = " Race/Ethnicity")
# 
# #save the table
# gtsave(av_staff_student_race, "student_v_staff_table.png",path = "W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/")
# 
# 
# 
