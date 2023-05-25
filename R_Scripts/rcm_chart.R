## Real Cost Measure Dumbbell Plot

# install packages if not already installed -----
list.of.packages <- c("dplyr","stringr","showtext","ggplot2","forcats","ggtext") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 

if(length(new.packages)) install.packages(new.packages) 


# Prep and Set Up 
#### Loading Libraries ####
library(dplyr)
library(stringr)
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

# Real Cost Measure Bar Chart ----

source("W:\\Project\\RDA Team\\Region 5 State of the Child\\R\\cost_of_living.R")
cost <- rcm_data %>% dplyr::rename("Total Households" ="% Households Below RCM",                      
                                   "White"="% White Households Below RCM",               
                                   "Black" ="% Black Households Below RCM",                
                                   "Latinx"="% Latino Households Below RCM",               
                                   "Children Under 6"="% Households with a child under 6 Below RCM"
) %>% dplyr::select(`Total Households`, White, Black, Latinx, `Children Under 6`, Geography) %>% 
  dplyr::filter(Geography=="Los Angeles County"|Geography=="Los Angeles County (North Central)--Lancaster City"|Geography=="Los Angeles County (North Central)--Palmdale City") #%>% 


cost2 <- cost %>% dplyr::filter(Geography=="Los Angeles County (North Central)--Lancaster City"|Geography=="Los Angeles County (North Central)--Palmdale City") %>% 
  summarise(Geography = c(Geography, 'Antelope Valley'),
            across(where(is.numeric), ~ c(., mean(.))))
cost2$Black[cost2$Geography=="Antelope Valley"] <- 66.6
cost2 <- cost2 %>% dplyr::filter(Geography == "Antelope Valley")

cost <- rbind(cost, cost2) %>% dplyr::filter(Geography=="Los Angeles County"|Geography=="Antelope Valley")
s <- cost

engvar = c("Total Households", "White",               
           "Black",                
           "Latinx",               
           "Children Under 6")
labels <- data.frame(engvar)

# convert df to long format
s_long <- reshape2::melt(s, id.vars=c("Geography"))

# Round 'value' field to 1 decimal, rename 'variable' field, add Spanish labels
s_long <- s_long %>% dplyr::mutate(value = round(value, 1)) %>% dplyr::rename(engvar = variable)  %>% 
  left_join(labels, by = "engvar")
s_long <- s_long %>% dplyr::filter(value > 0)

rcm_df <- na.omit(s_long)             # drop NA's
rcm_df_diff <- rcm_df %>% group_by(engvar) %>% 
  mutate(diff = (value[Geography == "Antelope Valley"]-value[Geography == "Los Angeles County"])*-1) %>% 
  dplyr::select(engvar, diff) %>% unique() %>% dplyr::rename("Subgroup" = "engvar", "Rate Difference" = "diff") %>% ungroup() %>% as.data.frame()


#sort rcm_df for ggplot
rcm_df <- rcm_df %>% mutate(engvar = fct_relevel(engvar, "Children Under 6", "White", "Latinx", "Black", "Total Households")) %>% 
  group_by(engvar) %>% 
  mutate(diff = (value[Geography == "Los Angeles County"] - value[Geography == "Antelope Valley"])*-1) %>% 
  ungroup()

lac_bell <- rcm_df %>% dplyr::filter(Geography=="Los Angeles County")
av_bell<- rcm_df %>% dplyr::filter(Geography=="Antelope Valley")

# Define chart height as max y value varies by indicator. Set vertical position for total line label - may need to be customized for each chart.

# # ggplot method that should export as .svg ##
rcm_plot <-
  ggplot(rcm_df) + # set up manual fill using 'test', add "-" before value to order bars when MAX is best
  labs(title = "Compared to <span style='color:#009CDB;'>Los Angeles County</span>, <span style='color:#332985;'>Antelope Valley</span> Families are <br>
More Likely to Earn Incomes Below the Real Cost of Living", caption = "Source: Catalyst California's calculations based on the United Ways of California Real Cost Measure, 2019.
Note: The Real Cost Measure is a measure of poverty that factors in the costs of housing, health care, child
care, transportation and other basic needs to reveal what it really costs to live in California.") + #, https://www.unitedwaysca.org/realcost/39-real-cost. 
  geom_segment(data = av_bell,
               aes(x = engvar, y = value,
                   yend = lac_bell$value, xend = lac_bell$engvar), 
               color = textgrey, size = 4.5, alpha = .5) +
  geom_point(aes(x = engvar, y = value, color= Geography), size = 4, show.legend = FALSE)+
  scale_color_manual(values = c(darkblue, lightblue))+
  scale_x_discrete(labels = function(engvar) str_wrap(engvar, width = 20)) +            # wrap long labels, 
  xlab("") +
  ylab("") +
  theme_void()+
  geom_text(data=dplyr::filter(rcm_df, Geography=="Los Angeles County"),
            aes(x = engvar, y = value, label=paste0(value, "%"   )), 
            color=lightblue, size=7, vjust=-1.5, fontface="bold", family=font_bar_label, hjust = 0.3)+
  geom_text(data=dplyr::filter(rcm_df, Geography=="Antelope Valley"),
            aes(x = engvar, y = value, label=paste0(value, "%")), 
            color=darkblue, size=7, vjust=-1.5, fontface="bold", family=font_bar_label , hjust = -0.1)+
  
  geom_rect(data=rcm_df, aes(xmin=-Inf, xmax=Inf, ymin=90, ymax=120), fill="#C0C0C0") +
  
  geom_text(data=dplyr::filter(rcm_df, engvar=="Total Households" & Geography=="Antelope Valley"), aes(label=paste0(diff, " Points Higher"), x=5, y=105), fontface="bold", size=9, family=font_bar_label) +
  geom_text(data=dplyr::filter(rcm_df, engvar=="Black" & Geography=="Antelope Valley"), aes(label=paste0(diff, " Points Higher"), x=4, y=105), fontface="bold", size=9, family=font_bar_label) +
  geom_text(data=dplyr::filter(rcm_df, engvar=="Latinx" & Geography=="Antelope Valley"), aes(label=paste0(diff, " Points Higher"), x=3, y=105), fontface="bold", size=9, family=font_bar_label) +
  geom_text(data=dplyr::filter(rcm_df, engvar=="White" & Geography=="Antelope Valley"), aes(label=paste0(diff, " Points Higher"), x=2, y=105), fontface="bold", size=9, family=font_bar_label) +
  geom_text(data=dplyr::filter(rcm_df, engvar=="Children Under 6" & Geography=="Antelope Valley"), aes(label=paste0(diff, " Points Higher"), x=1, y=105), fontface="bold", size=9, family=font_bar_label) +
  coord_flip()

rcm_plot <- rcm_plot + theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y =element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 24, family= font_axis_label, hjust = 0), 
  axis.ticks = element_blank(),
  plot.title = element_markdown(family = font_title, face = "bold", size = 32, hjust = 0, lineheight=.4, margin=margin(0,0,4,0)),
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.caption = element_text(family = font_caption, face = "bold", hjust = 0, size = 20,lineheight=0.35), 
  plot.margin = margin(t = 3,
                       b = 3,
                       r = 3,
                       l = 3)
)

rcm_plot

ggsave(file="rcm_chart.png", path="W:/Project/RDA Team/Region 5 State of the Child/GitHub/AB/State_of_the_Child_Region_5/Images/",
       plot=rcm_plot, bg='transparent', width = 5, height=4)
