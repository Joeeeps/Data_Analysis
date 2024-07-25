#### read libraries ####
library(here)
library(tidyverse)
library(plotly) #for interactive plots
library(htmlwidgets) #for saving interactive plots as html
#setwd to relevant area first... hopefully source file location

#### define directories ####
DIR_PROJECT <- paste0(getwd(),"/") #can also do paste0(here()) as long as the root folder of here is in the relevant position
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")

DIR_FIG_OUT  <- paste0(DIR_FIG,"004/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"004/")
#presumably raw data will be inserted prior to script
  
if(!dir.exists(DIR_RAW)) dir.create(DIR_RAW)
if(!dir.exists(DIR_DAT)) dir.create(DIR_DAT)
if(!dir.exists(DIR_FIG)) dir.create(DIR_FIG)
if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

#### Start analysis #### 

#data from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/quarterlymortalityreports/octobertodecember2019
#testing interactive graphs
#read data

mortality_rates <- read.csv("rawdata/004/Mortality_rates_Q4_2019.csv") #deleted intro column to make data tidy
glimpse(mortality_rates)

mortality_rates_edit <- mortality_rates %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Age_group",
               values_to = "Mortality_rate")
# Remove "X" from the beginning of Age_group column
mortality_rates_edit <- mortality_rates_edit %>%
  mutate(Age_group = gsub("^X", "", Age_group))

mortality_rates_edit <- mortality_rates_edit %>%
  mutate(Age_group = case_when(
    Age_group == "70.to.74.years" ~ "70-74 years",
    Age_group == "75.to.79.years" ~ "75-79 years",
    Age_group == "80.to.84.years" ~ "80-84 years",
    Age_group == "85.to.89..years" ~ "85-89 years",
    Age_group == "90.years.and.over" ~ "90+ years",
    TRUE ~ Age_group  # Keep any other values as is
  ))

glimpse(mortality_rates_edit) 

mortality_rates_graph <- ggplot(data = mortality_rates_edit, aes(x = Period, y = Mortality_rate, color = Age_group)) +
  geom_line(aes (group = "Age_group"))  +
  theme_bw() +
  labs(color = "Age-standardised rates" , y = "Mortality rate")    # Set the legend title 
#graph broken without running it through plotly

  
mortality_rates_plotly <-  ggplotly(mortality_rates_graph, tooltip = c("Period", "Age_group", "Mortality_rate"))
mortality_rates_plotly <- mortality_rates_plotly %>%
  layout(legend = list(x = 0, y = 1.08, # Adjust x and y coordinates as needed
                       orientation = "h")) %>%  # Set legend orientation to horizontal 
  layout(hovermode = 'x')

saveWidget(ggplotly(mortality_rates_plotly), file = paste0(DIR_FIG_OUT,"mortality_rates_graph.html")) #save as html, keeps interactive





