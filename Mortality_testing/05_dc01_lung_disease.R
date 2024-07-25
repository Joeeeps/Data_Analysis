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

DIR_FIG_OUT  <- paste0(DIR_FIG,"005/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"005/")
#presumably raw data will be inserted prior to script
  
if(!dir.exists(DIR_RAW)) dir.create(DIR_RAW)
if(!dir.exists(DIR_DAT)) dir.create(DIR_DAT)
if(!dir.exists(DIR_FIG)) dir.create(DIR_FIG)
if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

#### Start analysis #### 

#data from https://www.hse.gov.uk/statistics/gender/tables.htm CTRL + F "DC01", saved the first sheet of xlsx file as a .csv
#read data

lung_disease <- read.csv("rawdata/005/dc01_occupationally_related_lung_disease_no_meso_asbestois.csv", skip = 5) #skip = 5 skips the first five (irrelevant) rows
glimpse(lung_disease)  

#We want to make the data tidy

#merge the dates to a shared 'Year' column for tidy data 

lung_disease_edit <- lung_disease %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "Deaths")

# Remove "X" from the beginning of Year column
lung_disease_edit <- lung_disease_edit %>%
  mutate(Year = gsub("^X", "", Year))


#Rename 'lung.disease' into 'Cause_of_death'
lung_disease_edit <- lung_disease_edit %>%
 rename('Cause_of_death' = 'lung.disease') %>% rename('Sex' = 'sex')  #renaming sex to keep columns consistently lettered (i.e. start with capital letter) 

#removing total values as it gets in way
lung_disease_edit <- lung_disease_edit %>%
  filter(Cause_of_death != "Total") 

#only looking at data for all sex
lung_disease_edit <- lung_disease_edit %>%
  filter(Sex == "total") 



#Year column is categorical, causing plotting issues
#lung_disease_edit$Year <- as.integer(lung_disease_edit$Year)
#glimpse(lung_disease_edit)
#maybe not the issue actually 



#graph time
lung_disease_graph <- ggplot(data = lung_disease_edit, aes(x = Year, y = Deaths, color = Cause_of_death)) +
  geom_line(aes (group = "Year"))  +
  theme_bw() 

#graph broken without running it through plotly

  
lung_disease_plotly <-  ggplotly(lung_disease_graph, tooltip = c("Year", "Cause_of_death", "Deaths"))
lung_disease_plotly <- lung_disease_plotly %>%
  #layout(legend = list(x = 0, y = 1.08, # Adjust x and y coordinates as needed
  #                     orientation = "h")) %>%  # Set legend orientation to horizontal 
  layout(hovermode = 'x')
lung_disease_plotly
saveWidget(ggplotly(lung_disease_plotly), file = paste0(DIR_FIG_OUT,"lung_disease_graph.html")) #save as html, keeps interactive





