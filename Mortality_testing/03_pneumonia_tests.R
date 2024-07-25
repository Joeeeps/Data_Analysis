#### read libraries ####
library(here)
library(tidyverse)
#setwd to relevant area first...  source file location should work

#### define directories ####
DIR_PROJECT <- paste0(getwd(),"/") #can also do paste0(here()) as long as the root folder of here is in the relevant position... it wasn't so i set source file location manually
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")

DIR_FIG_OUT  <- paste0(DIR_FIG,"003/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"003/")
#presumably raw data will be inserted prior to script
  
if(!dir.exists(DIR_RAW)) dir.create(DIR_RAW)
if(!dir.exists(DIR_DAT)) dir.create(DIR_DAT)
if(!dir.exists(DIR_FIG)) dir.create(DIR_FIG)
if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

#### Start analysis ####

mortality_pneumonia <- read.csv("rawdata/003/mortality_pneumonia.csv")

#Figures for PMR from Occupational Health Decennial Supplement book 1995 (https://iiif.wellcomecollection.org/pdf/b32237686)

#Have jobs, observed deaths, age of death, PMR, and confidence intervals. 
#Need to calculate expected deaths, E = (O/PMR) * 100

mortality_pneumonia <- mortality_pneumonia %>%
  mutate(Expected_deaths = (Observed_deaths / PMR) * 100)


#Now make a graph comparing actual deaths versus expected, maybe line graph

ggplot(data = mortality_pneumonia) +
  geom_col(aes(x = Job_group, y = Expected_deaths, fill = Age), position = "dodge") + 
  geom_point(aes(x = Job_group, y = Observed_deaths, group = Age, color = Age), size = 1, position = position_dodge(width = 0.9)) +
  geom_line(aes(x = Job_group, y = Observed_deaths, group = Age, color = Age), size = .5, position = position_dodge(width = 0.9)) +
  theme_bw() +
  labs(title = "Expected vs Observed Deaths by Job Group and Age",
       x = "Job Group",
       y = "Number of Deaths",
       fill = "Age Group",
       color = "Age Group") +
  guides(fill = guide_legend(title = "Age Group"),
         color = guide_legend(title = "Age Group"))
ggsave(paste0(DIR_FIG_OUT,"expected_vs_observed_deaths_by_job.png"), width = 16, height = 9)


#Now make a graph comparing PMR by job group, and include the 95% confidence intervals

ggplot(data = mortality_pneumonia) +
  geom_col(aes(x = Job_group, y = PMR, fill = Age), position = "dodge") +
  geom_errorbar(aes(x = Job_group, ymin = Lower_95CI, ymax = Upper_95CI, fill = Age),
                position = position_dodge(width = 0.9),  # Dodge bars
                width = 0.2,  # Adjust width of error bars
                color = "black",  # Error bar color
                size = 0.7) +  # Error bar size
  geom_hline(yintercept = 100, linetype = "dashed", color = "green", size = 1) +
  labs(x = "Job Group", y = "Proportional Mortality Rate", title = "Proportional mortality rate by job group and age\
Green dashed lines = PMR 100") +
  theme_bw()
ggsave(paste0(DIR_FIG_OUT,"PMR_by_job_group.png"), width = 16, height = 9)

#Note, the CI having such a drastic range is (probably) caused by low samples size
 