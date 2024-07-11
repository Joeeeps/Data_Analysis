#### read libraries ####
library(here)
library(tidyverse)
#setwd to relevant area first... hopefully source file location

#### define directories ####
DIR_PROJECT <- paste0(getwd(),"/") #can also do paste0(here()) as long as the root folder of here is in the relevant position
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")
DIR_FIG_OUT  <- paste0(DIR_FIG,"002/") 
DIR_DAT_OUT  <- paste0(DIR_DAT,"002/") #presumably raw data will be inserted prior to script
  
if(!dir.exists(DIR_RAW)) dir.create(DIR_RAW)
if(!dir.exists(DIR_DAT)) dir.create(DIR_DAT)
if(!dir.exists(DIR_FIG)) dir.create(DIR_FIG)
if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

#### Start analysis ####

weekly_deaths_age_2024 <- read.csv("rawdata/002/weekly_deaths_age_2024.csv")

#Testing proportional death ratio and standard death ratio code writing

#Dataset is from 2024 and includes a list of deaths per week, by sex (M/F/All) and age group... however there is a few issues in the data and if we want to look at PMR/SMR we will have to fix them
glimpse(weekly_deaths_age_2024)

#Step 1: Remove duplicate columns ('Time', 'week.number', 'Sex', 'AgeGroups','RegistrationOrOccurence')
weekly_deaths_filtered <- weekly_deaths_age_2024 %>% 
  select(-Time, - week.number, -Sex, -AgeGroups, -RegistrationOrOccurrence) %>%  #Step 2: Rename columns for more consistency and clarity
rename(total_deaths = v4_0, year = calendar.years, administrative_geography = administrative.geography, week = Week, age_group = age.groups, registration_or_occurence = registration.or.occurrence) 

#Step 3: Fix errors in age_group data...   
#weekly_deaths_filtered %>% count(age_group) #shows columns 01-04, 05-09 and 10-14 have defaulted to date values in excel. 
weekly_deaths_filtered <- weekly_deaths_filtered %>% 
  mutate(age_group = recode(age_group,
                             "01-Apr" = "01-04",
                             "05-Sep" = "05-09",
                             "Oct-14" = "10-14",
                             .default = as.character(age_group)))  # Keep other values unchanged
  

weekly_deaths_filtered %>% count(age_group) #shows columns 01-04, 05-09 and 10-14 have defaulted to date values in excel. 
 
glimpse(weekly_deaths_filtered)

#Step 4: Make line char for deaths per age group

#y = total_deaths
#x = year

# Filter out "all-ages" age group
weekly_deaths_filtered <- weekly_deaths_filtered %>%
  filter(age_group != "all-ages")

#Are death rates significantly higher between males and females? 

death_binom <- weekly_deaths_filtered %>% select (sex, total_deaths) %>% filter(sex != "all")
binom_test <- binom.test(sum(death_binom$total_deaths[death_binom$sex == "male"]), 
                         sum(death_binom$total_deaths), 
                         p = 0.5, alternative = "two.sided")


#518,631 male deaths; 515,372 female deaths, 1,034,003 total deaths
#50.16% male deaths; 49.84% female deaths, 0.16% difference in death rates
#sample size makes significance (p =) in binomial testing... no context to draw conclusions
#measuring death rates is more relevant by age, as men are on average already dead by 80 

weekly_deaths_age_filtered <- weekly_deaths_filtered %>%
  filter(age_group != c("80-84", "85-89", "90+"))

death_binom_age_filtered <- weekly_deaths_age_filtered %>% select (sex, total_deaths) %>% filter(sex != "all")

binom_test_age <- binom.test(sum(death_binom_age_filtered$total_deaths[death_binom_age_filtered$sex == "male"]), 
                         sum(death_binom_age_filtered$total_deaths), 
                         p = 0.5, alternative = "two.sided")
#p = 2.2e-16 this time
#439,690 male deaths; #414,574 female deaths, 854,264 total deaths 
#51.5% male deaths; 48.5% male deaths - a 1.5% death difference pre-80 is more relevant  

# Calculate average deaths per age group and sex using dplyr
average_deaths <- weekly_deaths_filtered %>%
  group_by(age_group, sex) %>%
  summarise(avg_deaths = mean(total_deaths))

# Plot average deaths per age group and sex using ggplot
ggplot(average_deaths, aes(x = age_group, y = avg_deaths, color = sex, group = sex)) +
  geom_point() +  # Add points for each data point
  geom_line() +   # Connect points with lines
  labs(x = "Age Group", y = "Average Deaths", color = "Sex", title = "Average male death count is 1.5% higher between ages 0-80\
Bionomial test: p = 2.2e-16") +  # Labels for axes and legend
  theme_bw()  # Optional: Apply a minimal theme
ggsave(paste0(DIR_FIG_OUT,"Average_deaths_per_age_group.png"), width = 16, height = 9)
 