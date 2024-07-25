#### read libraries ####
library(here)
library(tidyverse)
#setwd to relevant area first... hopefully source file location

#### define directories ####
DIR_PROJECT <- paste0(getwd(),"/") #can also do paste0(here()) as long as the root folder of here is in the relevant position
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")

DIR_FIG_OUT  <- paste0(DIR_FIG,"006/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"006/")
#presumably raw data will be inserted prior to script
  
if(!dir.exists(DIR_RAW)) dir.create(DIR_RAW)
if(!dir.exists(DIR_DAT)) dir.create(DIR_DAT)
if(!dir.exists(DIR_FIG)) dir.create(DIR_FIG)
if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

#### Start analysis #### 

#data from https://www.hse.gov.uk/statistics/gender/tables.html
#MESOOCCUPATION 01-06, Mesothelioma mortality in Great Britain by occupation 2011-2022 and 2001-2010 (.xlsx)

#data has various subsheets, saved four different sheets to merge later

meso_2001_10_female <- read.csv("rawdata/006/2001-10_SOC2000_female.csv", skip = 4)  %>% select(1:11) #only first 11 columns are relevant (description below), so we select columns 1:11 
meso_2001_10_male <- read.csv("rawdata/006/2001-10_SOC2000_male.csv", skip = 4) %>% select(1:11)
meso_2011_22_female <- read.csv("rawdata/006/2011-22_SOC2010_female.csv", skip = 4) %>% select(1:11)
meso_2011_22_male <- read.csv("rawdata/006/2011-22_SOC2010_male.csv", skip = 4) %>% select(1:11)


#Each sheet has the following columns

#Groupings - Good for looking at data for different occupation categories
#1 Major group - General job group
#2 Sub-major group - More specific role group
#3 Minor group - Even more specific role group
#4 Unit group - Specific job role (seems to be unique ID for specific occupation) 
#5.1 (5.1 as in the column begins one lower than other coluns) Occupation - Occupation name 
#6 - Deaths
#6.1 - All
#7.1 - Meso deaths
#8.1 - Expected Meso deaths
#9.1 - Proportional mortality rate
#10 - 95% (confidence interval)
#10.1 - Lower (CI)
#11.2 - Upper (CI) 
#12 - Rank by N-digit SOC2000 

#The 2011-22 datasets also have birth cohorts, but to keep it simple I'll ignore them
#Also ignoring the Rank by N-digit SOC2000 as I'm not sure what it is 


#Ideally we'll sort data like this
#1 Remove top rows without data
#2 Major group 
#3 Sub-major
#4 Minor group
#5 Unit group
#6 Occupation
#7 Cause of death (All, meso)
#8 Deaths (count)
#9 Expected deaths 
#10 Proportional mortality rate
#11 Confidence interval (lower)
#12 Confidence interval (upper)


#Looking at head, we need to sort out things nicely

#Each group is four integers, correlating to the four different group levels (i.e. group '1111' has the value '1' for major group, sub-major etc.)

#Lots of missing data... to sort into tidy...  
#possible to make a list of meso_2001_10_female, meso_2001_10_male etc. and then do loop to do all this at once?

data_list <- c("meso_2001_10_female", "meso_2001_10_male", "meso_2011_22_female", "meso_2011_22_male" )

#maybe as a function first

process_data <- function(df_name) {
  df <- get(df_name)  # Fetch the data frame using its name
  df <- df %>%
    rename(
      occupation = X,
      All_deaths = Deaths,
      Mesothelioma_deaths = X.1,
      Mesothelioma_expected = X.2,
      PMR = X.3,
      Lower_CI = `X95..Confidence..Interval`,
      Upper_CI = X.4,
      Unit.Group = Unit...Group
    ) %>%
    slice(-1) %>% 
    filter(All_deaths != "") %>%  # Remove rows where All_deaths is empty
    rename(Occupation = occupation) %>%
    mutate(Major.Group = ifelse(Major.Group == "", NA, Major.Group)) %>%
    mutate(Source = df_name)  # Add a new column with the data frame name as the source
  return(df)
}

meso_all <- lapply(data_list, process_data)  %>%  bind_rows() #data_list gives us names of all our variables, then we run the filtering for each and place into a list and lastly we 'bind_rows' to bind them all together

#make sure all group levels are characters not integers 
meso_all$Major.Group <- as.character(meso_all$Major.Group) 
meso_all$Sub.Major.Group <- as.character(meso_all$Sub.Major.Group) 
meso_all$Minor.Group <- as.character(meso_all$Minor.Group) 
meso_all$Unit.Group <- as.character(meso_all$Unit.Group) 
meso_all$PMR <- as.integer(meso_all$PMR) #and make sure PMR is an integer
glimpse(meso_all)

#now its possible to plot meso deaths by the different group levels 
#is it possible to make a new column called "group_level" where each value is
#if Major.Group != NA, group_level = Major_Group
#if Sub.Major.Group != NA, group_level = Sub_major_group
#if Minor.group != NA, group_level = Minor_group
#if Unit.group != NA, group_level = Unit_group

#I want to do this so I can just compare levels individually 
 
meso_all <- meso_all %>%
  mutate(group_level = case_when(
    !is.na(Major.Group) ~ "Major_Group",
    !is.na(Sub.Major.Group) ~ "Sub_Major_Group",
    !is.na(Minor.Group) ~ "Minor_Group",
    !is.na(Unit.Group) ~ "Unit_Group",
    TRUE ~ NA_character_  # If none of the above conditions match, set to NA
  ))

#But what if I want to include full information about the group levels 
#Extract data from lower levels to fill the NA of higher levels 

meso_all <- meso_all %>%
  mutate(
    Major.Group = case_when(
      !is.na(substr(Unit.Group, 1, 1)) ~ substr(Unit.Group, 1, 1),
      !is.na(substr(Minor.Group, 1, 1)) ~ substr(Minor.Group, 1, 1),
      !is.na(substr(Sub.Major.Group, 1, 1)) ~ substr(Sub.Major.Group, 1, 1),
      TRUE ~ Major.Group  # Keep original Major.Group if none of the conditions are met
    ),
    Sub.Major.Group = case_when(
      !is.na(substr(Unit.Group, 1, 2)) ~ substr(Unit.Group, 1, 2),
      !is.na(substr(Minor.Group, 1, 2)) ~ substr(Minor.Group, 1, 2),
      TRUE ~ Sub.Major.Group  # Keep original Sub.Major.Group if none of the conditions are met
    ),
    Minor.Group = ifelse(
      !is.na(substr(Unit.Group, 1, 3)),
      substr(Unit.Group, 1, 3),
      Minor.Group  # Keep original Minor.Group if the condition is not met
    )
  )



#Theoretically we can do a multiple statement, where if major.group === 1 AND submajor = NA, Major Group == Occupation , then do same with minor group == 11 and minor.group == NA then Sub.Major.Group == Ocupation
# Create the boxplot
# Calculate the average PMR for each Major Group
meso_major <- meso_all %>%
  filter(group_level == "Major_Group")

 
# Create the bar plot
ggplot(data = meso_major, aes(x = Major.Group, y = PMR, fill = Source)) +
  geom_col(position = "Dodge") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "green", size = 1) +
  labs(x = "Major Group", y = "Average PMR", title = "Average PMR by Major Group") +
  theme_minimal()
ggsave(paste0(DIR_FIG_OUT,"Average_PMR_by_job.png"), width = 16, height = 9)
