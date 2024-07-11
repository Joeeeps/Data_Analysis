#### read libraries ####
library(here)
library(tidyverse)
#setwd to relevant area first... hopefully source file location

#### define directories ####
DIR_PROJECT <- paste0(getwd(),"/") #can also do paste0(here()) as long as the root folder of here is in the relevant position
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")

DIR_FIG_OUT  <- paste0(DIR_FIG,"001/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"001/")
#presumably raw data will be inserted prior to script
  
if(!dir.exists(DIR_RAW)) dir.create(DIR_RAW)
if(!dir.exists(DIR_DAT)) dir.create(DIR_DAT)
if(!dir.exists(DIR_FIG)) dir.create(DIR_FIG)
if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

#### Start analysis ####

#Example datasets from https://iiif.wellcomecollection.org/pdf/b32237686 for SMR and PMR
#For the PMR dataset I made up some values for the observed occupation A deaths, just for
#testing purposes and making examples
SMR_data <- read.csv("rawdata/001/SMR_example_data.csv")
PMR_data <-  read.csv("rawdata/001/PMR_example_data.csv")

#1) SMR data - stat test and visuaslise 
glimpse(SMR_data)

# Conduct ANOVA
anova_result_SMR <- aov(Deaths_per_thousand ~ Occupation, weights = Deaths, data = SMR_data)

# Summarise ANOVA results
summary(anova_result_SMR) 

#No weights   Df Sum Sq Mean Sq F value Pr(>F)
#Occupation   1    4.5    4.45     0.1  0.758
#Residuals   10  445.0   44.50      


#Make table
ggplot(SMR_data, aes(x = Age_group, y = Deaths_per_thousand, fill = Occupation)) +
  geom_col(position = "dodge") +  # Use geom_col() for dodged bar chart
  labs(x = "Age Group", y = "Deaths per Thousand", title = "Males in 'Occupation A' die at an average rate\
ANOVA: P = 0.76\
ANOVA: P = 0.98 (weighted for no. Deaths)") +
  scale_fill_manual(values = c("All" = "gray", "Occupation_A" = "black")) +  # Specify colors 
  theme_bw() 
ggsave(paste0(DIR_FIG_OUT,"SMR_death_rates.png"), width = 16, height = 9)

#2) PMR data - 
#Step 1: I want to look at the PMR... calculation is Observed_deaths_from_X / Deaths = PMR 
PMR_data_modified <- PMR_data %>% 
  mutate(PMR = (Observed_deaths_from_X / Expected_deaths_from_X) * 100) #making it in new df

glimpse(PMR_data_modified)


#Step 2: Statistical testing
# Initial ANOVA 
anova_result_PMR <- aov(PMR ~ Occupation, data = PMR_data_modified)
# Summarise ANOVA results
summary(anova_result_PMR)
#            Df Sum Sq Mean Sq F value Pr(>F)  
#Occupation   1 114637  114637   4.701 0.0412 *
#  Residuals   22 536494   24386     

#Clearly there are some age groups that aren't significantly different though 

# Trying ANCOVA under assumption Age_group and Death count impacts PMR 

ancova_result_PMR <-aov(PMR ~ Occupation + Age_group, weights = Deaths,  data = PMR_data_modified)
summary(ancova_result_PMR)  #adding weights  makes it significant 
             #Df Sum Sq Mean Sq  F value Pr(>F)   No Weights
#Occupation   1 114637  114637   4.738 0.0195 .
#Age_group   11 268247   24386   1.008 0.4990  
#Deaths       1  26281   26281   1.086 0.3219  
#Residuals   10 241966   24197

#ANCOVA must account for sample size as data is no longer significant without weightings 

#Step 2: Plot PMR of disease_X for Occupation_A
PMR_Occupation_A <- PMR_data_modified[PMR_data_modified$Occupation == 'Occupation_A', ]

ggplot(PMR_Occupation_A, aes (y = PMR, x = Age_group)) +
  geom_col(position = "dodge", fill = 'darkgreen')+
  geom_hline(yintercept = 100, linetype = "dashed", color = "red", size = 1) +
  geom_text(aes(label = round(Deaths)),
            vjust = -0.7)+
  labs(x = "Ages", y = "Proportional Mortality Rate from\
Disease_X in Ocucpation A", title = "PMR from Disease_X is significantly higher in Occupation_A\
Ancova: (No weights) Occupation Pr(>F) = 0.0546\ 
Ancova (Weighted for Death = Sample size) = 0.0195\
Labels = Deaths from Disease_X\
Red line = 100 PMR (based on overall Disease_X mortality rate)") +
  theme_bw() 
ggsave(paste0(DIR_FIG_OUT,"PMR_disease_X.png"), width = 16, height = 9)


#Testing linear regression modelling
# Example: Using 'Deaths' as weights in a linear regression model... so it takes into account sample size = deaths
model <- lm(PMR ~ Age_group + Occupation, data = PMR_data_modified, weights = Deaths)

# Summary of the model shows significant difference due to sample size
summary(model)

#Regression is almost certainly wrong here as based on assumptions 
plot(model, which = 1, add.smooth = FALSE)
plot(model, which = 2, add.smooth = FALSE)
plot(model, which = 3, add.smooth = FALSE)

