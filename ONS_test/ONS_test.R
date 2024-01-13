#### ONS test script ####
#This script aims to check which of two different experimental conditions is most suitable for future experiments with the provided amplicon sequencing data.

#Steps required
#1) Appropriately organise data into an easy to read and easy to analyse format 
#2) Check average quality scores across the samples
#3) Check the test accuracy of sample data to a set of known true positives
#4) Visualise data and draw conclusions

#### read libraries ####
library(tidyverse)
library(here) #here package is a nice way to organise projects
library(ShortRead) #for reading FASTQ data (for larger samples I've used Bash scripts with Python)

#### set working directory to script/project location ####
here::here() 

#### define and create directories ####

script_folders <- c(
DIR_PROJECT <- paste0(here(),"/"),
DIR_DAT <- paste0(DIR_PROJECT, "00_Assignment"), #replace with raw_data for most scenarios
DIR_ANA <- paste0(DIR_PROJECT, "00_Analysis/"), #replace 00 with 01 etc. for different scripts associated with same project 
DIR_TAB <- paste0(DIR_ANA, "00_Tables"),
DIR_FIG <- paste0(DIR_ANA, "00_Figures")) 


for (folder in script_folders) {
  if (!dir.exists(folder)) dir.create(folder) 
  }

#### Start analysis ####
list.files(DIR_DAT)




