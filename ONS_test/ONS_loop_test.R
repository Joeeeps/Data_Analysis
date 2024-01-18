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
library(Biostrings)

#### set working directory to script/project location ####
setwd <- here::here()
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
#### Quality Scores ####

# I want to get the quality score counts of all the samples, and colour code as appropriate
fastq_files <- list.files(DIR_DAT, pattern = "\\.fastq$", recursive = TRUE, full.names = TRUE)


# Loop over each FASTQ file
for (i in seq_along(fastq_files)) {
  fastq_loop <- readFastq(fastq_files[i]) #read files in 
  quality_loop <- quality(fastq_loop)  #gather quality score
  count_loop <- alphabetFrequency(quality_loop) #check quality score count (per position)
  histogram_loop <- data.frame(count_loop)  #dataframe in preparation for histogram
  histogram_loop <- rownames_to_column(histogram_loop, var = "Sequence_Position") #move rownames to column names
  histogram_loop <- histogram_loop %>% select(-X. ) #remove extra row not linked to QS
  colnames(histogram_loop)[-1] <- paste0("QS", seq_along(histogram_loop)) #rename score values  
 
  tidy_loop <- histogram_loop %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
  tidy_loop = tidy_loop %>% filter(Count != 0) #remove zero count data, note this also removes QS1 if no hits
  tidy_loop$QS <- factor(tidy_loop$QS, levels = unique(tidy_loop$QS)) #adjust levels
  qs_ranges_loop <- cut(as.numeric(tidy_loop$QS), breaks = c(0, 12, 22, Inf), labels = c("1-13", "14-23", "24+")) #define ranges for histogram
  average_loop <- mean(as.numeric(tidy_loop$QS), na.rm = TRUE)

  percentage_count <- tidy_loop %>%
    mutate(QS = as.numeric(QS),  # Convert 'QS' to numeric
           QS_range = case_when(
             QS >= 1 & QS <= 12 ~ "QS 1-13", #this corresponds to QS2-> 13 as QS2 becomes '1' numerical
             QS >= 13 & QS <= 22 ~ "QS 14-23", 
             QS >= 23 ~ "QS 24+" 
           )) %>%
    group_by(QS_range) %>%
    
    summarize(Percentage = sum(Count) / sum(tidy_loop$Count) * 100)
  
  # Create facet_wrap-style table
  ggplot(tidy_loop, aes(x = QS, y = Count, fill = qs_ranges_loop)) +
    geom_histogram(stat = "identity", position = "stack") +
    labs(title = paste(basename(fastq_files[i]), "\nAverage quality score:", round(average_loop, 3))) +
    scale_fill_manual(values = c("1-13" = "red", "14-23" = "#DF8D46", "24+" = "green"),
    name = "Quality scores", labels = c("1-13" = paste0("1-13 (", round(percentage_count$Percentage[1], 1), "%)"), 
                                        "14-23" = paste0("14-23 (", round(percentage_count$Percentage[2], 1), "%)"), 
                                         "24+" = paste0("24+ (", round(percentage_count$Percentage[3], 1), "%)")))
  
  ggsave(paste0(DIR_FIG, "/qs_",  basename(fastq_files[i]), ".png"), width = 1920, height = 1080, units = c("px"))
}
 