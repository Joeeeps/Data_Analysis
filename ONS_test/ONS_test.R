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
# I want to get the quality score counts of all the samples, and colour code as appropriate
fastq_files <- list.files(DIR_DAT, pattern = "\\.fastq$", recursive = TRUE, full.names = TRUE)

#C = Condition, B = Barcode 

#### C1B1 ####
fastq_C1B1 <- readFastq(fastq_files[1]) #read files in 
quality_C1B1 <- quality(fastq_C1B1)  #gather quality score
count_C1B1 <- alphabetFrequency(quality_C1B1) #check quality score count (per position)

histogram_C1B1 <- data.frame(count_C1B1) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C1B1)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C1B1) <- paste0("QS", range) #rename score values  
histogram_C1B1 <- rownames_to_column(histogram_C1B1, var = "Sequence_Position") #move rownames to column names
tidy_C1B1 <- histogram_C1B1 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C1B1 = tidy_C1B1 %>% filter(Count != 0) #remove zero count data 
tidy_C1B1$QS <- factor(tidy_C1B1$QS, levels = unique(tidy_C1B1$QS)) #adjust levels
qs_ranges_C1B1 <- cut(as.numeric(tidy_C1B1$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C1B1, aes(x = QS, y = Count, fill = qs_ranges_C1B1)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 1; Barcode 1",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC1B1.png")

#### C1B2 ####
fastq_C1B2 <- readFastq(fastq_files[2]) #read files in 
quality_C1B2 <- quality(fastq_C1B2)  #gather quality score
count_C1B2 <- alphabetFrequency(quality_C1B2) #check quality score count (per position)

histogram_C1B2 <- data.frame(count_C1B2) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C1B2)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C1B2) <- paste0("QS", range) #rename score values  
histogram_C1B2 <- rownames_to_column(histogram_C1B2, var = "Sequence_Position") #move rownames to column names
tidy_C1B2 <- histogram_C1B2 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C1B2 = tidy_C1B2 %>% filter(Count != 0) #remove zero count data 
tidy_C1B2$QS <- factor(tidy_C1B2$QS, levels = unique(tidy_C1B2$QS)) #adjust levels
qs_ranges_C1B2 <- cut(as.numeric(tidy_C1B2$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C1B2, aes(x = QS, y = Count, fill = qs_ranges_C1B2)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 1; Barcode 2",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC1B2.png")

#### C1B3 ####
fastq_C1B3 <- readFastq(fastq_files[3]) #read files in 
quality_C1B3 <- quality(fastq_C1B3)  #gather quality score
count_C1B3 <- alphabetFrequency(quality_C1B3) #check quality score count (per position)

histogram_C1B3 <- data.frame(count_C1B3) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C1B3)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C1B3) <- paste0("QS", range) #rename score values  
histogram_C1B3 <- rownames_to_column(histogram_C1B3, var = "Sequence_Position") #move rownames to column names
tidy_C1B3 <- histogram_C1B3 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C1B3 = tidy_C1B3 %>% filter(Count != 0) #remove zero count data 
tidy_C1B3$QS <- factor(tidy_C1B3$QS, levels = unique(tidy_C1B3$QS)) #adjust levels
qs_ranges_C1B3 <- cut(as.numeric(tidy_C1B3$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C1B3, aes(x = QS, y = Count, fill = qs_ranges_C1B3)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 1; Barcode 3",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC1B3.png")


#### C1B4 ####
fastq_C1B4 <- readFastq(fastq_files[4]) #read files in 
quality_C1B4 <- quality(fastq_C1B4)  #gather quality score
count_C1B4 <- alphabetFrequency(quality_C1B4) #check quality score count (per position)

histogram_C1B4 <- data.frame(count_C1B4) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C1B4)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C1B4) <- paste0("QS", range) #rename score values  
histogram_C1B4 <- rownames_to_column(histogram_C1B4, var = "Sequence_Position") #move rownames to column names
tidy_C1B4 <- histogram_C1B4 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C1B4 = tidy_C1B4 %>% filter(Count != 0) #remove zero count data 
tidy_C1B4$QS <- factor(tidy_C1B4$QS, levels = unique(tidy_C1B4$QS)) #adjust levels
qs_ranges_C1B4 <- cut(as.numeric(tidy_C1B4$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C1B4, aes(x = QS, y = Count, fill = qs_ranges_C1B4)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 1; Barcode 4",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC1B4.png")


#### C1B5 ####
fastq_C1B5 <- readFastq(fastq_files[5]) #read files in 
quality_C1B5 <- quality(fastq_C1B5)  #gather quality score
count_C1B5 <- alphabetFrequency(quality_C1B5) #check quality score count (per position)

histogram_C1B5 <- data.frame(count_C1B5) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C1B5)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C1B5) <- paste0("QS", range) #rename score values  
histogram_C1B5 <- rownames_to_column(histogram_C1B5, var = "Sequence_Position") #move rownames to column names
tidy_C1B5 <- histogram_C1B5 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C1B5 = tidy_C1B5 %>% filter(Count != 0) #remove zero count data 
tidy_C1B5$QS <- factor(tidy_C1B5$QS, levels = unique(tidy_C1B5$QS)) #adjust levels
qs_ranges_C1B5 <- cut(as.numeric(tidy_C1B5$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C1B5, aes(x = QS, y = Count, fill = qs_ranges_C1B5)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 1; Barcode 5",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC1B5.png")


#### C2B1 ####
fastq_C2B1 <- readFastq(fastq_files[6]) #read files in 
quality_C2B1 <- quality(fastq_C2B1)  #gather quality score
count_C2B1 <- alphabetFrequency(quality_C2B1) #check quality score count (per position)

histogram_C2B1 <- data.frame(count_C2B1) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C2B1)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C2B1) <- paste0("QS", range) #rename score values  
histogram_C2B1 <- rownames_to_column(histogram_C2B1, var = "Sequence_Position") #move rownames to column names
tidy_C2B1 <- histogram_C2B1 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C2B1 = tidy_C2B1 %>% filter(Count != 0) #remove zero count data 
tidy_C2B1$QS <- factor(tidy_C2B1$QS, levels = unique(tidy_C2B1$QS)) #adjust levels
qs_ranges_C2B1 <- cut(as.numeric(tidy_C2B1$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C2B1, aes(x = QS, y = Count, fill = qs_ranges_C2B1)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 2; Barcode 1",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC2B1.png")

#### C2B2 ####
fastq_C2B2 <- readFastq(fastq_files[7]) #read files in 
quality_C2B2 <- quality(fastq_C2B2)  #gather quality score
count_C2B2 <- alphabetFrequency(quality_C2B2) #check quality score count (per position)

histogram_C2B2 <- data.frame(count_C2B2) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C2B2)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C2B2) <- paste0("QS", range) #rename score values  
histogram_C2B2 <- rownames_to_column(histogram_C2B2, var = "Sequence_Position") #move rownames to column names
tidy_C2B2 <- histogram_C2B2 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C2B2 = tidy_C2B2 %>% filter(Count != 0) #remove zero count data 
tidy_C2B2$QS <- factor(tidy_C2B2$QS, levels = unique(tidy_C2B2$QS)) #adjust levels
qs_ranges_C2B2 <- cut(as.numeric(tidy_C2B2$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C2B2, aes(x = QS, y = Count, fill = qs_ranges_C2B2)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 2; Barcode 2",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC2B2.png")

 
#### C2B3 ####
fastq_C2B3 <- readFastq(fastq_files[8]) #read files in 
quality_C2B3 <- quality(fastq_C2B3)  #gather quality score
count_C2B3 <- alphabetFrequency(quality_C2B3) #check quality score count (per position)

histogram_C2B3 <- data.frame(count_C2B3) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C2B3)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C2B3) <- paste0("QS", range) #rename score values  
histogram_C2B3 <- rownames_to_column(histogram_C2B3, var = "Sequence_Position") #move rownames to column names
tidy_C2B3 <- histogram_C2B3 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C2B3 = tidy_C2B3 %>% filter(Count != 0) #remove zero count data 
tidy_C2B3$QS <- factor(tidy_C2B3$QS, levels = unique(tidy_C2B3$QS)) #adjust levels
qs_ranges_C2B3 <- cut(as.numeric(tidy_C2B3$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C2B3, aes(x = QS, y = Count, fill = qs_ranges_C2B3)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 2; Barcode 3",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC2B3.png")

#### C2B4 ####
fastq_C2B4 <- readFastq(fastq_files[9]) #read files in 
quality_C2B4 <- quality(fastq_C2B4)  #gather quality score
count_C2B4 <- alphabetFrequency(quality_C2B4) #check quality score count (per position)

histogram_C2B4 <- data.frame(count_C2B4) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C2B4)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C2B4) <- paste0("QS", range) #rename score values  
histogram_C2B4 <- rownames_to_column(histogram_C2B4, var = "Sequence_Position") #move rownames to column names
tidy_C2B4 <- histogram_C2B4 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C2B4 = tidy_C2B4 %>% filter(Count != 0) #remove zero count data 
tidy_C2B4$QS <- factor(tidy_C2B4$QS, levels = unique(tidy_C2B4$QS)) #adjust levels
qs_ranges_C2B4 <- cut(as.numeric(tidy_C2B4$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C2B4, aes(x = QS, y = Count, fill = qs_ranges_C2B4)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 2; Barcode 4",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC2B4.png")

#### C2B5 ####
fastq_C2B5 <- readFastq(fastq_files[10]) #read files in 
quality_C2B5 <- quality(fastq_C2B5)  #gather quality score
count_C2B5 <- alphabetFrequency(quality_C2B5) #check quality score count (per position)

histogram_C2B5 <- data.frame(count_C2B5) #dataframe in preparation for histogram
range <- c(0:(length(histogram_C2B5)-1)) #get range of quality scores, should be same for all samples
colnames(histogram_C2B5) <- paste0("QS", range) #rename score values  
histogram_C2B5 <- rownames_to_column(histogram_C2B5, var = "Sequence_Position") #move rownames to column names
tidy_C2B5 <- histogram_C2B5 %>% gather(key = "QS", value = "Count", -Sequence_Position) #make data nice and tidy
tidy_C2B5 = tidy_C2B5 %>% filter(Count != 0) #remove zero count data 
tidy_C2B5$QS <- factor(tidy_C2B5$QS, levels = unique(tidy_C2B5$QS)) #adjust levels
qs_ranges_C2B5 <- cut(as.numeric(tidy_C2B5$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+")) #define ranges for histogram

# Histogram with custom fill colors
ggplot(tidy_C2B5, aes(x = QS, y = Count, fill = qs_ranges_C2B5)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count: Condition 2; Barcode 5",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC2B5.png")