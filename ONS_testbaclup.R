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

fastq_files <- list.files(DIR_DAT, pattern = "\\.fastq$", recursive = TRUE, full.names = TRUE)
fastq_C1B1 <- readFastq(fastq_files[1]) 

# Extract files into a list
sequences <- sread(fastq_C1B1)
quality_scores <- quality(fastq_C1B1)
sequence_ids <- id(fastq_C1B1)
score <- alphabetScore(fastq_C1B1)
average_score <- score / width(fastq_C1B1)

alphabetFrequency(fastq_C1B1)


encoding(quality_scores)
encoding(quality_scores, ph)

test <- PhredQuality(quality_scores)

# Assuming fastq_data_list is the list containing your data
write.csv(data.frame(
  sample = 
    sequence = fastq_data_list$sequences,
  sequence_ids = fastq_data_list$sequence_ids,
  scores = fastq_data_list$scores,
  average_scores = fastq_data_list$average_scores
), file = "output.csv", row.names = FALSE)


# Extract read sequences
read_sequences <- quality(fastq_C1B1)

# Calculate alphabet frequency
alphabet_freq <- alphabetFrequency(read_sequences)
histotest <- data.frame(alphabet_freq)
range <- c(0:(length(histotest)-1))
colnames(histotest) <- paste0("QS", range) 
glimpse(histotest)
histotest <- rownames_to_column(histotest, var = "Sequence_Position")

# Convert to tidy format
histo_tidy <- histotest %>%
  gather(key = "QS", value = "Count", -Sequence_Position)

histo_tidy = histo_tidy %>% filter(Count != 0)
glimpse(histo_tidy)

# Create a factor with the desired order
histo_tidy$QS <- factor(histo_tidy$QS, levels = unique(histo_tidy$QS))

# Define quality score ranges
quality_score_ranges <- cut(as.numeric(histo_tidy$QS), breaks = c(0, 10, 20, Inf), labels = c("1-10", "11-20", "21+"))

# Histogram with custom fill colors
ggplot(histo_tidy, aes(x = QS, y = Count, fill = quality_score_ranges)) +
  geom_histogram(stat = "identity", bins = 20) +
  labs(title = "Quality Score count",
       x = "Quality Score",
       y = "Count") +
  scale_fill_manual(values = c("1-10" = "red", "11-20" = "#DF8D46", "21+" = "green")) +
  guides(fill = FALSE)

ggsave("QSC1B1.png")

