#Frequency of Phone use by Gender while driving, original data collected 2019, updated R script August 8th 2023 

#### read libraries ####
library(ggplot2)
library(dplyr)

#### SET WD to local location #### 
setwd <- "/"

#### Read in the files ####
phoneuse <- read.csv("data/DriverPhoneUse.csv")

#### Examine our data and create a df table ####
phoneusetable <- as.data.frame(table(phoneuse))
phoneusetable
 
#### Getting percentage phone use ####
phoneusetable <- phoneusetable %>%
  group_by(Gender) %>%
  mutate(
    Percentage = c(
      sum(Freq[Phone == "No"]) / sum(Freq[Phone %in% c("Yes", "No")]),
      sum(Freq[Phone == "Yes"]) / sum(Freq[Phone %in% c("Yes", "No")])
    )
  ) %>% 
  ungroup()

#### Plotting our data ####
ggplot() + geom_bar(aes(y = Percentage, x = Gender, fill = Phone), data = phoneusetable,
                    stat="identity")+
  scale_y_continuous(labels=scales::percent) 
  ggsave("figures/driver_percents.png", width = 934, height = 971, units = c("px"))

#### Chi-2 test #### Chosen as we are looking for significant differences between two categorical variables
chisq.test(table(phoneuse),  correct = FALSE) #X-squared = 2.8539, df = 1, p-value = 0.09115