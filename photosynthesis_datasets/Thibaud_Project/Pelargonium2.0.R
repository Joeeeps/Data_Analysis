#Pelargonium titration script 

library(tidyverse)
setwd("~/Desktop/Coding/R/Thibaud_Project")

# load in Pelargonium dataset. 
Pelargonium <- read.csv("TitrationPelargonium.csv")

#calculate FA 
Pelargonium$FA <- ((0.00001*Pelargonium$VNaOH/1000)*3.5)/(Pelargonium$FW/1000)*1000000

#calculate average_FA
Pelargonium_average_FA <- aggregate(FA ~ timepoint + species + species_treatment + time_of_day + probe, Pelargonium, mean)
Pelargonium_average_FA <- rename(Pelargonium_average_FA, average_FA = FA)

Pelargonium_average_FA_evening <- Pelargonium_average_FA %>% filter(time_of_day == "evening")
Pelargonium_average_FA_evening <- rename(Pelargonium_average_FA_evening, average_evening_FA = average_FA)
Pelargonium_average_FA_evening <- Pelargonium_average_FA_evening %>% select(!time_of_day)

Pelargonium_average_FA_morning <- Pelargonium_average_FA %>% filter(time_of_day == "morning")
Pelargonium_average_FA_morning <- rename(Pelargonium_average_FA_morning, average_morning_FA = average_FA)
Pelargonium_average_FA_morning <- Pelargonium_average_FA_morning %>% select(!time_of_day)

Pelargonium_calculations <- merge(Pelargonium_average_FA_evening, Pelargonium_average_FA_morning)
Pelargonium_calculations$absolute_difference <- Pelargonium_calculations$average_morning_FA - Pelargonium_calculations$average_evening_FA
Pelargonium_calculations$percentage_difference  <- ((Pelargonium_calculations$average_morning_FA - Pelargonium_calculations$average_evening)/Pelargonium_calculations$average_evening_FA)*100
#difference and sd calulations

#filter by species then plot graph 
Pelargonium_tet_leaf_col <- Pelargonium_average_FA %>% filter (species == "Pelargonium tetragonum (leaf)") #change species name as required
Pelargonium_tet_leaf_point <- Pelargonium_calculations %>% filter (species == "Pelargonium tetragonum (leaf)") #change species name as required

#tet leaf 
ggplot()+
  geom_col(data = Pelargonium_tet_leaf_col, aes(x = timepoint, y = average_FA, fill = time_of_day), position = 'dodge') +
  geom_point(data = Pelargonium_tet_leaf_point, aes ( y = absolute_difference, x = timepoint, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(data = Pelargonium_tet_leaf_point, aes ( y = absolute_difference, x = timepoint, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(data = Pelargonium_tet_leaf_point, aes ( y = percentage_difference, x = timepoint, col = "Percentage increase"), colour = "black", size = 2) +
  geom_line(data = Pelargonium_tet_leaf_point, aes ( y = percentage_difference, x = timepoint, col = "Percentage increase"), colour = "black", size = 1) +
  facet_wrap('species_treatment')
ggsave("graphs/Pelargonium/Pelargonium_Tetragonum_leaf.png",  units = c("px"))
#filter by species then plot graph 
Pelargonium_tet_stem_col <- Pelargonium_average_FA %>% filter (species == "Pelargonium tetragonum (stem)") #change species name as required
Pelargonium_tet_stem_point <- Pelargonium_calculations %>% filter (species == "Pelargonium tetragonum (stem)") #change species name as required

#tet stem 
ggplot()+
  geom_col(data = Pelargonium_tet_stem_col, aes(x = timepoint, y = average_FA, fill = time_of_day), position = 'dodge') +
  geom_point(data = Pelargonium_tet_stem_point, aes ( y = absolute_difference, x = timepoint, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(data = Pelargonium_tet_stem_point, aes ( y = absolute_difference, x = timepoint, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(data = Pelargonium_tet_stem_point, aes ( y = percentage_difference, x = timepoint, col = "Percentage increase"), colour = "black", size = 2) +
  geom_line(data = Pelargonium_tet_stem_point, aes ( y = percentage_difference, x = timepoint, col = "Percentage increase"), colour = "black", size = 1) +
  facet_wrap('species_treatment')
ggsave("graphs/Pelargonium/Pelargonium_Tetragonum_stem.png",  units = c("px"))

