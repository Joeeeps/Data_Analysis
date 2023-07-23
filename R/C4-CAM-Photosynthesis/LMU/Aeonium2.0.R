#Version 2 of the Aeonium titration script ~ 

library(tidyverse)
setwd("C:/Users/Joe/Desktop/Germany/Thibaud_project")

# load in file, then filter to separate rows which contain average_FA data without calculation (Siwi's data) and rows which contain raw FA data (my collected data). 
Aeonium <- read.csv("TitrationAeonium.csv")

#FA calculations already done in excel, but this is how to do in R
#=((0.00001*VNaOH/1000)*3.5)/(FW/1000)*1000000
#Aeonium$FAinR <- ((0.00001*Aeonium$VNaOH/1000)*3.5)/(Aeonium$FW/1000)*1000000

Aeonium_I_II <- Aeonium %>% filter(is.na(FA))
Aeonium_III <- Aeonium %>% filter(is.na(average_FA))

#The next step is then to calculate average_FA for the Aeonium_III samples
Aeonium_III_average <- aggregate(FA ~ timepoint + species + species_treatment + time_of_day + replicate, Aeonium, mean)
Aeonium_III_average <- rename(Aeonium_III_average, average_FA = FA)

#Then filter the Aeonium_I_II data to contain the same columns as Aeonium_III_average
Aeonium_I_II_average <- Aeonium_I_II %>% select(timepoint, species_treatment, time_of_day, replicate, average_FA, species, replicate)

#Then combine the two dataframes
Aeonium_merge <- bind_rows(Aeonium_III_average, Aeonium_I_II_average)

###now to get the sd and difference calculations

###sd takes into account replicates to measure variation, while difference calculations use the average of all replicates fitting a particular time/treatment

#Difference calculations ----

#this line of code excludes replicates, so averages are done accounting for timepoint, time of day and treatment only
Aeonium_difference <- aggregate(average_FA ~ timepoint + species + species_treatment + time_of_day, Aeonium_merge, mean)

#split code into morning and evening so they can be calculated against each other
Aeonium_morning_difference <- Aeonium_difference %>% filter(time_of_day == "morning")
Aeonium_evening_difference <- Aeonium_difference %>% filter(time_of_day == "evening")

Aeonium_difference_calculations <- Aeonium_morning_difference # I filter then rejoin these to avoid having copies of the results for both morning and evening 

Aeonium_difference_calculations$absolute_difference  <- Aeonium_morning_difference$average_FA - Aeonium_evening_difference$average_FA
Aeonium_difference_calculations$percentage_difference <- ((Aeonium_morning_difference$average_FA - Aeonium_evening_difference$average_FA)/Aeonium_evening_difference$average_FA)*100

Aeonium_differences <- bind_rows(Aeonium_difference_calculations, Aeonium_evening_difference) #rejoining the evening data to the morning is required for average_FA plots

#Standard deviation calculations ----

Aeonium_sd <- aggregate(average_FA ~ timepoint + species + species_treatment + time_of_day + replicate, Aeonium_merge, mean)

#split code into morning and evening so they can be calculated against each other
Aeonium_morning_sd <- Aeonium_sd %>% filter(time_of_day == "morning")
Aeonium_evening_sd <- Aeonium_sd %>% filter(time_of_day == "evening")

Aeonium_sd_calculations <- Aeonium_morning_sd # I filter then rejoin these to avoid having copies of the results for both morning and evening 

Aeonium_sd_calculations$absolute_difference  <- Aeonium_morning_sd$average_FA - Aeonium_evening_sd$average_FA
Aeonium_sd_calculations$percentage_difference <- ((Aeonium_morning_sd$average_FA - Aeonium_evening_sd$average_FA)/Aeonium_evening_sd$average_FA)*100

Aeonium_sd_calculations <- bind_rows(Aeonium_sd_calculations, Aeonium_evening_sd)

Aeonium_absolute_sd <- aggregate(absolute_difference ~ species_treatment + timepoint, Aeonium_sd_calculations, sd)
Aeonium_absolute_sd <- rename(Aeonium_absolute_sd, absolute_sd = absolute_difference)
Aeonium_absolute_sd

Aeonium_percentage_sd <- aggregate(percentage_difference ~ species_treatment + timepoint, Aeonium_sd_calculations, sd)
Aeonium_percentage_sd <- rename(Aeonium_percentage_sd, percentage_sd = percentage_difference)
Aeonium_percentage_sd

Aeonium_sd <- merge(Aeonium_absolute_sd, Aeonium_percentage_sd)
Aeonium_sd$time_of_day = "morning" #I do this so I can merge the sd values into only the morning column

#Now to combine the sd and percentage difference calculations
Aeonium_graphs <- merge(Aeonium_sd, Aeonium_differences)

#but the evening calculations are still missing, so this needs to be rectified
Aeonium_graphs <- bind_rows(Aeonium_evening_difference, Aeonium_graphs)

#Filter by species, enabling species-specific plots to be made
Aeonium_cana <- Aeonium_graphs %>% filter (species == "A. canariense ssp. canariense")
Aeonium_chr <- Aeonium_graphs %>% filter (species == "A. canariense ssp. christii")
Aeonium_cun <- Aeonium_graphs %>% filter (species == "A. cuneatum")
Aeonium_dav <- Aeonium_graphs %>% filter (species == "A. davidbramwellii")
Aeonium_gor <- Aeonium_graphs %>% filter (species == "A. gorgoneum")
Aeonium_leu <- Aeonium_graphs %>% filter (species == "A. leucoblepharum")
Aeonium_per <- Aeonium_graphs %>% filter (species == "A. percaneum")
Aeonium_stu <- Aeonium_graphs %>% filter (species == "A. stuessyi")
Aeonium_und <- Aeonium_graphs %>% filter (species == "A. undulatum")

#Species plots ----

#Aeonium canariense ssp. canariense ----
ggplot(data = Aeonium_cana, aes (x = timepoint, fill = time_of_day)) + #choose the species, set time to X axis and fill as time of day, to distinguish samples by evening or morning
  geom_col(aes(y = average_FA), position = "dodge") + #y aesthetic to plot average FA on the Y axis, position dodge to prevent stacking bars
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) + #error bar to get the minimum and maximum range of the standard deviation
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) + #same as above, but for percentage
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) + #this plots points, in orange, associated with the absolute difference
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) + #this plots lines, in orange, joining the absolute difference of each timepoint (note I changed timepoints from I, II and II to 1, 2, 3 in excel, formatted as 'text' within excel itself)
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+ #same as above, but for percentage
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+ #same as above, but for percentage
  theme_bw()+ #this changes the 'theme' of the plot, for visability purposes
  facet_wrap('species_treatment') #this function makes multiple plots, separated by the treatment applied 
ggsave("graphs/Aeonium_cana.png", units = c("px")) #this saves it within the working directory in the 'graphs' folder, giving it the name Aeonium_cana.png, the units = c pixel don't matter for this but adding width =, height = between 'units' and the file name allows for manual adjustment of plot sizes 

#Aeonium canariense ssp. christii ----
ggplot(data = Aeonium_chr, aes (x = timepoint, fill = time_of_day)) +
  geom_col(aes(y = average_FA), position = "dodge") +
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) +
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) +
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+
  scale_x_continuous(breaks = c(1:3, 1)) +
  theme_bw()+
  facet_wrap('species_treatment')
ggsave("graphs/Aeonium_chr.png",  units = c("px"))


#Aeonium cuneatum ----
ggplot(data = Aeonium_cun, aes (x = timepoint, fill = time_of_day)) +
  geom_col(aes(y = average_FA), position = "dodge") +
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) +
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) +
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+
  scale_x_continuous(breaks = c(1:3, 1)) +
  theme_bw()+
  facet_wrap('species_treatment')
ggsave("graphs/Aeonium_cun.png",  units = c("px"))

#Aeonium davidbramwellii ----
ggplot(data = Aeonium_dav, aes (x = timepoint, fill = time_of_day)) +
  geom_col(aes(y = average_FA), position = "dodge") +
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) +
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) +
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme_bw()+
  facet_wrap('species_treatment')
ggsave("graphs/Aeonium_dav.png", units = c("px"))

#Aeonium gorgoneum ----
ggplot(data = Aeonium_gor, aes (x = timepoint, fill = time_of_day)) +
  geom_col(aes(y = average_FA), position = "dodge") +
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) +
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) +
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme_bw()+
  facet_wrap('species_treatment')
ggsave("graphs/Aeonium_gor.png",  units = c("px"))

#Aeonium leucoblepharum ----
ggplot(data = Aeonium_leu, aes (x = timepoint, fill = time_of_day)) +
  geom_col(aes(y = average_FA), position = "dodge") +
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) +
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) +
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme_bw()+
  facet_wrap('species_treatment')
ggsave("graphs/Aeonium_leu.png", units = c("px"))

#Aeonium percaneum ----
ggplot(data = Aeonium_per, aes (x = timepoint, fill = time_of_day)) +
  geom_col(aes(y = average_FA), position = "dodge") +
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) +
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) +
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme_bw()+
  facet_wrap('species_treatment') 
ggsave("graphs/Aeonium_per.png", units = c("px"))

#Aeonium stuessyi ----
ggplot(data = Aeonium_stu, aes (x = timepoint, fill = time_of_day)) +
  geom_col(aes(y = average_FA), position = "dodge") +
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) +
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) +
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+
  scale_x_continuous(breaks = c(1:3, 3)) +
  theme_bw()+
  facet_wrap('species_treatment')
ggsave("graphs/Aeonium_stu.png", units = c("px"))

####Aeonium undulatum ----
ggplot(data = Aeonium_und, aes (x = timepoint, fill = time_of_day)) +
  geom_col(aes(y = average_FA), position = "dodge") +
  geom_errorbar(aes(ymin=absolute_difference-absolute_sd, ymax=absolute_difference+absolute_sd), colour = "orange", size = 0.5) +
  geom_errorbar(aes(ymin=percentage_difference-percentage_sd, ymax=percentage_difference+percentage_sd), colour = "black", size = 0.5) +
  geom_point(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 2) +
  geom_line(aes (y = absolute_difference, col = "Absolute difference"), colour = "orange", size = 1) +
  geom_point(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 2)+
  geom_line(aes (y= percentage_difference, col = "Percent increase"), colour = "black", size = 1)+
  scale_x_continuous(breaks = c(1, 2, 3)) +
  theme_bw()+
  facet_wrap('species_treatment') 
ggsave("graphs/Aeonium_und.png",  units = c("px"))




