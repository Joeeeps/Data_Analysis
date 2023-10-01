#Energy and Smart Meter data from https://www.ons.gov.uk/economy/environmentalaccounts/articles/climatechangeinsightsuk/august2023
#I merged these two datasets to see whether smart meter usage correlates with a reduction in household energy
 
#### libraries ####
library(tidyverse)

#### set working directory ####
setwd("~/OneDrive/Desktop/R/Energy and Smart Meters (Regression)")

##### read files ####
energysmarts <- read.csv("energyandsmartmeters.csv")
energysmarts$Year <- as.character(energysmarts$Year)
glimpse(energysmarts)

#Smart meter data is from 2012-2023, it contains the total number of smart meters and the percentage of households with meters
#Energy data is from 1990-2021, it contains relative percent household energy usage, using 1990 at 100% as a base. I will focus on the household energy column.
#We want to see if energy usage correlates with smart meter usage, 2012-2021 are the crossover dates.

energysmarts2012_2021 <- energysmarts %>% filter(Year >= 2012 & Year <= 2021)
glimpse(energysmarts2012_2021)

#### viewing data ####

ggplot(data = energysmarts2012_2021, aes()) +
  geom_point(aes(x = X..smart, Household ))
       
#Is there a statistically significant relationship between household energy and smart meter installation
#Aka do smart meters make people more energy conscious? 
#Scatter plot kind of follows a trend, lets try fit a model

energy_model <- lm(Household ~ X..smart, data = energysmarts2012_2021)
#suggests each percentage of households with a smart meter leads to relative 0.2437 decrease in energy usage

anova(energy_model)
summary(energy_model)

#low p value suggests there is likely a statistically significant relationship between predictor (smart) and response (household energy)

#now for predictions
pred_data <- data.frame(X..smart = seq(0,100, length.out = 9))
predict(energy_model, pred_data)

pred_data <- mutate(pred_data, Household = predict(energy_model, pred_data))
head(pred_data, 10)
#this predicts the drop in household energy usage based on smart meters

#now we can plot a regression to see how our moel data fits our actual data

ggplot(pred_data, aes(x = X..smart, y = Household)) + 
  geom_line() + geom_point(data = energysmarts2012_2021) + 
  xlab("Smart meter %") + ylab("Household energy") + 
  theme_bw(base_size = 22)
ggsave("energy_x_smartmeter_regression.png")
#There are too few data points to really draw any conclusions, and of course many issues with looking at the data like this:
#i.e. Smart meters don't directly reduce energy, at best they can make people more conscious of energy usage
#i.e. External schemes which aim to reduce energy usage are likely to have a big impact

#Correlation does not mean causation... if we assume pre-2012 always had 0 samrt meters, we can draw out more data...

energysmarts_assumptions <- energysmarts %>% mutate(X..smart = ifelse(is.na(X..smart), 0, X..smart))

#### viewing data ####

ggplot(data = energysmarts_assumptions, aes()) +
  geom_point(aes(x = X..smart, Household ))


energy_ass_model <- lm(Household ~ X..smart, data = energysmarts2012_2021)
#suggests each percentage of households with a smart meter leads to relative 0.2437 decrease in energy usage

anova(energy_ass_model)
summary(energy_ass_model)

#low p value suggests there is likely a statistically significant relationship between predictor (smart) and response (household energy)

#now for predictions
pred_ass_data <- data.frame(X..smart = seq(0,100, length.out = 9))
predict(energy_ass_model, pred_data)

pred_ass_data <- mutate(pred_ass_data, Household = predict(energy_ass_model, pred_ass_data))
head(pred_ass_data, 10)
#this predicts the drop in household energy usage based on smart meters

#now we can plot a regression to see how our model data fits our actual data

ggplot(pred_ass_data, aes(x = X..smart, y = Household)) + 
  geom_line() + geom_point(data = energysmarts_assumptions) + 
  xlab("Smart meter %") + ylab("Household energy") + 
  theme_bw(base_size = 22)
ggsave("energy_x_regression_all_data.png")
#While post 2012 led to consistent energy reductions, we know from the data
#that the 1990 value of household energy (100) is relative, so some years must have increased in energy usage)

#% Households without a smart meter ploted on Household energy by year
ggplot(energysmarts, aes(x = Year, y = Household)) +
  geom_col() +
  geom_hline(yintercept = 100, color = "red", linetype = "dashed", size = 1) +
  geom_point(data = energysmarts, aes(y = 100 - X..smart, color = "Households without Smartmeter (%)")) +
  labs(color = "")

#Overall this suggests smart meters could be helpful, but aren't a good predictor for energy reductions
#As more energy had reduced from 2003-2010 compared to 2011-2021...
#The safest assumption is that smart meters are part of a set of new initiatives and technology to reduce household energy

#If we were to predict the level of energy reduction we need year-on-year to combat global warming however I'm sure the fall in usage won't be enough.


