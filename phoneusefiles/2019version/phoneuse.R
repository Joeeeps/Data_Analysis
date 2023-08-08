library(ggplot2)
library(dplyr)

phoney <- read.csv("phoney.csv")   
  

ggplot() + geom_bar(aes(y = Percentage, x = Sex, fill = Phone_use), data = phoney,
                    stat="identity")+
 
  scale_y_continuous(labels=scales::percent) +
  
  ggsave("driver percents.png")

driver <- read.csv("DriverPhoneUse.csv")
driver <- as_tibble(driver)

f <- filter(driver, Gender == 'F')
f <- as_tibble(f)
f


m <- filter(driver, Gender == 'M')
m <- as_tibble(m)
m

phoneyes <- filter(driver, Phone_Use =='Yes')
sum(phoneyes$Phone_Use =='Yes')
#filtering males and female from each other

fyes <- filter(f, Phone_Use == 'Yes')
fno <- filter(f, Phone_Use == 'No')

myes <- filter(m, Phone_Use == 'Yes')
mno <- filter(m, Phone_Use == 'No')
mnosum <- sum(mno$Gender == 'M')
fnosum <- sum(fno$Gender == 'F')

MaleO <- sum(myes$Gender == 'M')
FemaleO <- sum(fyes$Gender == 'F')

MaleE <- sum(phoneyes$Gender == "M" | phoneyes$Gender =='F') * 0.5
FemaleE <- sum(phoneyes$Gender == "F") * 0.5

chisq.test(df$MaleO, df$MaleE, correct=FALSE)
#chi2 = SUM OF: (O-E)2 divided by E 
 
