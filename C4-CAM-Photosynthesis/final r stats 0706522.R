#loadpackages
library(tidyverse)

#read in files

setwd("//studata05/home/BO/Boa18jp/ManW10/Desktop/Masters/071422 work/express/zzzPascal-Antoine express/updated")

#Data preparation for comparing accession data together.  ----

orthogroup <- c(1:179) #Orthogroups already arranged in order, this lets me group each orthogroup. 

#AUS ---- 

#LGT
AUS_LGT <- read.csv("AUS_LGT.csv") 
AUS_LGT <- data.frame(AUS_LGT, orthogroup)
AUS_LGT$orthogroup <- as.character(AUS_LGT$orthogroup)
AUS_LGT$average.root <- as.numeric(AUS_LGT$average.root)
AUS_LGT$average.tip.leaf <- as.numeric(AUS_LGT$average.tip.leaf)
AUS_LGT$sd.root <- as.numeric(AUS_LGT$sd.root)
AUS_LGT$sd.tip.leaf <- as.numeric(AUS_LGT$sd.tip.leaf) 
AUS_LGT$se.root <- as.numeric(AUS_LGT$se.root)
AUS_LGT$se.tip.leaf <- as.numeric(AUS_LGT$se.tip.leaf)
AUS_LGT$average.overall <- as.numeric(AUS_LGT$average.overall)
AUS_LGT$type <- "LGT"
AUS_LGT$accession <- "AUS"

#Native 
AUS_native <- read.csv("AUS_native.csv")
AUS_native <- data.frame(AUS_native, orthogroup)
AUS_native$orthogroup <- as.character(AUS_native$orthogroup)
AUS_native$average.root <- as.numeric(AUS_native$average.root)
AUS_native$average.tip.leaf <- as.numeric(AUS_native$average.tip.leaf)
AUS_native$average.overall <- as.numeric(AUS_native$average.overall)
AUS_native$type <- "Recipient"
AUS_native$donor <- AUS_LGT$donor
AUS_native$sd.root <- as.numeric(AUS_native$sd.root)
AUS_native$sd.tip.leaf <- as.numeric(AUS_native$sd.tip.leaf) 
AUS_native$se.root <- as.numeric(AUS_native$se.root)
AUS_native$se.tip.leaf <- as.numeric(AUS_native$se.tip.leaf)
AUS_native$expression.AUS_root_A.txt <- as.numeric(AUS_native$expression.AUS_root_A.txt)
AUS_native$expression.AUS_root_B.txt <- as.numeric(AUS_native$expression.AUS_root_B.txt)
AUS_native$expression.AUS_root_C.txt <- as.numeric(AUS_native$expression.AUS_root_C.txt)
AUS_native$expression.AUS_tip_leaf_A.txt <- as.numeric(AUS_native$expression.AUS_tip_leaf_A.txt)
AUS_native$expression.AUS_tip_leaf_B.txt <- as.numeric(AUS_native$expression.AUS_tip_leaf_B.txt)
AUS_native$expression.AUS_tip_leaf_C.txt <- as.numeric(AUS_native$expression.AUS_tip_leaf_C.txt)
AUS_native$accession <- "AUS" 

AUS_SET_native <- read.csv("AUS_SET_native.csv")
AUS_SET_native <- data.frame(AUS_SET_native, orthogroup)
AUS_SET_native$orthogroup <- as.character(AUS_SET_native$orthogroup)
AUS_SET_native$average.root <- as.numeric(AUS_SET_native$average.root)
AUS_SET_native$average.tip.leaf <- as.numeric(AUS_SET_native$average.tip.leaf)
AUS_SET_native$average.overall <- as.numeric(AUS_SET_native$average.overall)
AUS_SET_native$type <- "Donor"
AUS_SET_native$donor <- AUS_LGT$donor
AUS_SET_native$sd.root <- as.numeric(AUS_SET_native$sd.root)
AUS_SET_native$sd.tip.leaf <- as.numeric(AUS_SET_native$sd.tip.leaf) 
AUS_SET_native$se.root <- as.numeric(AUS_SET_native$se.root)
AUS_SET_native$se.tip.leaf <- as.numeric(AUS_SET_native$se.tip.leaf)
glimpse(AUS_SET_native)

AUS_THE_native <- read.csv("AUS_THE_native.csv")
AUS_THE_native <- data.frame(AUS_THE_native, orthogroup)
AUS_THE_native$orthogroup <- as.character(AUS_THE_native$orthogroup)
AUS_THE_native$average.root <- as.numeric(AUS_THE_native$average.root)
AUS_THE_native$average.tip.leaf <- as.numeric(AUS_THE_native$average.tip.leaf)
AUS_THE_native$average.overall <- as.numeric(AUS_THE_native$average.overall)
AUS_THE_native$type <- "Donor"
AUS_THE_native$donor <- AUS_LGT$donor
AUS_THE_native$sd.root <- as.numeric(AUS_THE_native$sd.root)
AUS_THE_native$sd.tip.leaf <- as.numeric(AUS_THE_native$sd.tip.leaf) 
AUS_THE_native$se.root <- as.numeric(AUS_THE_native$se.root)
AUS_THE_native$se.tip.leaf <- as.numeric(AUS_THE_native$se.tip.leaf)

#ZAM ----

#LGT
ZAM_LGT <- read.csv("ZAM_LGT.csv") 
ZAM_LGT <- data.frame(ZAM_LGT, orthogroup)
ZAM_LGT$orthogroup <- as.character(ZAM_LGT$orthogroup)
ZAM_LGT$average.root <- as.numeric(ZAM_LGT$average.root)
ZAM_LGT$average.tip.leaf <- as.numeric(ZAM_LGT$average.tip.leaf)
ZAM_LGT$sd.root <- as.numeric(ZAM_LGT$sd.root)
ZAM_LGT$sd.tip.leaf <- as.numeric(ZAM_LGT$sd.tip.leaf) 
ZAM_LGT$se.root <- as.numeric(ZAM_LGT$se.root)
ZAM_LGT$se.tip.leaf <- as.numeric(ZAM_LGT$se.tip.leaf)
ZAM_LGT$average.overall <- as.numeric(ZAM_LGT$average.overall)
ZAM_LGT$type <- "LGT"
ZAM_LGT$accession <- "ZAM"

#Native
ZAM_native <- read.csv("ZAM_native.csv")
ZAM_native <- data.frame(ZAM_native, orthogroup)
ZAM_native$orthogroup <- as.character(ZAM_native$orthogroup)
ZAM_native$average.root <- as.numeric(ZAM_native$average.root)
ZAM_native$average.tip.leaf <- as.numeric(ZAM_native$average.tip.leaf)
ZAM_native$average.overall <- as.numeric(ZAM_native$average.overall)
ZAM_native$type <- "Recipient"
ZAM_native$donor <- ZAM_LGT$donor
ZAM_native$sd.root <- as.numeric(ZAM_native$sd.root)
ZAM_native$sd.tip.leaf <- as.numeric(ZAM_native$sd.tip.leaf) 
ZAM_native$se.root <- as.numeric(ZAM_native$se.root)
ZAM_native$se.tip.leaf <- as.numeric(ZAM_native$se.tip.leaf)
ZAM_native$expression.ZAM_root_1.txt <- as.numeric(ZAM_native$expression.ZAM_root_1.txt)
ZAM_native$expression.ZAM_root_2.txt <- as.numeric(ZAM_native$expression.ZAM_root_2.txt)
ZAM_native$expression.ZAM_root_3.txt <- as.numeric(ZAM_native$expression.ZAM_root_3.txt)
ZAM_native$expression.ZAM_tip_leaf_1.txt <- as.numeric(ZAM_native$expression.ZAM_tip_leaf_1.txt)
ZAM_native$expression.ZAM_tip_leaf_2.txt <- as.numeric(ZAM_native$expression.ZAM_tip_leaf_2.txt)
ZAM_native$expression.ZAM_tip_leaf_3.txt <- as.numeric(ZAM_native$expression.ZAM_tip_leaf_3.txt)
ZAM_native$accession <- "ZAM"


ZAM_SET_native <- read.csv("ZAM_SET_native.csv")
ZAM_SET_native <- data.frame(ZAM_SET_native, orthogroup)
ZAM_SET_native$orthogroup <- as.character(ZAM_SET_native$orthogroup)
ZAM_SET_native$average.root <- as.numeric(ZAM_SET_native$average.root)
ZAM_SET_native$average.tip.leaf <- as.numeric(ZAM_SET_native$average.tip.leaf)
ZAM_SET_native$average.overall <- as.numeric(ZAM_SET_native$average.overall)
ZAM_SET_native$type <- "Donor"
ZAM_SET_native$donor <- ZAM_LGT$donor
ZAM_SET_native$sd.root <- as.numeric(ZAM_SET_native$sd.root)
ZAM_SET_native$sd.tip.leaf <- as.numeric(ZAM_SET_native$sd.tip.leaf) 
ZAM_SET_native$se.root <- as.numeric(ZAM_SET_native$se.root)
ZAM_SET_native$se.tip.leaf <- as.numeric(ZAM_SET_native$se.tip.leaf)
glimpse(ZAM_SET_native)

ZAM_THE_native <- read.csv("ZAM_THE_native.csv")
ZAM_THE_native <- data.frame(ZAM_THE_native, orthogroup)
ZAM_THE_native$orthogroup <- as.character(ZAM_THE_native$orthogroup)
ZAM_THE_native$average.root <- as.numeric(ZAM_THE_native$average.root)
ZAM_THE_native$average.tip.leaf <- as.numeric(ZAM_THE_native$average.tip.leaf)
ZAM_THE_native$average.overall <- as.numeric(ZAM_THE_native$average.overall)
ZAM_THE_native$type <- "Donor"
ZAM_THE_native$donor <- ZAM_LGT$donor
ZAM_THE_native$sd.root <- as.numeric(ZAM_THE_native$sd.root)
ZAM_THE_native$sd.tip.leaf <- as.numeric(ZAM_THE_native$sd.tip.leaf) 
ZAM_THE_native$se.root <- as.numeric(ZAM_THE_native$se.root)
ZAM_THE_native$se.tip.leaf <- as.numeric(ZAM_THE_native$se.tip.leaf)


#KWT ----

#LGT
KWT_LGT <- read.csv("KWT_LGT.csv") 
KWT_LGT <- data.frame(KWT_LGT, orthogroup)
KWT_LGT$orthogroup <- as.character(KWT_LGT$orthogroup)
KWT_LGT$average.root <- as.numeric(KWT_LGT$average.root)
KWT_LGT$average.tip.leaf <- as.numeric(KWT_LGT$average.tip.leaf)
KWT_LGT$sd.root <- as.numeric(KWT_LGT$sd.root)
KWT_LGT$sd.tip.leaf <- as.numeric(KWT_LGT$sd.tip.leaf) 
KWT_LGT$se.root <- as.numeric(KWT_LGT$se.root)
KWT_LGT$se.tip.leaf <- as.numeric(KWT_LGT$se.tip.leaf)
KWT_LGT$average.overall <- as.numeric(KWT_LGT$average.overall)
KWT_LGT$type <- "LGT"
KWT_LGT$accession <- "KWT"

#Native 
KWT_native <- read.csv("KWT_native.csv")
KWT_native <- data.frame(KWT_native, orthogroup)
KWT_native$orthogroup <- as.character(KWT_native$orthogroup)
KWT_native$average.root <- as.numeric(KWT_native$average.root)
KWT_native$average.tip.leaf <- as.numeric(KWT_native$average.tip.leaf)
KWT_native$average.overall <- as.numeric(KWT_native$average.overall)
KWT_native$type <- "Recipient"
KWT_native$donor <- KWT_LGT$donor
KWT_native$sd.root <- as.numeric(KWT_native$sd.root)
KWT_native$sd.tip.leaf <- as.numeric(KWT_native$sd.tip.leaf) 
KWT_native$se.root <- as.numeric(KWT_native$se.root)
KWT_native$se.tip.leaf <- as.numeric(KWT_native$se.tip.leaf)
KWT_native$expression.KWT_root_A.txt <- as.numeric(KWT_native$expression.KWT_root_A.txt)
KWT_native$expression.KWT_root_B.txt <- as.numeric(KWT_native$expression.KWT_root_B.txt)
KWT_native$expression.KWT_root_C.txt <- as.numeric(KWT_native$expression.KWT_root_C.txt)
KWT_native$expression.KWT_tip_leaf_A.txt <- as.numeric(KWT_native$expression.KWT_tip_leaf_A.txt)
KWT_native$expression.KWT_tip_leaf_B.txt <- as.numeric(KWT_native$expression.KWT_tip_leaf_B.txt)
KWT_native$expression.KWT_tip_leaf_C.txt <- as.numeric(KWT_native$expression.KWT_tip_leaf_C.txt)
KWT_native$accession <- "KWT"

KWT_SET_native <- read.csv("KWT_SET_native.csv")
KWT_SET_native <- data.frame(KWT_SET_native, orthogroup)
KWT_SET_native$orthogroup <- as.character(KWT_SET_native$orthogroup)
KWT_SET_native$average.root <- as.numeric(KWT_SET_native$average.root)
KWT_SET_native$average.tip.leaf <- as.numeric(KWT_SET_native$average.tip.leaf)
KWT_SET_native$average.overall <- as.numeric(KWT_SET_native$average.overall)
KWT_SET_native$type <- "Donor"
KWT_SET_native$donor <- KWT_LGT$donor
KWT_SET_native$sd.root <- as.numeric(KWT_SET_native$sd.root)
KWT_SET_native$sd.tip.leaf <- as.numeric(KWT_SET_native$sd.tip.leaf) 
KWT_SET_native$se.root <- as.numeric(KWT_SET_native$se.root)
KWT_SET_native$se.tip.leaf <- as.numeric(KWT_SET_native$se.tip.leaf)
glimpse(KWT_SET_native)

KWT_THE_native <- read.csv("KWT_THE_native.csv")
KWT_THE_native <- data.frame(KWT_THE_native, orthogroup)
KWT_THE_native$orthogroup <- as.character(KWT_THE_native$orthogroup)
KWT_THE_native$average.root <- as.numeric(KWT_THE_native$average.root)
KWT_THE_native$average.tip.leaf <- as.numeric(KWT_THE_native$average.tip.leaf)
KWT_THE_native$average.overall <- as.numeric(KWT_THE_native$average.overall)
KWT_THE_native$type <- "Donor"
KWT_THE_native$donor <- KWT_LGT$donor
KWT_THE_native$sd.root <- as.numeric(KWT_THE_native$sd.root)
KWT_THE_native$sd.tip.leaf <- as.numeric(KWT_THE_native$sd.tip.leaf) 
KWT_THE_native$se.root <- as.numeric(KWT_THE_native$se.root)
KWT_THE_native$se.tip.leaf <- as.numeric(KWT_THE_native$se.tip.leaf)

#Donor native genes
SET_native <- read.csv("All_SET_native.csv")
SET_native <- data.frame(SET_native, orthogroup)
SET_native$orthogroup <- as.character(SET_native$orthogroup)
SET_native$type <- "SET"

THE_native <- read.csv("All_THE_native.csv")
THE_native <- data.frame(THE_native, orthogroup)
THE_native$orthogroup <- as.character(THE_native$orthogroup)
THE_native$type <- "THE"

#All accessions ----

All_accessions <- bind_rows(AUS_LGT, AUS_native, KWT_LGT, KWT_native, ZAM_LGT, ZAM_native)
All_accessions <- All_accessions %>% select(gene,orthogroup,type,accession,average.tip.leaf,average.root,donor)
All_accessions <- All_accessions %>% filter_at(vars(average.tip.leaf), all_vars(!is.na(.)))

All_accessions <- All_accessions %>% filter(donor == "Cenchrinae" | donor == "Andropogoneae")

All_accessions_LGT <- All_accessions %>% filter(type == "LGT")
All_accessions_recipient <- All_accessions %>% filter(type == "Recipient")

LGTortho <- anti_join(All_accessions_LGT, All_accessions_recipient, by = "orthogroup")
Recortho <- anti_join(All_accessions_recipient, All_accessions_LGT, by = "orthogroup")
Diffortho <- bind_rows(LGTortho, Recortho) # did this to check which orthogroups didn't match in LGT vs recipient 
Diffortho$orthogroup
#"170" "172" "39"  "50"  "163" "71"  "76"  "86"  "89"  "102" "106" "145" "148" "163" "170" "172" "174" "176" "177" "21"  "26"  "21"  "117" rows that aren't in both LGT/rec

All_accessions <- All_accessions %>% filter(orthogroup != "170")
All_accessions <- All_accessions %>% filter(orthogroup != "172") 
All_accessions <- All_accessions %>% filter(orthogroup != "39")
All_accessions <- All_accessions %>% filter(orthogroup != "50")
All_accessions <- All_accessions %>% filter(orthogroup != "163")
All_accessions <- All_accessions %>% filter(orthogroup != "71")
All_accessions <- All_accessions %>% filter(orthogroup != "76")
All_accessions <- All_accessions %>% filter(orthogroup != "86")
All_accessions <- All_accessions %>% filter(orthogroup != "89")
All_accessions <- All_accessions %>% filter(orthogroup != "102")
All_accessions <- All_accessions %>% filter(orthogroup != "106")
All_accessions <- All_accessions %>% filter(orthogroup != "145")
All_accessions <- All_accessions %>% filter(orthogroup != "148")
All_accessions <- All_accessions %>% filter(orthogroup != "163")
All_accessions <- All_accessions %>% filter(orthogroup != "174")
All_accessions <- All_accessions %>% filter(orthogroup != "176")
All_accessions <- All_accessions %>% filter(orthogroup != "177")
All_accessions <- All_accessions %>% filter(orthogroup != "21")
All_accessions <- All_accessions %>% filter(orthogroup != "26")
All_accessions <- All_accessions %>% filter(orthogroup != "117")

#All_accessions_LGT <- bind_rows(AUS_LGT, KWT_LGT, ZAM_LGT)
#All_accessions_native <- bind_rows(AUS_native, KWT_native, ZAM_native)

All_accessions_with_donor <- bind_rows(AUS_LGT, AUS_native, KWT_LGT, KWT_native, ZAM_LGT, ZAM_native, SET_native, THE_native)
All_accessions_with_donor <- All_accessions_with_donor %>% select(gene,orthogroup,type,accession,average.tip.leaf,average.root,donor)
All_accessions_with_donor <- All_accessions_with_donor %>% filter_at(vars(average.tip.leaf), all_vars(!is.na(.)))
All_accessions_with_donor <- All_accessions_with_donor %>% filter(donor == "Cenchrinae" | donor == "Andropogoneae") #not filtering 

#Donor not filtered as later plots don't require it 

All_accessions %>% count(average.root, type)

#stats ----
All_accessions_recipient <- All_accessions %>% filter(type == "Recipient")
All_accessions_LGT <- All_accessions %>% filter(type == "LGT")
kruskal.test(average.root ~ accession, data = All_accessions_recipient)
#Kruskal-Wallis chi-squared = 1.8117, df = 2, p-value = 0.4042
kruskal.test(average.tip.leaf ~ accession, data = All_accessions_recipient)
#Kruskal-Wallis chi-squared = 1.6856, df = 2, p-value = 0.4305
kruskal.test(average.root ~ accession, data = All_accessions_LGT)
#Kruskal-Wallis chi-squared = 1.7932, df = 2, p-value = 0.408
kruskal.test(average.tip.leaf ~ accession, data = All_accessions_LGT)
#Kruskal-Wallis chi-squared = 0.5298, df = 2, p-value = 0.7673


#histograms figure 1 results ----

setwd("//studata05/home/BO/Boa18jp/ManW10/Desktop/Masters/071422 work/express/zzzPascal-Antoine express/updated/graphs/results1")

ggplot(All_accessions, aes(x = average.root, fill = accession)) + geom_histogram() + facet_wrap(~type) +
  xlab("Average root expression")  +labs(fill = "Accession")
ggsave("root_histogram_all.png", width = 1920, height = 1080, units = c("px"))

#Restricted to 250
ggplot(All_accessions, aes(x = average.root, fill = accession)) + geom_histogram() + xlim(0, 250)  + ylim(0, 30) + facet_wrap(~type) +
  xlab("Average root expression") +labs(fill = "Accession")
ggsave("root_histogram_all_250.png", width = 1920, height = 1080, units = c("px"))

#Leaf tip  

#All data
ggplot(All_accessions, aes(x = average.tip.leaf, fill = accession)) + geom_histogram()  + facet_wrap(~type) +
xlab("Average tip leaf expression")+labs(fill = "Accession")
ggsave("leaf_histogram_all.png", width = 1920, height = 1080, units = c("px"))

#Restricted to 250
ggplot(All_accessions, aes(x = average.tip.leaf, fill = accession)) + geom_histogram() + xlim(0, 250) + ylim(0, 30) + facet_wrap(~type)+
  xlab("Average tip leaf expression")+labs(fill = "Accession")
ggsave("leaf_histogram_all_250.png", width = 1920, height = 1080, units = c("px"))

#dot plots figure 2 results ----
dotplotleaf <- All_accessions %>% select(average.tip.leaf, orthogroup, type, accession)
dotplotleaf  <- rename(dotplotleaf, expression = average.tip.leaf)
dotplotleaf$area <- "Tip.leaf"

dotplotroot <- All_accessions %>% select(average.root, orthogroup, type, accession)
dotplotroot  <- rename(dotplotroot, expression = average.root)
dotplotroot$area <- "Root"

dotplot <-  bind_rows(dotplotleaf, dotplotroot)
 
dotplotZAM <- dotplot %>% filter(accession == "ZAM")
dotplotZAM <- rename(dotplotZAM, ZAM_expression = expression)
dotplotZAM <- dotplotZAM %>%  select(ZAM_expression, orthogroup, type, area)
glimpse(dotplotZAM)

dotplotKWT <- dotplot %>% filter(accession == "KWT")
dotplotKWT <- rename(dotplotKWT, KWT_expression = expression)
dotplotKWT <- dotplotKWT %>%  select(KWT_expression, orthogroup, type, area)
dotplotKWT

dotplotAUS <- dotplot %>% filter(accession == "AUS")
dotplotAUS <- rename(dotplotAUS, AUS_expression = expression)
dotplotAUS <- dotplotAUS %>%  select(AUS_expression, orthogroup, type, area)
dotplotAUS

dotplotAUSKWT <- merge(dotplotAUS, dotplotKWT)
dotplotAUSZAM <- merge(dotplotZAM, dotplotAUS)
dotplotKWTZAM <- merge(dotplotZAM, dotplotKWT)

dotplotAUSKWT$difference <- dotplotAUSKWT$AUS_expression-dotplotAUSKWT$KWT_expression
dotplotAUSZAM$difference <- dotplotAUSZAM$AUS_expression-dotplotAUSZAM$ZAM_expression
dotplotKWTZAM$difference <- dotplotKWTZAM$KWT_expression-dotplotKWTZAM$ZAM_expression

aggregate(dotplotAUSKWT$AUS_expression, na.rm=TRUE, by = list (dotplotAUSKWT$area, dotplotAUSKWT$type), FUN = IQR)
#AUS 14.9 root, 5.53 leaf - KWT 5.32 root, 2.26 tip leaf  | recep AUS 1.25 root, 2.14 leafy; KWT 10.4 root, 16.8 leaf
aggregate(dotplotAUSKWT$KWT_expression, na.rm=TRUE, by = list (dotplotAUSKWT$area, dotplotAUSKWT$type), FUN = IQR)
#see above
aggregate(dotplotAUSZAM$AUS_expression, na.rm=TRUE, by = list (dotplotAUSZAM$area, dotplotAUSZAM$type), FUN = IQR)
#AUS - 26 ROOT, 26 leaf - ZAM 0.7 root, 1 leaf | recep AUS 54.4 root, 38.6 leaf; ZAM root 38.7 leafy 20 
aggregate(dotplotAUSZAM$ZAM_expression, na.rm=TRUE, by = list (dotplotAUSZAM$area, dotplotAUSZAM$type), FUN = IQR)
#see above
aggregate(dotplotKWTZAM$KWT_expression, na.rm=TRUE, by = list (dotplotKWTZAM$area, dotplotKWTZAM$type), FUN = IQR)
#KWT - 11 root, 10 tip leaf, ZAM - 4.5 root, 5.5 tip leaf | KWT 21.88 root, 11.36 leaf; ZAM 8.6 root, 15.7 leaf
aggregate(dotplotKWTZAM$ZAM_expression, na.rm=TRUE, by = list (dotplotKWTZAM$area, dotplotKWTZAM$type), FUN = IQR)
#see above

#looking at IQR/sum [change as needed] for expression of orthologues with LGTs/recipient genes in multiple accessions
aggregate(All_accessions_with_donor$average.root, na.rm=TRUE, by = list (All_accessions_with_donor$accession, All_accessions_with_donor$type), FUN = IQR)
aggregate(All_accessions_with_donor$average.tip.leaf, na.rm=TRUE, by = list (All_accessions_with_donor$accession, All_accessions_with_donor$type), FUN = IQR)


#check expr between total orthologues for accessions

#graphs

setwd("//studata05/home/BO/Boa18jp/ManW10/Desktop/Masters/071422 work/express/zzzPascal-Antoine express/updated/graphs/results2")

ggplot(dotplotAUSKWT, aes(x = AUS_expression, y = KWT_expression, color = area, shape = type)) +
  geom_point() + geom_segment(x=0,y=0,xend=100,yend=100, colour = "black") +
  ylim(0,70) + xlim(0,70)+
  labs(color = "Area", shape = "Type")
ggsave("AUSxKWT.png", width = 1920, height = 1080, units = c("px"))

ggplot(dotplotAUSZAM, aes(x = AUS_expression, y = ZAM_expression, color = area, shape = type)) +
  geom_point() + geom_segment(x=0,y=0,xend=800,yend=800, colour = "black") +
  labs(color = "Area", shape = "Type") +
  ylim(0,800) + xlim(0,800)
ggsave("AUSxZAM.png", width = 1920, height = 1080, units = c("px"))

ggplot(dotplotKWTZAM, aes(x = KWT_expression, y = ZAM_expression, color = area, shape = type)) +
  geom_point() + geom_segment(x=0,y=0,xend=700,yend=700, colour = "black") +
  labs(color = "Area", shape = "Type") +
  ylim(0,80) + xlim(0,80)
ggsave("KWTxZAM.png", width = 1920, height = 1080, units = c("px"))

#root vs leaf figure 3 ---- 
setwd("//studata05/home/BO/Boa18jp/ManW10/Desktop/Masters/071422 work/express/zzzPascal-Antoine express/updated/graphs/results3")

ggplot(All_accessions_with_donor, aes( x = average.tip.leaf, y = average.root, colour = type)) + geom_point() +
  ylim(0,250) +xlim(0,250) + geom_segment(x=0,y=0,xend=250,yend=250, color= "black")+
  labs(color = "Type", shape = "Type") +
  xlab("Average tip leaf expression") + ylab("Average root expression")
ggsave("RootvsTipleaf.png", width = 1920, height = 1080, units = c("px"))

#stats
Root_wilcox <- All_accessions %>% select(orthogroup, type, average.root, donor)
Root_wilcox <- rename(Root_wilcox, expression = average.root)
Root_wilcox$area <- "root"

tip.leaf_wilcox <- All_accessions %>% select(orthogroup, type, average.tip.leaf, donor)
tip.leaf_wilcox <- rename(tip.leaf_wilcox, expression = average.tip.leaf)
tip.leaf_wilcox$area <- "tip.leaf"

wilcoxrootleaf <- bind_rows(Root_wilcox, tip.leaf_wilcox)
wilcoxrootleafLGT <- wilcoxrootleaf %>% filter(type == "LGT")
wilcoxrootleafRec <- wilcoxrootleaf %>% filter(type == "Recipient")


wilcox.test(expression ~ area, paired = TRUE, data = wilcoxrootleafLGT)
#Wilcoxon signed rank test with continuity correction V = 3592, p-value = 0.002653
wilcoxrootleafLGT_THE <- wilcoxrootleafLGT %>% filter(donor == "Andropogoneae")
wilcox.test(expression ~area, paired = TRUE, data = wilcoxrootleafLGT_THE)
#V = 365, p-value = 0.6207

wilcoxrootleafLGT_SET <- wilcoxrootleafLGT %>% filter(donor == "Cenchrinae")
wilcox.test(expression ~area, paired = TRUE, data = wilcoxrootleafLGT_SET)
#V = 1677, p-value = 0.0007863

#LGT correlations

cor.test(All_accessions_LGT$average.tip.leaf, All_accessions_LGT$average.root, method = "spearman")
#S = 71035, p-value < 2.2e-16  rho 0.7764453, R2 0.6028673  LGT root TIP LEAF

#Recipient correlations 
cor.test(All_accessions_recipient$average.tip.leaf, All_accessions_recipient$average.root, method = "spearman")
#S = 103305, p-value < 2.2e-16 rho 0.7304889, R2 0.533614,Recipient root tip leaf 

wilcox.test(expression ~ area, paired = TRUE, data = wilcoxrootleafRec)
#Wilcoxon signed rank test with continuity correction  V = 4969, p-value = 0.01108


#Setaria correlations####### this was redone later on as some orthogroups only had one of either LGT or donor data 
#SET_spear <-  SET_native %>% filter(donor == "Cenchrinae") #did this test because while we got orthogroup 1-179 on SET, not all these orthogroups were found as LGTs
#cor.test(SET_spear$average.tip.leaf, SET_spear$average.root, method = "spearman")

#S = 19205, p-value = 3.812e-12, rho 0.7037303, R2 - 0.4952363  SET root/leaf correlation
#LGT SET correlations
#cor.test(SET_LGT$average.tip.leaf, SET_LGT$average.root, rm.na=TRUE, method = "spearman")
#S = 30950, p-value < 2.2e-16  rho 0.7900816 , R2 -  0.6242289 SET LGT root/leaf correlation

#Themedia correlations #### THESE TESTS WERE REDONE WITH MORE STRINGENT FILTERS

#THE_spear <-  THE_native %>% filter(donor == "Andropogoneae")
#cor.test(THE_spear$average.tip.leaf, THE_spear$average.root, method = "spearman")
#S = 2528.5, p-value = 5.186e-11, rho 0.8090849, r2 0.6546184 THE root/leaf correlation

#cor.test(THE_LGT$average.tip.leaf, THE_LGT$average.root, rm.na=TRUE, method = "spearman")
#S = 4142.4, p-value = 5.577e-10 rho = 0.7605019, r2 = 0.5783631 THE root/leaf correlation

############################ THESE CORRELATIONTESTS WERE REDONE LATER ON 
#SET wilcox root vs leaves 
SET_wilcox <- All_accessions_with_donor %>% filter(type == "SET")

SET_wilcox_root <- SET_wilcox %>% select(orthogroup, type, average.root)
SET_wilcox_root <- rename(SET_wilcox_root, expression = average.root)
SET_wilcox_root$area <- "root"

SET_wilcox_tip.leaf <- SET_wilcox %>% select(orthogroup, type, average.tip.leaf)
SET_wilcox_tip.leaf <- rename(SET_wilcox_tip.leaf, expression = average.tip.leaf)
SET_wilcox_tip.leaf$area <- "tip.leaf"

SET_wilcox_tiproot <- bind_rows(SET_wilcox_root, SET_wilcox_tip.leaf)
glimpse(SET_wilcox)

wilcox.test(expression ~ area, paired = TRUE, data = SET_wilcox_tiproot)
#V = 1523, p-value = 0.05965 alternative hypothesis: true location shift is not equal to 0


#THE values for table 1 of results (table 2 overall), wilcox and correlations
#Themeda wilcox root vs leaves 

THE_wilcox <- All_accessions_with_donor %>% filter(type == "THE")

THE_wilcox_root <- THE_wilcox %>% select(orthogroup, type, average.root)
THE_wilcox_root <- rename(THE_wilcox_root, expression = average.root)
THE_wilcox_root$area <- "root"

THE_wilcox_tip.leaf <- THE_wilcox %>% select(orthogroup, type, average.tip.leaf)
THE_wilcox_tip.leaf <- rename(THE_wilcox_tip.leaf, expression = average.tip.leaf)
THE_wilcox_tip.leaf$area <- "tip.leaf"

THE_wilcox <- bind_rows(THE_wilcox_root, THE_wilcox_tip.leaf)
glimpse(THE_wilcox)

wilcox.test(expression ~ area, paired = TRUE, data = THE_wilcox)
#V = 488, p-value = 0.4601 alternative hypothesis: true location shift is not equal to 0

#sums of expression
aggregate(THE_wilcox$average.root, na.rm=TRUE, by = list (THE = THE_wilcox$type), FUN = IQR)
aggregate(THE_wilcox$average.tip.leaf, na.rm=TRUE, by = list (THE = THE_wilcox$type), FUN = IQR)
#THE
#Root 33.75475
#tip.leaf 21.04

#LGTs derived from Andropog/THE
#Root 9.907833 
#tip leaf 11.3585 

aggregate(SET_wilcox$average.root, na.rm=TRUE, by = list (SET = SET_wilcox$type), FUN = IQR)
aggregate(SET_wilcox$average.tip.leaf, na.rm=TRUE, by = list (SET = SET_wilcox$type), FUN = IQR)
#SET
#Root 26.62000
#Tip.leaf 23.27111

#LGTs derived from Cenchrinae/SET 
#Root 12.09292
#Tip.leaf 8.005833


aggregate(All_accessions$average.tip.leaf, na.rm=TRUE, by = list (type = All_accessions$type), FUN = IQR)
#LGT 10.45117 
#Recipient 21.55897

aggregate(All_accessions$average.root, na.rm=TRUE, by = list (All_accessions$type), FUN = IQR)          
#LGT 16.14973
#Recipient 31.26583 

#Alloteropsis LGT vs Recipient wilcox comparisons
#filtering all groups which does not have both LGT / recipient
#note I make a new dataframe, I want to keep the All_accessions_with_donor unfiltered so I can look at individual genes later on 
Allo_wilcox <- All_accessions_with_donor %>% filter(type == "LGT" | type == "Recipient")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "102")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "106")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "117")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "145")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "148")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "174")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "176")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "177")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "26")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "39")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "50")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "71")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "76")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "170")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "86")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "89")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "172")
Allo_wilcox <- Allo_wilcox %>% filter(orthogroup != "21")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-AUS1-05848")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-37704")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-06903")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-06901")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-19399,ZAM150510-19400,ZAM150510-53954")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-17759,ZAM150510-48086")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-ZAM150510-43471")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-ZAM150510-04117")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-AUS1-01510")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-KWT3-15761")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-ZAM150510-04720")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-AUS1-31856")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-AUS1-21433")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "AUS1-05378-RA")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-39780")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-40072")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-64572,ZAM150510-65292,ZAM150510-34507")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-20465")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-14549")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-AUS1-25218RA")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-39086,ZAM150510-56345,ZAM150510-28242")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-46668,ZAM150510-17757,ZAM150510-66244")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-ZAM150510-35725,LGT-ZAM150510-40314,LGT-ZAM15")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "AUS1-18925RA,AUS1-18923RA")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-ZAM150510-45408,LGT-ZAM150510-45409,LGT-ZAM1")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "ZAM150510-59536,ZAM150510-39163,ZAM150510-13935,ZAM150510-42411")
Allo_wilcox <- Allo_wilcox %>% filter(gene != "LGT-AUS1-07291-RA,LGT-AUS1-39450RA")

#filter so data only includes accessions/orthogroup combos which have both recipient and donor genes  
 
aggregate(Allo_wilcox$average.tip.leaf, na.rm=TRUE, by = list (type = Allo_wilcox$type), FUN = IQR)
aggregate(Allo_wilcox$average.root, na.rm=TRUE, by = list (type = Allo_wilcox$type), FUN = IQR)

#note 
#Allo_wilcox_root <- Allo_wilcox %>% select(average.root, orthogroup, type)
#Allo_wilcox_tip.leaf <- Allo_wilcox %>% select(average.tip.leaf, orthogroup, type)
 

 #lgt v recepients
 
wilcox.test(average.root ~ type, paired = TRUE, data = Allo_wilcox_root)
#signed rank test  V = 2097, p-value = 0.001294

#wilcox.test(expression ~ type, data = Root_wilcox)
#not using un paired W = 5372, p-value = 1.971e-06 left in as reference 

wilcox.test(average.tip.leaf ~ type, paired = TRUE, data = Allo_wilcox_tip.leaf)
#V = 2030, p-value = 0.0006516

#not using as unpaired... description in SET section, left in as reference
#wilcox.test(expression ~ type, data = tip.leaf_wilcox)
#W = 5566.5, p-value = 9.429e-06

 
#testing recipient vs donors
rec_vs_donor <- All_accessions_with_donor %>% filter(type != "LGT")
rec_vs_donor_r <- rec_vs_donor %>% filter(type == "Recipient")
rec_vs_donor_s <- rec_vs_donor %>% filter(type == "SET")
rec_vs_donor_t <- rec_vs_donor %>% filter(type == "THE")
rec_vs_donor_s$type <-  "Donor"
rec_vs_donor_t$type <- "Donor"
rec_vs_donor_d <- bind_rows(rec_vs_donor_s, rec_vs_donor_t)

rec_vs_donor_filter <- bind_rows(rec_vs_donor_d, rec_vs_donor_r)
rec_vs_donor_filter %>% count(orthogroup)
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "102")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "89")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "86")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "76")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "106")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "122")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "137")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "138")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "140")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "141")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "142")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "145")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "148")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "151")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "170")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "172")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "174")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "175")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "176")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "177")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "27")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "29")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "30")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "39")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "61")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "71")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "76")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "86")
rec_vs_donor_filter <- rec_vs_donor_filter %>% filter(orthogroup != "89")
glimpse(rec_vs_donor_filter)
#Recipient vs donors
rec_vs_donor_filter_r <- rec_vs_donor_filter %>% filter(type == "Recipient")
rec_vs_donor_filter_r <- rename(rec_vs_donor_filter_r, recipient.root = average.root)
rec_vs_donor_filter_r <- rename(rec_vs_donor_filter_r, recipient.tip.leaf = average.tip.leaf)
rec_vs_donor_filter_r <- rec_vs_donor_filter_r %>% select(recipient.root, recipient.tip.leaf, orthogroup, donor)
rec_vs_donor_filter_d <- rec_vs_donor_filter %>% filter(type == "Donor")
rec_vs_donor_filter_d <- rename(rec_vs_donor_filter_d, donor.root = average.root)
rec_vs_donor_filter_d <- rename(rec_vs_donor_filter_d, donor.tip.leaf = average.tip.leaf)
rec_vs_donor_filter_d <- rec_vs_donor_filter_d %>% select(donor.root, donor.tip.leaf, orthogroup, donor)

rec_vs_donor_filter_dr <- merge(rec_vs_donor_filter_d, rec_vs_donor_filter_r)
#tested these because shows LGTs are uniquely downregulated in relation to native genes
wilcox.test(rec_vs_donor_filter_dr$recipient.root, rec_vs_donor_filter_dr$donor.root, paired = TRUE)
#V = 3820, p = 0.9869 wilcox signed rank 
wilcox.test(rec_vs_donor_filter_dr$recipient.tip.leaf, rec_vs_donor_filter_dr$donor.tip.leaf, paired = TRUE)
#V = 3524, p-value = 0.6676

THE_wilcox <- All_accessions_with_donor %>% filter(donor == "Andropogoneae")
THE_wilcox <- THE_wilcox %>% filter(type == "LGT" | type == "THE")
THE_wilcox_root <- THE_wilcox %>% select(average.root, orthogroup, type) 
THE_wilcox_tip.leaf <- THE_wilcox %>% select(average.tip.leaf, orthogroup, type) 

 
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "106")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "117")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "145")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "172")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "176")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "177")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "26")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "27")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "29")
THE_wilcox_root <- THE_wilcox_root %>% filter(orthogroup != "30")

THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "106")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "117")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "145")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "172")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "176")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "177")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "26")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "27")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "29")
THE_wilcox_tip.leaf <- THE_wilcox_tip.leaf %>% filter(orthogroup != "30")


#removing orthogroups which don't have both LGT and donor (THE) orthologue data

#preparing root data to be paired... twr = THE wilcox root 
LGTtwr <- THE_wilcox_root %>% filter(type == "LGT")
LGTtwr <- rename(LGTtwr, LGT.root = average.root)
LGTtwr <- LGTtwr %>% select(LGT.root, orthogroup)
DONtwr <- THE_wilcox_root %>% filter(type == "THE")
DONtwr <- rename(DONtwr, THE.root = average.root)
DONtwr <- DONtwr %>% select(THE.root, orthogroup)

mergetwr <- merge(LGTtwr, DONtwr)
LGTtwr <- mergetwr %>% select(LGT.root, orthogroup)
LGTtwr$type <-  "LGT"
LGTtwr  <- rename(LGTtwr, average.root = LGT.root)

DONtwr <- mergetwr %>% select(THE.root, orthogroup)
DONtwr$type <- "THE"
DONtwr <- rename(DONtwr, average.root = THE.root)

mergetwr <- bind_rows(LGTtwr, DONtwr)
 

#preparing tip.leaf data to be paired.. twl = THE wilcox tip.leaf

LGTtwl <- THE_wilcox_tip.leaf %>% filter(type == "LGT")
LGTtwl  <- rename(LGTtwl, LGT.tip.leaf = average.tip.leaf)
LGTtwl <- LGTtwl %>% select(LGT.tip.leaf, orthogroup)
DONtwl <- THE_wilcox_tip.leaf %>% filter(type == "THE")
DONtwl <- rename(DONtwl, THE.tip.leaf = average.tip.leaf)
DONtwl <- DONtwl %>% select(THE.tip.leaf, orthogroup)

mergetwl <- merge(LGTtwl, DONtwl)
LGTtwl <- mergetwl %>% select(LGT.tip.leaf, orthogroup)
LGTtwl$type <-  "LGT"
LGTtwl  <- rename(LGTtwl, average.tip.leaf = LGT.tip.leaf)

DONtwl <- mergetwl %>% select(THE.tip.leaf, orthogroup)
DONtwl$type <- "THE"
DONtwl <- rename(DONtwl, average.tip.leaf = THE.tip.leaf)

mergetwl <- bind_rows(LGTtwl, DONtwl)


wilcox.test(average.root ~ type, paired = TRUE, data = mergetwr)
#wilcoxon signed rank V = 125, p-value = 0.0003807

#wilcox.test(average.root ~ type, data = THE_wilcox_root)
#W = 482, p-value = 0.0319 non paired test, not using (see SET section for description)



wilcox.test(average.tip.leaf ~ type, paired = TRUE, data = mergetwl)
#wilcox signed rank V = 159, p-value = 0.003772 
#wilcox.test(average.tip.leaf ~type, data = THE_wilcox_tip.leaf
#W = 457, p-value = 0.01599 non-paired, left in as reference (see SET section for description)

#correlations and differences between orthogroups which contain both THE/LGT data
 
corTHEr_LGT <- THE_wilcox_root %>% filter(type == "LGT")
corTHEtl_LGT <- THE_wilcox_tip.leaf %>% filter(type == "LGT")
cor_THE_LGT <- corTHEr_LGT
cor_THE_LGT$average.tip.leaf <- corTHEtl_LGT$average.tip.leaf

cor.test(cor_THE_LGT$average.tip.leaf, cor_THE_LGT$average.root, rm.na=TRUE, method = "spearman")
#S = 2607.7, p-value = 1.769e-08, rho - 0.755378, r2 = 0.57

 

corTHEr_THE <- THE_wilcox_root %>% filter(type == "THE")
corTHEtl_THE <- THE_wilcox_tip.leaf %>% filter(type == "THE")
cor_THE_THE <- corTHEr_THE
cor_THE_THE$average.tip.leaf <- corTHEtl_THE$average.tip.leaf

cor.test(cor_THE_THE$average.tip.leaf, cor_THE_THE$average.root, rm.na=TRUE, method = "spearman")
#S = 1278.4, p-value = 9.727e-09, rho = 0.8046804, r2 = 0.65  

 

 #LGT vs SET 


SET_wilcox <- All_accessions_with_donor %>% filter(donor == "Cenchrinae")
SET_wilcox <- SET_wilcox %>% filter(type == "LGT" | type == "SET")
SET_wilcox_root <- SET_wilcox %>% select(average.root, orthogroup, type) 
SET_wilcox_tip.leaf <- SET_wilcox %>% select(average.tip.leaf, orthogroup, type) 

SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "122")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "140")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "141")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "142")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "151")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "21")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "50")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "61") 
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "137")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "138")
SET_wilcox_root <-SET_wilcox_root %>% filter(orthogroup != "175")

SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "122")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "140")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "141")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "142")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "151")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "21")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "50")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "61") 
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "137")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "138")
SET_wilcox_tip.leaf <-SET_wilcox_tip.leaf %>% filter(orthogroup != "175")

#removing orthogroups which don't have LGT and donor (SET) orthologue data

#preparing root data to be paired... swr = SET wilcox root 
LGTswr <- SET_wilcox_root %>% filter(type == "LGT")
LGTswr  <- rename(LGTswr, LGT.root = average.root)
LGTswr <- LGTswr %>% select(LGT.root, orthogroup)
DONswr <- SET_wilcox_root %>% filter(type == "SET")
DONswr <- rename(DONswr, SET.root = average.root)
DONswr <- DONswr %>% select(SET.root, orthogroup)

mergeSWR <- merge(LGTswr, DONswr)
LGTswr <- mergeSWR %>% select(LGT.root, orthogroup)
LGTswr$type <-  "LGT"
LGTswr  <- rename(LGTswr, average.root = LGT.root)

DONswr <- mergeSWR %>% select(SET.root, orthogroup)
DONswr$type <- "SET"
DONswr <- rename(DONswr, average.root = SET.root)

#preparing tip.leaf data to be paired.. swl = SET wilcox tip.leaf

LGTswl <- SET_wilcox_tip.leaf %>% filter(type == "LGT")
LGTswl  <- rename(LGTswl, LGT.tip.leaf = average.tip.leaf)
LGTswl <- LGTswl %>% select(LGT.tip.leaf, orthogroup)
DONswl <- SET_wilcox_tip.leaf %>% filter(type == "SET")
DONswl <- rename(DONswl, SET.tip.leaf = average.tip.leaf)
DONswl <- DONswl %>% select(SET.tip.leaf, orthogroup)

mergeswl <- merge(LGTswl, DONswl)
LGTswl <- mergeswl %>% select(LGT.tip.leaf, orthogroup)
LGTswl$type <-  "LGT"
LGTswl  <- rename(LGTswl, average.tip.leaf = LGT.tip.leaf)

DONswl <- mergeswl %>% select(SET.tip.leaf, orthogroup)
DONswl$type <- "SET"
DONswl <- rename(DONswl, average.tip.leaf = SET.tip.leaf)

mergeswl <- bind_rows(LGTswl, DONswl)

#maybe code isn't efficiently wrote, but I originally did this test without pairing the data because there were multiple LGT from different accessions meaning LGT/SET had a different number of genes
#I realised today (25/05/22) that despite this I still paired distance data somehow
#...so I looked into it and replicated the method here, the code separates SET_wilcox values into LGT or SET, reassigns expression names and merges data
#the merged data has an equal number of LGT and SET rows (replicating SET to match LGT count makes sense as its responsible for donating to multiple accessions)... did alternatively think of averaging accession data but thought this method was better
#after breaking the merged data up again and renaming expression values to the same name, I bound the rows so i could sort by root.expression/tip.leaf expression against type (LGT or SET)
#and now pairing works!

wilcox.test(average.root ~ type, paired = TRUE, data = mergeSWR)
#Wilcoxon signed rank-test V = 1046, p-value = 0.001566 

#wilcox.test(average.root ~ type, data = SET_wilcox_root)
#Wilcoxon rank-sum test W = 2024, p-value = 7.218e-05 - not using this test in my results but left it for reference 

wilcox.test(average.tip.leaf ~ type, paired = TRUE, data = mergeswl)
#Wilcoxon signed-rank test V = 1185, p-value = 0.01706

#wilcox.test(average.tip.leaf ~ type, data = SET_wilcox_tip.leaf)
#W = 2237.5, p-value = 0.001176 - not using test in results, thought I'd leave it for reference 

#Recipient vs THE, these stats are not used in writeup. 
RecTHE_root <- bind_rows(THE_wilcox_root, Root_wilcox_Recipient)
Rec_THE_tip.leaf <- bind_rows(THE_wilcox_tip.leaf, tip.leaf_wilcox_Recipient )

wilcox.test(expression~type, data = RecTHE_root)
#W = 3028, p-value = 0.5113

wilcox.test(expression~type, data = Rec_THE_tip.leaf)
#W = 2775.5, p-value = 0.8298

#correlations between orthogroups which contain both SET/LGT data

corSETr_LGT <- SET_wilcox_root %>% filter(type == "LGT")
corSETtl_LGT <- SET_wilcox_tip.leaf %>% filter(type == "LGT")
cor_SET_LGT <- corSETr_LGT
cor_SET_LGT$average.tip.leaf <- corSETtl_LGT$average.tip.leaf

cor.test(cor_SET_LGT$average.tip.leaf, cor_SET_LGT$average.root, rm.na=TRUE, method = "spearman")
# S = 25816, p-value < 2.2e-16 rho 0.7802559, r2 = 0.6087993

corSETr_SET <- SET_wilcox_root %>% filter(type == "SET")
corSETtl_SET <- SET_wilcox_tip.leaf %>% filter(type == "SET")
cor_SET_SET <- corSETr_SET
cor_SET_SET$average.tip.leaf <- corSETtl_SET$average.tip.leaf

cor.test(cor_SET_SET$average.tip.leaf, cor_SET_SET$average.root, rm.na=TRUE, method = "spearman")
# S = 15809, p-value = 7.503e-12, rho = 0.7111932, r2 = 0.51


#figure 4 boxplots ----
#Quantiles
All_accessions_with_donor <- All_accessions_with_donor %>% select(gene, average.root, average.tip.leaf, donor, accession, orthogroup, type)

#Boxplots ---- 
setwd("//studata05/home/BO/Boa18jp/ManW10/Desktop/Masters/071422 work/express/zzzPascal-Antoine express/updated/graphs/results4")

ggplot(All_accessions_with_donor, aes(x = accession, y = average.root, fill = type)) + geom_boxplot()  +ylim(0,4000)
#ggsave("tip.leaf_boxplot_nolim.png", width = 1920, height = 1080, units = c("px"))

ggplot(All_accessions_with_donor, aes(x = type, y = average.tip.leaf, fill = type)) + geom_boxplot()   + ylim(0,250) + 
  xlab("") +ylab("Average tip leaf expression") + scale_fill_discrete(name = "Type of gene")
ggsave("type_tip.leaf_boxplot_250lim.png", width = 1920, height = 1080, units = c("px"))

ggplot(All_accessions_with_donor, aes(x = accession, y = average.root, fill = type)) + geom_boxplot()  
#ggsave("root_boxplot_nolim.png", width = 1920, height = 1080, units = c("px"))

ggplot(All_accessions_with_donor, aes(x = type, y = average.root, fill = type)) + geom_boxplot()+ 
  ylim(0,250) + 
  xlab("") + ylab("Average root expression") + scale_fill_discrete(name = "Type of gene")
ggsave("type_root_boxplot_250lim.png", width = 1920, height = 1080, units = c("px"))


#distance plots prep (Fig 5/6) ---- 

#preparation
#AUS
AUS_SET <- bind_rows(AUS_LGT, AUS_native, AUS_SET_native)
AUS_SET$accession <- "AUS"
AUS_THE <- bind_rows(AUS_LGT, AUS_native, AUS_THE_native)  
AUS_THE$accession <- "AUS"
#Finish off data preparation by filtering to only include genes with the appropriate donor
AUS_SET <- AUS_SET %>% filter(donor == "Cenchrinae")
AUS_THE <- AUS_THE %>% filter(donor == "Andropogoneae")

#ZAM
ZAM_SET <- bind_rows(ZAM_LGT, ZAM_native, ZAM_SET_native)
ZAM_SET$accession <- "ZAM"
ZAM_THE <- bind_rows(ZAM_LGT, ZAM_native, ZAM_THE_native)  
ZAM_THE$accession <- "ZAM"
#Finish off data preparation by filtering to only include genes with the appropriate donor
ZAM_SET <- ZAM_SET %>% filter(donor == "Cenchrinae")
ZAM_THE <- ZAM_THE %>% filter(donor == "Andropogoneae")

#KWT
KWT_SET <- bind_rows(KWT_LGT, KWT_native, KWT_SET_native)
KWT_SET$accession <- "KWT"
KWT_THE <- bind_rows(KWT_LGT, KWT_native, KWT_THE_native)  
KWT_THE$accession <- "KWT"
#Finish off data preparation by filtering to only include genes with the appropriate donor
KWT_SET <- KWT_SET %>% filter(donor == "Cenchrinae")
KWT_THE <- KWT_THE %>% filter(donor == "Andropogoneae")

#merge                    
ALL_SET <- bind_rows(ZAM_SET, KWT_SET, AUS_SET)
SET_LGT <- ALL_SET %>% filter(type == "LGT")
SET_recipient <- ALL_SET %>% filter(type == "Recipient")
SET_donor <- ALL_SET %>% filter(type == "Donor")

#calculations
 

ALL_SET$LGT_recipient_distance <- sqrt((SET_LGT$average.root-SET_recipient$average.root)^2+(SET_LGT$average.tip.leaf-SET_recipient$average.tip.leaf)^2)
ALL_SET$LGT_donor_distance<- sqrt((SET_LGT$average.root-SET_donor$average.root)^2+(SET_LGT$average.tip.leaf-SET_donor$average.tip.leaf)^2)
ALL_SET$donor_recipient_distance <- sqrt((SET_donor$average.root-SET_recipient$average.root)^2+(SET_donor$average.tip.leaf-SET_recipient$average.tip.leaf)^2)

ALL_THE <- bind_rows(ZAM_THE, KWT_THE, AUS_THE)
THE_LGT <- ALL_THE %>% filter(type == "LGT")
THE_recipient <- ALL_THE %>% filter(type == "Recipient")
THE_donor <- ALL_THE %>% filter(type == "Donor")

ALL_THE$LGT_recipient_distance <- sqrt((THE_LGT$average.root-THE_recipient$average.root)^2+(THE_LGT$average.tip.leaf-THE_recipient$average.tip.leaf)^2)
ALL_THE$LGT_donor_distance<- sqrt((THE_LGT$average.root-THE_donor$average.root)^2+(THE_LGT$average.tip.leaf-THE_donor$average.tip.leaf)^2)
ALL_THE$donor_recipient_distance <- sqrt((THE_donor$average.root-THE_recipient$average.root)^2+(THE_donor$average.tip.leaf-THE_recipient$average.tip.leaf)^2)

#Counts for table 
dist_count_orthogroups <- c(1:111)
SET_dist_count <- as.data.frame(dist_count_orthogroups)
SET_dist_count$LGT_recipient_distance <- sqrt((SET_LGT$average.root-SET_recipient$average.root)^2+(SET_LGT$average.tip.leaf-SET_recipient$average.tip.leaf)^2)
SET_dist_count$LGT_donor_distance<- sqrt((SET_LGT$average.root-SET_donor$average.root)^2+(SET_LGT$average.tip.leaf-SET_donor$average.tip.leaf)^2)
SET_dist_count$donor_recipient_distance <- sqrt((SET_donor$average.root-SET_recipient$average.root)^2+(SET_donor$average.tip.leaf-SET_recipient$average.tip.leaf)^2)
SET_dist_count <- na.omit(SET_dist_count)

SET_dist_count_fig5 <- SET_dist_count %>% select(LGT_recipient_distance, LGT_donor_distance, dist_count_orthogroups)
SET_dist_count_fig5$average <- (SET_dist_count_fig5$LGT_recipient_distance+SET_dist_count_fig5$LGT_donor_distance)/2
SET_dist_count_fig5$percent_of_avg_recepient <- (SET_dist_count_fig5$LGT_recipient_distance / (SET_dist_count_fig5$average))*100
SET_dist_count_fig5$percent_of_avg_donor <- (SET_dist_count_fig5$LGT_donor_distance / (SET_dist_count_fig5$average))*100
SET_dist_count_fig5$recep_percent_of_donor <- (SET_dist_count_fig5$LGT_recipient_distance / SET_dist_count_fig5$LGT_donor_distance)*100
SET_dist_count_fig5$donor_percent_of_recep <- (SET_dist_count_fig5$LGT_donor_distance / SET_dist_count_fig5$LGT_recipient_distance)*100

SET_dist_count_fig6 <- SET_dist_count %>% select(LGT_recipient_distance, donor_recipient_distance, dist_count_orthogroups)
SET_dist_count_fig6$average <- (SET_dist_count_fig6$LGT_recipient_distance+SET_dist_count_fig6$donor_recipient_distance)/2
SET_dist_count_fig6$percent_of_avg_LGT <- (SET_dist_count_fig6$LGT_recipient_distance / (SET_dist_count_fig6$average))*100
SET_dist_count_fig6$percent_of_avg_donor <- (SET_dist_count_fig6$donor_recipient_distance / (SET_dist_count_fig6$average))*100
SET_dist_count_fig6$LGT_percent_of_donor <- (SET_dist_count_fig6$LGT_recipient_distance / SET_dist_count_fig6$donor_recipient_distance)*100
SET_dist_count_fig6$donor_percent_of_LGT <- (SET_dist_count_fig6$donor_recipient_distance / SET_dist_count_fig6$LGT_recipient_distance)*100


SET_dist_count %>% count(dist_count_orthogroups) # 74 total counts for SET plots


dist_count_orthogroups <- c(1:62)

THE_dist_count <- as.data.frame(dist_count_orthogroups)
THE_dist_count$LGT_recipient_distance <- sqrt((THE_LGT$average.root-THE_recipient$average.root)^2+(THE_LGT$average.tip.leaf-THE_recipient$average.tip.leaf)^2)
THE_dist_count$LGT_donor_distance<- sqrt((THE_LGT$average.root-THE_donor$average.root)^2+(THE_LGT$average.tip.leaf-THE_donor$average.tip.leaf)^2)
THE_dist_count$donor_recipient_distance <- sqrt((THE_donor$average.root-THE_recipient$average.root)^2+(THE_donor$average.tip.leaf-THE_recipient$average.tip.leaf)^2)
THE_dist_count <- na.omit(THE_dist_count)

THE_dist_count_fig5 <- THE_dist_count %>% select(LGT_recipient_distance, LGT_donor_distance, dist_count_orthogroups)
THE_dist_count_fig5$average <- (THE_dist_count_fig5$LGT_recipient_distance+THE_dist_count_fig5$LGT_donor_distance)/2
THE_dist_count_fig5$percent_of_avg_recepient <- (THE_dist_count_fig5$LGT_recipient_distance / (THE_dist_count_fig5$average))*100
THE_dist_count_fig5$percent_of_avg_donor <- (THE_dist_count_fig5$LGT_donor_distance / (THE_dist_count_fig5$average))*100
THE_dist_count_fig5$recep_percent_of_donor <- (THE_dist_count_fig5$LGT_recipient_distance / THE_dist_count_fig5$LGT_donor_distance)*100
THE_dist_count_fig5$donor_percent_of_recep <- (THE_dist_count_fig5$LGT_donor_distance / THE_dist_count_fig5$LGT_recipient_distance)*100

THE_dist_count %>% count(dist_count_orthogroups) # 35 total counts here 

THE_dist_count_fig6 <- THE_dist_count %>% select(LGT_recipient_distance, donor_recipient_distance, dist_count_orthogroups)
THE_dist_count_fig6$average <- (THE_dist_count_fig6$LGT_recipient_distance+THE_dist_count_fig6$donor_recipient_distance)/2
THE_dist_count_fig6$percent_of_avg_LGT <- (THE_dist_count_fig6$LGT_recipient_distance / (THE_dist_count_fig6$average))*100
THE_dist_count_fig6$percent_of_avg_donor <- (THE_dist_count_fig6$donor_recipient_distance / (THE_dist_count_fig6$average))*100
THE_dist_count_fig6$LGT_percent_of_donor <- (THE_dist_count_fig6$LGT_recipient_distance / THE_dist_count_fig6$donor_recipient_distance)*100
THE_dist_count_fig6$donor_percent_of_LGT <- (THE_dist_count_fig6$donor_recipient_distance / THE_dist_count_fig6$LGT_recipient_distance)*100
                                               

#Figure 5 LGT dist ---- 

setwd("//studata05/home/BO/Boa18jp/ManW10/Desktop/Masters/071422 work/express/zzzPascal-Antoine express/updated/graphs/results5")
wilcox.test(SET_dist_count$LGT_recipient_distance, SET_dist_count$LGT_donor_distance, paired = T)
 # V = 1189, p-value = 0.6121

ggplot(ALL_SET, aes(x = LGT_donor_distance, y = LGT_recipient_distance))+ geom_point(size = 2)  +
  xlim(0,600)+ylim(0,600) + geom_segment(x=0,y=0,xend=600,yend=600) + xlab("LGT distance to donor (S. italica)") + 
  ylab("LGT distance to recipient") 
ggsave("SET_distance_LGT.png", width = 1920, height = 1080, units = c("px"))

wilcox.test(THE_dist_count$LGT_recipient_distance, THE_dist_count$LGT_donor_distance, paired = T)
 #V = 255, p-value = 0.3341 

ggplot(ALL_THE, aes(x = LGT_donor_distance, y = LGT_recipient_distance))+ geom_point(size = 2)  +
  xlim(0,600)+ylim(0,600) + geom_segment(x=0,y=0,xend=600,yend=600) + xlab("LGT distance to donor (T. triandra)") + 
  ylab("LGT distance to recipient")  
ggsave("THE_distance_LGT.png", width = 1920, height = 1080, units = c("px"))

#Figure 6 recipient dist ----
setwd("//studata05/home/BO/Boa18jp/ManW10/Desktop/Masters/071422 work/express/zzzPascal-Antoine express/updated/graphs/results6")

wilcox.test(SET_dist_count$LGT_recipient_distance, SET_dist_count$donor_recipient_distance, paired = T)
  #V = 1663, p-value = 0.0505

ggplot(ALL_SET, aes(y= LGT_recipient_distance, x = donor_recipient_distance)) + geom_point(size = 2) +
  xlim(0,600)+ylim(0,600)+ geom_segment(x=0,y=0,xend=600,yend=600) + ylab("Recipient distance to LGT") + xlab("Recipient distance to donor (S. italica)")
ggsave("SET_distance_recipient.png", width = 1920, height = 1080, units = c("px"))

wilcox.test(THE_dist_count$LGT_recipient_distance, THE_dist_count$donor_recipient_distance, paired = T)
 #V = 283, p-value = 0.8108

ggplot(ALL_THE, aes(y= LGT_recipient_distance, x = donor_recipient_distance)) + geom_point(size = 2) +
  xlim(0,600)+ylim(0,600)+ geom_segment(x=0,y=0,xend=600,yend=600) + ylab("Recipient distance to LGT") + xlab("Recipient distance to donor (T. triandra)")  
ggsave("THE_distance_recipient.png", width = 1920, height = 1080, units = c("px"))
 
 