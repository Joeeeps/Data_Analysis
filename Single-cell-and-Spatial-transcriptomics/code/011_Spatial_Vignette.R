#### read libraries ####
library(here)
#https://giottosuite.readthedocs.io/en/master/ 

#### define directories ####
DIR_PROJECT <- paste0(here(),"/")
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")

DIR_FIG_OUT  <- paste0(DIR_FIG,"011/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"011/")

if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

##### Analysis begin

#This is how to use Seurat to analyse spatially resolved RNA-data

