#### read libraries ####
library(here)
library(DWLS)
library(spacexr)
library(Seurat)
library(tidyverse)
setwd("C:/Users/guest_joe/Desktop/01_Spatial Analysis Project")

#### define directories ####
DIR_PROJECT <- paste0(here(),"/")
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")

DIR_FIG_OUT  <- paste0(DIR_FIG,"003/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"003/")

if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

#### Start analysis ####

#### RCTD is a computational method that allows the discovery of genes within a cell type whose expression depends on the spatial environment ####

#### install RCTD ####
#devtools::install_github("dmcable/RCTD", build_vignettes = TRUE)

#### Process Single Cell Reference ####
#Requires 'counts' and 'cell_types' information
shamcerspatial <- readRDS("data/002/RDS/shamcerspatial.rds")
shamcerboth <- readRDS("data/001/RDS/shamcerboth_cell_rename.rds")
 
#For preparing the reference, I did this:
# 1. get the counts for both spatial and single cell

shamcerbothcounts <- GetAssayData(object = shamcerboth, slot = "counts")
shamcerspatialcounts <- GetAssayData(object = shamcerspatial, slot = "counts")

#check image names#
Images(shamcerspatial)

shamcerspatialcounts_c1 <- shamcerspatialcounts[, shamcerspatial$sample=="cancer1"]
shamcerspatialcounts_c2 <- shamcerspatialcounts[, shamcerspatial$sample=="cancer2"]
shamcerspatialcounts_s1 <- shamcerspatialcounts[, shamcerspatial$sample=="sham1"]
shamcerspatialcounts_s2 <- shamcerspatialcounts[, shamcerspatial$sample=="sham2"]


# 2. get the annotations (here I just used the clusters; not cell type names)
cell_types <-  shamcerboth$seurat_clusters

# 3. Create the Reference object
reference <- Reference(shamcerbothcounts, cell_types)

#Now we have the count data for each cell type in the scRNA data
# 4. Get spatial coordinates
shamcerspatialcoords_c1 <- GetTissueCoordinates(shamcerspatial, image = "slice1_cancer1") 
shamcerspatialcoords_c2 <- GetTissueCoordinates(shamcerspatial, image = "slice1_cancer2") 
shamcerspatialcoords_s1 <- GetTissueCoordinates(shamcerspatial, image = "slice1") 
shamcerspatialcoords_s2 <- GetTissueCoordinates(shamcerspatial, image = "slice1_sham2") 

dim(shamcerspatialcounts_c1)
dim(shamcerspatialcoords_c1)

# 5. Prepare data for SpatialRNA conversion
 
#nUMI <- colSums(shamcerbothcounts) # In this case, total counts per pixel is nUMI - number of unique molecular identifiers

#rownames(shamcerspatialcoords) <- shamcerspatialcoords$barcodes; shamcerspatialcoords$barcodes <- NULL # Move barcodes to rownames
# 5. Create SpatialRNA object

puck_c1 <- SpatialRNA(coords = shamcerspatialcoords_c1, counts = shamcerspatialcounts_c1)
puck_c2 <- SpatialRNA(coords = shamcerspatialcoords_c2, counts = shamcerspatialcounts_c2)
puck_s1 <- SpatialRNA(coords = shamcerspatialcoords_s1, counts = shamcerspatialcounts_s1)
puck_s2 <- SpatialRNA(coords = shamcerspatialcoords_s2, counts = shamcerspatialcounts_s2)

#observing data
print(dim(puck_c1@counts))
hist(log(puck_c1@nUMI))

barcodes_c1 <- colnames(puck_c1@counts) # pixels to be used (a list of barcode names). 
barcodes_c2 <- colnames(puck_c2@counts) # pixels to be used (a list of barcode names). 
barcodes_s1 <- colnames(puck_s1@counts) # pixels to be used (a list of barcode names). 
barcodes_s2 <- colnames(puck_s2@counts) # pixels to be used (a list of barcode names). 

###### spatial plotting ######

#plotting nUMI for each pixel
#####this plot should resemble the spatial area 
c1_plot <- plot_puck_continuous(puck_c1, barcodes_c1, puck_c1@nUMI, size = 4, ylimit = c(0,round(quantile(puck_c1@nUMI,0.9))), 
                     title ='Cancer1') 
c2_plot <- plot_puck_continuous(puck_c2, barcodes_c2, puck_c2@nUMI, size = 4,ylimit = c(0,round(quantile(puck_c2@nUMI,0.9))), 
                     title ='Cancer2') 
s1_plot <- plot_puck_continuous(puck_s1, barcodes_s1, puck_s1@nUMI, size = 4,ylimit = c(0,round(quantile(puck_s1@nUMI,0.9))), 
                                title ='Sham1') 
s2_plot <- plot_puck_continuous(puck_s2, barcodes_s2, puck_s2@nUMI, size = 4,ylimit = c(0,round(quantile(puck_s2@nUMI,0.9))), 
                                title ='Sham2') 
plot_puck_continuous(puck_s2, barcodes_s2, puck_s2@nUMI, size = 4,ylimit = c(0,round(quantile(puck_s2@nUMI,0.9))), 
                     title ='Sham2') 


c1_plot+c2_plot + s1_plot+s2_plot
ggsave("figures/003/nUMI_plot_puck.pdf", width = 16, height = 9, dpi = 100)
 
#test puck plot

plot_puck_continuous(puck_s2, barcodes_s2, puck_s2@nUMI, size = 4,ylimit = c(0,round(quantile(puck_s2@nUMI,0.9))), 
                     title ='Sham2') 
get_cell_type_info(shamcerspatial, cell_type_names, nUMI = puck_c1@nUMI, cell_type_names = NULL)


#saveRDS(puck_c1, file = "data/003/RDS/puck_c1.rds")
#saveRDS(puck_c2, file = "data/003/RDS/puck_c2.rds")
#saveRDS(puck_s1, file = "data/003/RDS/puck_s1.rds")
#saveRDS(puck_s2, file = "data/003/RDS/puck_s2.rds")

RCTD_c1 <- create.RCTD(reference = reference, spatialRNA = puck_c1)
RCTD_c2 <- create.RCTD(reference = reference, spatialRNA = puck_c2)
RCTD_s1 <- create.RCTD(reference = reference, spatialRNA = puck_s1)
RCTD_s2 <- create.RCTD(reference = reference, spatialRNA = puck_s2)


myRCTD_c1 <- run.RCTD(RCTD_c1, doublet_mode = 'full') #full, multi and doublet differences
saveRDS(myRCTD_c1, file = "data/003/RDS/full_myRCTD_c1.rds")

myRCTD_c2 <- run.RCTD(RCTD_c2, doublet_mode = 'full')
saveRDS(myRCTD_c2, file = "data/003/RDS/full_myRCTD_c2.rds")

myRCTD_s1 <- run.RCTD(RCTD_s1, doublet_mode = 'full')
saveRDS(myRCTD_s1, file = "data/003/RDS/full_myRCTD_s1.rds")

myRCTD_s2 <- run.RCTD(RCTD_s2, doublet_mode = 'full')
saveRDS(myRCTD_s2, file = "data/003/RDS/full_myRCTD_s2.rds")
 
#### RCTD results ####
myRCTD_c1 <- readRDS("data/003/RDS/full_myRCTD_c1.rds")
myRCTD_c2 <- readRDS("data/003/RDS/full_myRCTD_c2.rds")
myRCTD_s1 <- readRDS("data/003/RDS/full_myRCTD_s1.rds")
myRCTD_s2 <- readRDS("data/003/RDS/full_myRCTD_s2.rds")

#The results of RCTD are located in @results field, with @results$weights for cell type weights for each pixel

results_c1 <- myRCTD_c1@results
results_c2 <- myRCTD_c2@results
results_s1 <- myRCTD_s1@results
results_s2 <- myRCTD_s2@results

rctd_lst <- list()
rctd_lst[["c1"]] <- myRCTD_c1
rctd_lst[["c2"]] <- myRCTD_c2
rctd_lst[["s1"]] <- myRCTD_s1
rctd_lst[["s2"]] <- myRCTD_s2

results_lst <- list() #create lists for all 4 datasets results
results_lst[["c1"]] <- results_c1
results_lst[["c2"]] <- results_c2
results_lst[["s1"]] <- results_s1
results_lst[["s2"]] <- results_s2


for(i in 1:4) { #loop to plot RCTD graphs for each of four datasets
  results <- results_lst[[i]]
  rctd <- rctd_lst[[i]]
  norm_weights = normalize_weights(results$weights) #normalise the cell type proportions to sum to 1
  print(colMeans(norm_weights))
  cell_type_names <- rctd@cell_type_info$info[[2]] #list of cell type names 
  spatialRNA <- rctd@spatialRNA
  resultsdir <- paste0('figures/003/RCTD_Plots/',names(rctd_lst[i]),"") #save to specified directory
  resultsdir
  #plotting
  plot_weights(cell_type_names, spatialRNA, resultsdir, norm_weights)  # confidence weights for each cell type as in full mode
  plot_weights_unthreshold(cell_type_names, spatialRNA, resultsdir, norm_weights)# Plots all weights for each cell type as in full_mode. (saved as 
  # 'results/cell_type_occur.pdf')
  plot_cond_occur(cell_type_names, resultsdir, norm_weights, spatialRNA)
  # makes a map of all cell types, (saved as 
  # 'results/all_cell_types.pdf')
}


resultc1 <- results_lst[1]
resultc2 <- results_lst[2]
results1 <- results_lst[3]
results2 <- results_lst[4]

normc1 = normalize_weights(results_lst$c1$weights)
normc2 = normalize_weights(results_lst$c2$weights)
norms1 = normalize_weights(results_lst$s1$weights)
norms2 = normalize_weights(results_lst$s2$weights)

meanc1 <- colMeans(normc1)
meanc1$source <- "c1"
meanc2 <- colMeans(normc2)
meanc2$source <- "c2"
means1 <- colMeans(norms1)
means1$source <- "s1"
means2 <- colMeans(norms2)
means2$source <- "s2"

testi <- bind_rows(meanc1,meanc2,means1,means2)
write.csv(testi, "data/003/testi.csv") 
glimpse(testi)


mean_weight_plot <- read.csv("data/003/mean_weights.csv")
mean_weight_plot$cluster <- as.character(mean_weight_plot$cluster)

ggplot(data = mean_weight_plot, aes(x = cluster, y = mean_weight, fill = source)) + 
geom_col(position = "dodge")  
ggsave("data/003/cluster_weight.pdf", width = 16, height = 9, dpi = 100) 

ggplot(mean_weight_plot, aes(x = cluster, y = mean_weight, fill = source)) + geom_col(position = "dodge") + facet_wrap("cell_type")
ggsave("data/003/cluster_weight_facet.pdf", width = 16, height = 9, dpi = 100)

#without hepatocytes
mean_weight_no_hep <- mean_weight_plot %>% filter(cell_type != c("Hepatocyte"))

ggplot(data = mean_weight_no_hep, aes(x = cluster, y = mean_weight, fill = source))  +
  geom_col(position = "dodge")
ggsave("data/003/cluster_weight_no_hep.pdf", width = 16, height = 9, dpi = 100)


#####testing for featureplots
SpatialDimPlot(myRCTD_c1, label = TRUE, label.size = 3)


###morem testing

results <- results_lst[[1]]
norm_weights_c1 = normalize_weights(results$weights) #normalise the cell type proportions to sum to 1
norm_weights_c1 <- as.matrix(norm_weights_c1)

results <- results_lst[[2]]
norm_weights_c2 = normalize_weights(results$weights) #normalise the cell type proportions to sum to 1
norm_weights_c2 <- as.matrix(norm_weights_c2)

results <- results_lst[[3]]
norm_weights_s1 = normalize_weights(results$weights) #normalise the cell type proportions to sum to 1
norm_weights_s1 <- as.matrix(norm_weights_s1)

results <- results_lst[[4]]
norm_weights_s2 = normalize_weights(results$weights) #normalise the cell type proportions to sum to 1
norm_weights_s2 <- as.matrix(norm_weights_s2)

norm_weights_all <-  rbind(norm_weights_c1, norm_weights_c2, norm_weights_s1, norm_weights_s2)

##### put into seurat object #####
shamcerspatial <- readRDS("data/002/RDS/shamcerspatial.rds")
shamcerspatial$weights <- norm_weights_all[,10] # [,num] ensures a column is taken, [num,] ensures a row is taken
SpatialFeaturePlot(shamcerspatial, features = "weights") &  scale_fill_continuous(type = "viridis", limits = c(min,max))  
 
ggsave("figures/003/SpatialFeaturePlot_11", width = 16, height = 9, dpi = 100)

min <- 0
max <- max(norm_weights_all[,10]) #note cluster names 0:17, but column numbers are 1:18
max

for(i in 1:18) { 
  min <- 0
  max <- max(norm_weights_all[,i])
  shamcerspatial$weights <- norm_weights_all[,i] # [,num] ensures a column is taken, [num,] ensures a row is taken
  SpatialFeaturePlot(shamcerspatial, features = "weights") &  scale_fill_continuous(type = "viridis", limits = c(min,max))
  ggsave(paste0("figures/003/SpatialFeaturePlot/SpatialFeaturePlot_Cluster",i-1,".pdf"), width = 16, height = 9, dpi = 100)
}

".pdf"


ggsave(paste0("figures/002/AlbCyp2e1/FeaturePlot",myfeatures[i],".pdf"), width = 16, height = 9, dpi = 100)
}