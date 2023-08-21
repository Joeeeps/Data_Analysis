#### read libraries ####
setwd("C:/Users/guest_joe/Desktop/01_Spatial Analysis Project")
library(here)
library(Seurat)
library(SeuratData) 
library(tidyverse)
library(harmony)

#### define directories ####
DIR_PROJECT <- paste0(here(),"/")
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")

DIR_FIG_OUT  <- paste0(DIR_FIG,"002/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"002/")

if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)

#### Start analysis ####

#### Load in 10X Spatial datasets #### 
sham1.data <- Load10X_Spatial('rawdata/002/A1_Sham1/')
cancer1.data <- Load10X_Spatial('rawdata/002/C1_4T1_1/') 
sham2.data <- Load10X_Spatial('rawdata/002/B1_Sham2/')
cancer2.data <- Load10X_Spatial('rawdata/002/D1_4T1_2/')

#### Adding in columns to identify both sample and type 
sham1.data$sample <- "sham1"
sham2.data$sample <- "sham2"
cancer1.data$sample <- "cancer1"
cancer2.data$sample <- "cancer2"

sham1.data$type <- "sham"
sham2.data$type <- "sham"
cancer1.data$type <- "cancer"
cancer2.data$type <- "cancer"

#### Merge the datasets ####
shamcerboth_spatial <- merge(sham1.data, y = c(sham2.data, cancer1.data, cancer2.data), add.cell.ids = c("sham1", "sham2", "cancer1", "cancer2"))
saveRDS(shamcerboth_spatial, file = "data/002/RDS/shamcer_sp_orig.rds")
shamcerboth_spatial <- SCTransform(shamcerboth_spatial, assay = "Spatial", verbose = FALSE)  

#### Feature Plot test ####
SpatialFeaturePlot(shamcerboth_spatial, features = c("Lrrk2")) 

#### Dimensionality reduction and clustering #### 
shamcerboth_spatial <- RunPCA(shamcerboth_spatial, assay = "SCT", verbose = FALSE) #Single Cell?
shamcerboth_spatial <- FindNeighbors(shamcerboth_spatial, reduction = "pca", dims = 1:30)
shamcerboth_spatial <- FindClusters(shamcerboth_spatial, verbose = FALSE)
shamcerboth_spatial <- RunUMAP(shamcerboth_spatial, reduction = "pca", dims = 1:30)  

p1 <- DimPlot(shamcerboth_spatial, reduction = "umap", group.by = "sample", label = TRUE)
p1
ggsave("figures/002/DimPlots/DimPlot.png")
p2 <- SpatialDimPlot(shamcerboth_spatial, label = TRUE, label.size = 3)
p2
ggsave("figures/002/DimPlots/SpatialDimplot.png", width = 16, height = 9, dpi = 100 )
p1+p2


#Plot UMAP next to dimensions graph

#### Using Harmony to reduce batch effects ####

#### Batch effects reduced for samples ####
shamcerboth_harmony_sample <- RunHarmony(shamcerboth_spatial, group.by.vars = "sample")
shamcerboth_harmony_sample <- RunUMAP(shamcerboth_harmony_sample, reduction = "harmony", dims = 1:30)
shamcerboth_harmony_sample <- FindNeighbors(shamcerboth_harmony_sample, reduction = "harmony")  %>% FindClusters(resolution = 0.6)
umapsampleharmony_sample <- DimPlot(shamcerboth_harmony_sample, reduction = "umap", group.by = "sample", label = TRUE)  
umapsampleharmony_sample
ggsave("figures/002/DimPlots/DimPlotSampleReduction.pdf", width = 16, height = 9, dpi = 100)

#### Batch effects reduced between cancer/sham types
shamcerboth_harmony_type <- RunHarmony(shamcerboth_spatial, group.by.vars = "type")
shamcerboth_harmony_type <- RunUMAP(shamcerboth_harmony_type, reduction = "harmony", dims = 1:30)
shamcerboth_harmony_type <- FindNeighbors(shamcerboth_harmony_type, reduction = "harmony")  %>% FindClusters(resolution = 0.6)
umaptypeharmony_type <- DimPlot(shamcerboth_harmony_type, reduction = "umap", label = TRUE, group.by = "type") 
umaptypeharmony_type
ggsave("figures/002/DimPlots/DimPlotTypeReduction.pdf", width = 16, height = 9, dpi = 100)

#### Assessing Cyp2e1 and Alb expression ####
myfeatures <- c("Alb", "Cyp2e1") 
#Gene expression of Cyp2e1 and Alb#
for (i in 1:2) {
FeaturePlot(shamcerboth_harmony_sample, reduction = "umap", split.by = "sample", features = myfeatures[[i]]) & 
              scale_color_gradient(high =  "red", low = "green")
 
ggsave(paste0("figures/002/AlbCyp2e1/FeaturePlot",myfeatures[i],".pdf"), width = 16, height = 9, dpi = 100)
}

p1 <- FeaturePlot(shamcerboth_harmony_sample, reduction = "umap", features = myfeatures[[1]],
            cols = c("green", "red"))  

p2 <- FeaturePlot(shamcerboth_harmony_sample, reduction = "umap", features = myfeatures[[2]],
                  cols = c("green", "red"))  

p1+p2
# with harmony reduction
for (i in 1:2) {
  FeaturePlot(shamcerboth_harmony_sample, reduction = "harmony", split.by = "sample", features = myfeatures[[i]]) & 
    scale_color_gradient(high =  "red", low = "green")
  
  ggsave(paste0("figures/002/AlbCyp2e1/FeaturePlot",myfeatures[i],"Harmony.pdf"), width = 16, height = 9, dpi = 100)
}

# with PCA reduction
for (i in 1:2) {
  FeaturePlot(shamcerboth_harmony_sample, reduction = "pca", features = myfeatures[[i]]) + 
    scale_color_gradient(high =  "red", low = "green")
  
  ggsave(paste0("figures/002/AlbCyp2e1/FeaturePlot",myfeatures[i],"PCA.pdf"), width = 16, height = 9, dpi = 100)
}
 
#Filter test
AlbCyp2e1_filter <- shamcerboth_harmony_sample[grepl("^Alb$|Cyp2e1", rownames(shamcerboth_harmony_sample)), ]
FeaturePlot(AlbCyp2e1_filter, reduction = "umap", label = TRUE, features = c("Alb","Cyp2e1")) + 
  scale_color_gradient(high =  "red", low = "green")
            

saveRDS(shamcerboth_spatial, file = "data/002/RDS/shamcerspatial.rds")
shamcerspatial <- readRDS("data/002/RDS/shamcerspatial.rds")



#####dim plot testing #####
#saveRDS(shamcerboth_harmony_sample, file = "data/002/RDS/shamcerboth_harmony_sample.rds")
shamcerboth_harmony_sample <- readRDS("data/002/RDS/shamcerboth_harmony_sample.rds")
SpatialDimPlot(shamcerboth_harmony_sample,  label = TRUE)

SpatialDim(shamcerboth_harmony_sample, reduction = "umap", idents = "0,1", features = myfeatures[[i]]) & 
  scale_color_gradient(high =  "red", low = "green")


ggsave("figures/002/DimPlots/SpatialDimplotHarmonySample.pdf", width = 16, height = 9, dpi = 100)

ISpatialDimPlot(shamcerboth_harmony_sample, )

#####260423
SpatialDimPlot(cancer1.data, cells.highlight = CellsByIdentities(object = shamcerboth_spatial,
                                                               idents = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)), facet.highlight = TRUE, ncol = 3)
ggsave("figures/002/SpatialDimPlots/cancer1.pdf")
SpatialDimPlot(cancer2.data, cells.highlight = CellsByIdentities(object = shamcerboth_spatial,
                                                               idents = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)), facet.highlight = TRUE, ncol = 3)
ggsave("figures/002/SpatialDimPlots/cancer2.pdf")
SpatialDimPlot(sham1.data, cells.highlight = CellsByIdentities(object = shamcerboth_spatial,
                                                                        idents = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)), facet.highlight = TRUE, ncol = 3)
ggsave("figures/002/SpatialDimPlots/sham1.pdf")
SpatialDimPlot(sham2.data, cells.highlight = CellsByIdentities(object = shamcerboth_spatial,
                                                               idents = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)), facet.highlight = TRUE, ncol = 3)
ggsave("figures/002/SpatialDimPlots/sham2.pdf")

#https://satijalab.org/seurat/articles/spatial_vignette.html

shamcer_sp_reference <- readRDS("data/002/RDS/shamcer_sp_orig.rds") #this may be too processed

shamcer_sp_reference <- SCTransform(shamcer_sp_reference, ncells = 30000,verbose = FALSE) %>%
  RunPCA(verbose = FALSE) %>% 
  RunUMAP(dims = 1:30)
