#### read libraries ####
library(here)
library(tidyverse)
library(harmony)
library(Seurat)
library(SeuratData) 
 #### define directories ####
DIR_PROJECT <- paste0(here(),"/") 
DIR_RAW   	<- paste0(DIR_PROJECT,"rawdata/")
DIR_DAT   	<- paste0(DIR_PROJECT,"data/")
DIR_FIG   	<- paste0(DIR_PROJECT,"figures/")

#here package lets you create a template of directories to use in the code
DIR_FIG_OUT  <- paste0(DIR_FIG,"001/")
DIR_DAT_OUT  <- paste0(DIR_DAT,"001/")

if(!dir.exists(DIR_FIG_OUT)) dir.create(DIR_FIG_OUT)
if(!dir.exists(DIR_DAT_OUT)) dir.create(DIR_DAT_OUT)
#creating specific directory sections for different codes just by changing 001 to 002 etc.

#### Start analysis ####

#The dataset used looks at four different mouse livers, of cancer infected (cancer1, cancer2) and non-infected (sham1, sham2) mice
#This script aims to assess the variance in gene frequency between samples, as well as different gene clusters
#Begin by loading the datasets, provided by 10X genomics

sham1.data <- Read10X('rawdata/001/sham1/filtered_feature_bc_matrix')
cancer1.data <- Read10X('rawdata/001/cancer1/filtered_feature_bc_matrix')
sham2.data <- Read10X('rawdata/001/sham2/filtered_feature_bc_matrix')
cancer2.data <- Read10X('rawdata/001/cancer2/filtered_feature_bc_matrix')
 
#### Creating Seurat objects ####
#Seurat objects are a representation of single-cell expression data for R

#some QC to remove cells with too small samples 
sham1 <- CreateSeuratObject(counts = sham1.data, project = "sham1", min.cells = 3, min.features = 200)
cancer1 <- CreateSeuratObject(counts = cancer1.data, project = "cancer1", min.cells = 3, min.features = 200)
sham2 <- CreateSeuratObject(counts = sham2.data, project = "sham2", min.cells = 3, min.features = 200)
cancer2 <- CreateSeuratObject(counts = cancer2.data, project = "cancer2", min.cells = 3, min.features = 200)

#### Merging datasets ####

#^mt- refers to Mus musculus mitochondrial DNA
#Filtering small cells using subsetting, cells with high mitochondria (indicator of damage) and large cells (indicator of cell clumping) 
shamcerboth <-　merge(sham1, y = c(sham2, cancer1, cancer2), add.cell.ids = c("sham1", "sham2", "cancer1", "cancer2"), project = "SHAMCERBOTH")
shamcerboth[["percent.mt"]] <- PercentageFeatureSet(shamcerboth, pattern = "^mt-")  
shamcerboth <- subset(shamcerboth, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5) 

#### Overview of data (violin plot) ####
VlnPlot(shamcerboth, features = c("nFeature_RNA", "nCount_RNA", "percent.mt")) #sham1 looks low quality

#### Checking if features correlate with count ####
plot_countmt <- FeatureScatter(shamcerboth, feature1 = "nCount_RNA", feature2 = "percent.mt")
plot_countfeature <- FeatureScatter(shamcerboth, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
plot_countmt + plot_countfeature 
ggsave("figures/001/01_count_correlation.png", width = 16, height = 9, dpi = 100)

#### Identifying highly variable features, which are outliers on a 'mean variability plot' ####
shamcerboth <- FindVariableFeatures(shamcerboth, selection.method = "vst", nfeatures = 2000) #this adds a var.features section to the data

#### Making mean expression values of shamcer both to zero, and scaling expression so variance is 1 overall ####
all.genes <- rownames(shamcerboth)
shamcerboth <- ScaleData(shamcerboth, features = all.genes)

#### Creating a PCA plot ####
#PCA1 has most variability, followed by PCA2 etc. scaling data prevents a particular sample dominating
shamcerboth <- NormalizeData(shamcerboth)
shamcerboth <- RunPCA(shamcerboth, features = VariableFeatures(object = shamcerboth))
print(shamcerboth[["pca"]], dims = 1:5, nfeatures = 5)

#### Using a Heatmap to check where variability of each PC lies #### 
DimHeatmap(shamcerboth, dims = 1:2, cells = 500, balanced = TRUE) #some reason I can't save the heatmap automatically

#### Using a JackStraw plot to plot clusters by significant differences ####
shamcerboth <- JackStraw(shamcerboth, num.replicate = 100)
shamcerboth <- ScoreJackStraw(shamcerboth, dims = 1:20)
JackStrawPlot(shamcerboth, dims = 1:15) #see 15 clusters with significant differences

#### Identify clusters ####
shamcerboth <- FindNeighbors(shamcerboth, dims = 1:10) #Find closest neighbours by k.param
shamcerboth <- FindClusters(shamcerboth, resolution = 0.5) #Identify clusters of cells by shared nearest neighbor algorithm
head(Idents(shamcerboth), 5) #Now we can look at clusterIDs if needed

#### Using UMAP to put similar cells together in low-dimensional space #### 
shamcerboth <- RunUMAP(shamcerboth, dims = 1:10)

#### DimPlot to visualise clusters, and the cluster distribution for each sample ####
DimPlot(shamcerboth, reduction = "umap", label = TRUE, split.by = "orig.ident") 
ggsave("figures/001/03_UMAP.png", width = 16, height = 9, dpi = 100)

#### Clusterprofiler ####
library("clusterProfiler") # Package for better visualisation of gene data
library(org.Mm.eg.db) # Mus musculus dataset

#### Find marker genes (DEGs) for identity classes #### 
for (i in 0:17){
  message(i) 
  res <- FindMarkers(shamcerboth, ident.1 = i, min.pct = 0.25) 
  write.csv(res, paste0('data/001/clusterid/clustermarkers/clustermarkers_',i,'.csv'))
} 

#### Convert symbol IDs to ENTREZ ID ####
symbol.universe <- rownames(shamcerboth)
entrez.universe <- mapIds(org.Mm.eg.db, keys=symbol.universe, column='ENTREZID', keytype='SYMBOL')
entrez.universe <- unique(entrez.universe)

#### Gene ontology - adds descriptions for each gene ####
for (i in 0:17){
  message(i)
  clustermarkers <- read.csv(paste0('data/001/clusterid/clustermarkers/clustermarkers_',i,'.csv'))
  clustermarkersfilter <- clustermarkers %>% filter((p_val_adj <= 0.05)) 
  clustermarkersfilter <- clustermarkersfilter %>% filter((avg_log2FC > 0))
  symbolID <- unlist(clustermarkersfilter[1])
  ENTREZID <- mapIds(org.Mm.eg.db, keys=symbolID, column='ENTREZID', keytype='SYMBOL')
  ego <- enrichGO(gene          = ENTREZID, #use loop to change gene name 
                  universe      = entrez.universe,
                  OrgDb         = org.Mm.eg.db,
                  ont           = "BP",
                  pAdjustMethod = "BH",
                  pvalueCutoff  = 0.01,
                  qvalueCutoff  = 0.05,
                  readable      = TRUE)
  write.csv(ego, paste0('data/001/clusterid/GOresult/GOresult_cluster_',i,'.csv'))
}

#merge using excel function then read all data into one datafrmae
Cluster_merge <- read.csv('data/001/clusterid/GOresult/GOresult_merge.csv')

#### Specifying marker genes and checking their regional expression value ####
markergenes <- read.csv('rawdata/001/marker_genes.csv') #read marker gene list
markergenes_type <- unique(markergenes$cell_type) #how many unique gene types are there?
 
for (i in 1:30){
  message(i)
  target.genes <- markergenes %>% filter(cell_type == markergenes_type[i]) 
  target.genes <- target.genes$gene
  shamcerboth <- AddModuleScore(
    shamcerboth, list(target.genes[]), 
    name = "Cluster_"
  )
  FeaturePlot(shamcerboth, features = 'Cluster_1', label = TRUE) +  #add name and change colour scheme to make differences easier to see 
    scale_color_gradient2(low = "grey", high = "red") + 
    ggtitle(paste0(markergenes_type[i]))
  
  ggsave(paste0("figures/001/targetgene/",markergenes_type[i],".png"), width = 16, height = 9, dpi = 100)
  
} 

#usingsplitby

for (i in 1:30){
  message(i)
  target.genes <- markergenes %>% filter(cell_type == markergenes_type[i]) 
  target.genes <- target.genes$gene
  shamcerboth <- AddModuleScore(
    shamcerboth, list(target.genes[]), 
    name = "Cluster_"
  )
  FeaturePlot(shamcerboth, features = 'Cluster_1', label = TRUE, split.by =  "orig.ident") + #add name and change colour scheme to make differences easier to see 
    ggtitle(paste0(markergenes_type[i]))
  
  ggsave(paste0("figures/001/targetgene/split/",markergenes_type[i],"split.png"), width = 16, height = 9, dpi = 100)
  
} 
#saveRDS(shamcerboth, file = "data/001/RDS/shamcerboth")
shamcerboth <- readRDS("data/001/RDS/shamcerboth.rds")


#takes 11 minutes to run script fully

#Assigning cell type identity to clusters
new.cluster.ids <- c("Neutrophil", "NA", "T Cell", "Neutrophil", "NA", "T Cell", "NA", "NA", "NA", "NA", "Hepatocyte", "Hepatocyte", "T Cell", "Neutrophil", "Hepatocyte", "NA", "Macrophage", "T Cell")
names(new.cluster.ids) <- levels(shamcerboth)
shamcerboth <- RenameIdents(shamcerboth, new.cluster.ids) #if doesn't work reload shamcerboth code
DimPlot(shamcerboth, reduction = "umap", label = TRUE, pt.size = 0.5) #add split to split by sample groups
ggsave("figures/001/04_UMAP_ident.pdf", width = 16, height = 9, dpi = 100)

#saveRDS(shamcerboth, file = "data/001/RDS/shamcerboth_cell_rename.rds")
