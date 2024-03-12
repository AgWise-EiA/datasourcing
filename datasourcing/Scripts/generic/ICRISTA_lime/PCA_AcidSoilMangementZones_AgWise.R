library(geodata)
library(terra)
library(fastcluster)
library(ade4)
library(factoextra)
#setwd("C:/Users/MPatil/OneDrive - ICRISAT/_ICRISAT/EiA")


ph<-terra::rast('ph_eth.tiff')
soc<-terra::rast('soc_eth.tiff')
p<-terra::rast('exP_eth.tiff')
exAcid<-terra::rast('acidEx_eth.tiff')
ca<-terra::rast('caEx_eth.tiff')
ecec<-terra::rast('ecec_eth.tiff')
texture<-terra::rast('texture_eth.tiff')


# mask by ph
layers <- c(p, ph, exAcid, ca, texture, soc, ecec) 
names(layers)<-c('p','ph','exAcid','ca','texture','soc','ecec')

ph_low <- terra::ifel(layers$ph < 6, 1, NA)
layers <- layers * ph_low

# log-transform data
stacked <- layers
stacked <- terra::subst(stacked, 0, NA)
g <- terra::global(stacked, min, na.rm=T)$min
stacked <- log10(stacked + 0.5 * g)

# sample 10,000 random grid cells
set.seed(123)
sr <- terra::spatSample(stacked, min(10000, terra::ncell(stacked)), method="regular", na.rm=F, as.raster=F, as.df=T, xy=T)
sr <- na.omit(sr)


# run pca 

names(sr) 

pca <- dudi.pca(sr[,c(3:9)], scannf=F, nf=4)
(eig.val <- data.frame(eigenvalue=c(pca$eig)))
(exp.var <- c(cumsum(pca$eig) / sum(pca$eig)))
(loading <- pca$co)
fviz_pca_var(pca, col.var="contrib") + 
  scale_color_gradient2(low="blue", mid="purple", high="red", midpoint=13) +
  theme_minimal() 

pc_rast <- terra::predict(stacked, pca, cpkgs="FactoMineR")


names(pc_rast) <- c('pc1', 'pc2', 'pc3', 'pc4')
terra::plot(pc_rast, main="")


# keep only principal components with eigenvalues > 1
pc_final <- subset(eig.val, eigenvalue > 1)


# ------------------------------------------------------------------------------
# hierarchical clustering
# ------------------------------------------------------------------------------

# select principal components with eigenvalue above 1
pc_rast <- pc_rast[[1:nrow(pc_final)]]
g <- terra::global(pc_rast, mean, na.rm=T)$mean + 2 * terra::global(pc_rast, IQR, na.rm=T)$global
pc_rast <- terra::clamp(pc_rast, -Inf, g)

# hierarchical clustering
v <- data.frame(terra::values(pc_rast))
v<-na.omit(v)
v_un <- unique(round(v, 1),rm.na=T)
v_na <- na.omit(v_un)

distances <- dist(v_na)
hclust_r <- hclust(distances) 
hclust_dendo <- as.dendrogram(hclust_r)
plot(hclust_dendo, type="rectangle", ylab="Height", leaflab="none")
hclust_cluster <- cutree(hclust_r, k=4)

# get clusters
v_na$cluster <- hclust_cluster
v_na$id <- row.names(v_na)

# back to spat raster
pc_rast_r <- round(pc_rast, 1)
pc_rast_r_clust <- terra::subst(pc_rast_r, v_na[,1:3], v_na[,4])
names(pc_rast_r_clust) <- 'cluster'

terra::plot(pc_rast_r_clust$cluster)
terra::writeRaster(pc_rast_r_clust, "./acid-soil-clusters.tiff", overwrite=T)

# make plot
png('ea-map.png', unit='in', width=8.5, height=8.5, res=500)
terra::plot(pc_rast_r_clust$cluster, col=c('forestgreen', 'gold', 'orange', 'orangered'))
terra::plot(eth, border='black', axes=F, add=T)
dev.off()

# ------------------------------------------------------------------------------
# cluster characterization 
# ------------------------------------------------------------------------------

# dataframes
all <- c(pc_rast_r_clust, layers)
all_df <- terra::as.data.frame(all)

# plot
png('ea-boxplot.png', unit='in', width=12, height=7, res=500)
par(mfrow=c(2,4), xaxs='i', yaxs='i', mar=c(3,5,1,1), cex.axis=1.4, cex.lab=1.6)
pal <- colorRampPalette(c('forestgreen', 'gold', 'orange', 'orangered'))
boxplot(all_df$p ~ all_df$cluster, ylab='p', xlab='', col=pal(4), ylim=c(0,3000))
boxplot(all_df$ph ~ all_df$cluster, ylab='ph', xlab='', col=pal(4), ylim=c(4,7.5))
boxplot(all_df$exAcid ~ all_df$cluster, ylab='ExAcid', xlab='', col=pal(4), ylim=c(0,6))
boxplot(all_df$ca ~ all_df$cluster, ylab='ca', xlab='', col=pal(4), ylim=c(0,20))
boxplot(all_df$texture ~ all_df$cluster, ylab='clay+silt', xlab='', col=pal(4), ylim=c(0,100))
boxplot(all_df$soc ~ all_df$cluster, ylab='soc', xlab='', col=pal(4), ylim=c(0,60))
boxplot(all_df$ecec ~ all_df$cluster, ylab='ecec', xlab='', col=pal(4), ylim=c(0,40))
dev.off()

# ------------------------------------------------------------------------------
# random forest
# ------------------------------------------------------------------------------

pc_rast <- terra::rast("./acid-soil-clusters.tiff")
all <- c(pc_rast, layers)
set.seed(123)
data <- terra::spatSample(all, min(100000, terra::ncell(all)), method="regular", na.rm=F, as.raster=F, as.df=T, xy=T)
data <- na.omit(data)
all_df <- data[-c(1:2)]
names(all_df)
all_df <- na.omit(all_df)
all_df <- all_df[c("cluster", "p", "ph", "exAcid", "ecec","soc","texture")]

rf <- randomForest::randomForest(as.factor(cluster)~., all_df)
rf
randomForest::varImpPlot(rf, )

cart <- rpart::rpart(as.factor(cluster)~., all_df)
rpart::plotcp(cart)
cart_opt = rpart::rpart(as.factor(cluster)~., all_df, cp=0.015)
rpart.plot::rpart.plot(cart_opt, type=2, extra="auto", round=0, under=T, box.palette="BlGnYl")
