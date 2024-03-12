library(randomForest)
library(raster)
library(snow)
library(terra)
library(sp)
setwd("C:/Users/MPatil/OneDrive - ICRISAT/_ICRISAT/EiA")

data<-read.csv("LimeTrialsData.csv")
#Data template file description
##author - name author or citation or data source
##year - Year of data 
##location - name of the location
##y - latitude in degree decimel
##x - longitude in degree decimel
##lime - rate of lime application (tons/ha)
##p2 - P fertilizer rate (kg/ha)
##n - N fertilizer rate (kg/ha)
##fym -Farm yard manure rate (tons/ha)
##compost - Compost application rate (tons/ha)
##biochar Biochar application rate (tons/ha)
##ph2 - pH in water afpter liming experiement
##exacid2 - exchangeable acidity after lime application
##yield - grain yield (kg/ha)
##dph - change in ph due to lime application
##dacid - percent change in exchangeable acidity due to liming 

data<-data[which(data$y>0),4:19]
data<-data[which(data$x>0),]
data<-data[which(data$lime>0),] #data points with lime application
##Data points with zero application of organics
data<-data[which(data$fym==0),]
data<-data[which(data$compost==0),]
data<-data[which(data$biochar==0),]


dataph<-data[,c(2:3,6,15)]#Data for model based on ph
dataexacid<-data[,c(2:3,6,16)]#Data for model based on Ex. acidity


#Locading the rasters for predition of lime rates
country <- geodata::world(path='.', resolution=5, level=0)
isocodes <- geodata::country_codes()
isocodes_eth<-subset(isocodes,NAME=='Ethiopia')
eth <- subset(country, country$GID_0 %in% isocodes_eth$ISO3)

ph5.5<-terra::rast('ph_cropland_final.tiff')

ph<-terra::rast('ph_eth.tiff')
soc<-terra::rast('soc_eth.tiff')
exP<-terra::rast('exP_eth.tiff')
cec<-terra::rast('cec_eth.tiff')
acidEx<-terra::rast('acidEx_eth.tiff')
texture<-terra::rast('texture_eth.tiff')
tmin<-terra::rast('tmin_eth.tif')
tmax<-terra::rast('tmax_eth.tif')
rainfall<-terra::rast('rainfall_eth.tif')



## Lime recommendation based on change in Exchangeable acidity
layers<-c(ph,texture,tmin,tmax,rainfall,acidEx,cec,soc,exP)
names(layers)<-c('ph1','texture','tmin','tmax','rainfall','ExAcid','CEC','OC','P')
dataModel<-extract(layers,dataexacid[,c('x','y')])
dataModel<-cbind(dataexacid,dataModel)
dataModel<-dataModel[,c(1:4,6:14)]
dataModel<-dataModel[complete.cases(dataModel), ]


set.seed(1234)
training<-sample(nrow(dataModel),0.8*nrow(dataModel))
train<-dataModel[training,]
test<-dataModel[-training,]

# Random forest model with lime rate as target variable
fit_rf<-randomForest(log(lime)~ph1+dacid+texture+tmin+tmax+rainfall+ExAcid+CEC+OC+P,data=train,importance=TRUE,ntree=500)
# Predicting the lime rate for testing data 
pred_rf_tst<-predict(fit_rf, newdata = test)

plot(test$lime,exp(pred_rf_tst))
abline(a=1,b=1)
R2(test$lime,exp(pred_rf_tst))
RMSE(test$lime,exp(pred_rf_tst))


set.seed(123)
sr <- terra::spatSample(layers, min(1000, terra::ncell(layers)), method="regular", na.rm=F, as.raster=F, as.df=T, xy=T)
sr <- na.omit(sr)

lime.summary<-matrix(NA,nrow=10,ncol = 6)
colnames(lime.summary)<-c('Min','Q1','Median','Mean','Q3','Max')
#rownames(lime.summary)<-c('0.5','1','1.5','2','2.5','3','3.5','4','4.5','5')
rownames(lime.summary)<-c('0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1')
for(i in 1:10){
  sr$dacid=(i/10)*sr$ExAcid
  lime.req<-exp(predict(fit_rf,sr))
  lime.summary[i,]<-summary(lime.req)
  
}

#Predict lime requirement map
dacid<-acidEx*0.5 #create new raster for change in Ex Acidity (reduce the Ex Acidity by 50%)
names(dacid)<-"dacid"
layers<-c(layers,dacid)
lime.map<-predict(layers,fit_rf)
lime.map<-exp(lime.map)

#Crop map to the area with soil pH less than 5.5
lime.map<-terra::crop(lime.map,ph5.5,mask=T)
terra::plot(eth);terra::plot(lime.map,add=T)
writeRaster(lime.map,filename = 'limeRequirement_ExAcidity.tif',overwrite=T)




## Lime recommendation based on change in pH
layers<-c(ph,texture,tmin,tmax,rainfall,acidEx,cec,soc,exP)
names(layers)<-c('ph1','texture','tmin','tmax','rainfall','ExAcid','CEC','OC','P')


dataModel<-extract(layers,dataph[,c('x','y')])
dataModel<-cbind(dataph,dataModel)
dataModel<-dataModel[,c(1:4,6:14)]
dataModel<-dataModel[complete.cases(dataModel), ]


set.seed(1234)
training<-sample(nrow(dataModel),0.8*nrow(dataModel))
train<-dataModel[training,]
test<-dataModel[-training,]

# Random forest model with lime rate as target variable
fit_rf<-randomForest(log(lime)~ph1+dph+texture+tmin+tmax+rainfall+ExAcid+CEC+OC+P,data=train,importance=TRUE,ntree=500)
# Predicting the lime rate for testing data 
pred_rf_tst<-predict(fit_rf, newdata = test)

plot(test$lime,exp(pred_rf_tst))
abline(a=1,b=1)
R2(test$lime,exp(pred_rf_tst))
RMSE(test$lime,exp(pred_rf_tst))


set.seed(123)
sr <- terra::spatSample(layers, min(1000, terra::ncell(layers)), method="regular", na.rm=F, as.raster=F, as.df=T, xy=T)
sr <- na.omit(sr)

lime.summary<-matrix(NA,nrow=10,ncol = 6)
colnames(lime.summary)<-c('Min','Q1','Median','Mean','Q3','Max')
#rownames(lime.summary)<-c('0.5','1','1.5','2','2.5','3','3.5','4','4.5','5')
rownames(lime.summary)<-c('0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9','1')
for(i in 1:10){
  sr$dph=i/10
  lime.req<-exp(predict(fit_rf,sr))
  lime.summary[i,]<-summary(lime.req)
  
}

#Predict lime requirement map
dph<-ph*0+.5 #create new raster for dph (increasing pH by 0.5)
names(dph)<-"dph"
layers<-c(layers,dph)
lime.map<-predict(layers,fit_rf)
lime.map<-exp(lime.map)

#Crop map to the area with soil pH less than 5.5

lime.map<-terra::crop(lime.map,ph5.5,mask=T)
terra::plot(eth);terra::plot(lime.map,add=T)
writeRaster(lime.map,filename = 'limeRequirement_pH.tif',overwrite=T)






lime_exacid<-terra::rast('limeRequirement_ExAcidity.tif')
lime_ph<-terra::rast('limeRequirement_pH.tif')


lime_exacid<-terra::crop(lime_exacid,ph5.5,mask=T)
lime_ph<-terra::crop(lime_ph,ph5.5,mask=T)

png('eth-lime.png', unit='in', width=12,height=5, res=500)
par(mfrow=c(1,2))
terra::plot(lime_exacid,main="Lime (t/ha) based on Ex Acidity");terra::plot(eth, add=T)
terra::plot(lime_ph,main="Lime (t/ha) based on pH");terra::plot(eth, add=T)
dev.off()
