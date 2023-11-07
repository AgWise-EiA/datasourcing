#summarize_pointdata(rastLayerRF_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1981.nc",rastLayerRF_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1982.nc",
                          # gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)), pl_j=35, hv_j=150, planting_harvest_sameYear = TRUE)
library(tidyverse)
gpsdata=data.frame(longitude = c(30.838409, 31.053028), latitude = c(-20.079191, -17.824858))
pl_j=260; hv_j=152; planting_harvest_sameYear = FALSE
year = 1991
harvest_year = year + 1

rastLayerRF_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1981.nc"
rastLayerRF_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1982.nc"
rastLayerTmax_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMax/AgEra/1981.nc"
rastLayerTmax_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMax/AgEra/1982.nc"
rastLayerTmin_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMin/AgEra/1981.nc"
rastLayerTmin_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMin/AgEra/1982.nc"
rastLayerRH_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/RelativeHumidity/AgEra/1981.nc"
rastLayerRH_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/RelativeHumidity/AgEra/1982.nc"
rastLayerSR_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/SolarRadiation/AgEra/1981.nc"
rastLayerSR_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/SolarRadiation/AgEra/1982.nc"
rastLayerWS_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/WindSpeed/AgEra/1981.nc"
rastLayerWS_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/WindSpeed/AgEra/1982.nc"

# Semi automated input
gpsdata=data.frame(longitude = c(30.838409, 31.053028, 31.92437), latitude = c(-20.079191, -17.824858,-14.40447))
pl_j=260; hv_j=152; planting_harvest_sameYear = FALSE
year = 2013
harvest_year = year + 1
rastLayerRF_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/", year,".nc")
rastLayerRF_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/", harvest_year,".nc")
rastLayerTmax_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMax/AgEra/",year,".nc")
rastLayerTmax_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMax/AgEra/",harvest_year,".nc")
rastLayerTmin_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMin/AgEra/",year,".nc")
rastLayerTmin_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMin/AgEra/",harvest_year,".nc")
rastLayerRH_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/RelativeHumidity/AgEra/",year,".nc")
rastLayerRH_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/RelativeHumidity/AgEra/",harvest_year,".nc")
rastLayerSR_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/SolarRadiation/AgEra/",year,".nc")
rastLayerSR_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/SolarRadiation/AgEra/",harvest_year,".nc")
rastLayerWS_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/WindSpeed/AgEra/",year,".nc")
rastLayerWS_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/WindSpeed/AgEra/",harvest_year,".nc")

test <- summarize_pointdata(rastLayerRF_1=rastLayerRF_1,rastLayerRF_2=rastLayerRF_2,
                    rastLayerTmax_1 = rastLayerTmax_1, rastLayerTmax_2 = rastLayerTmax_2, gpsdata=gpsdata, pl_j=pl_j, hv_j=hv_j, planting_harvest_sameYear = planting_harvest_sameYear, rastLayerTmin_1 = rastLayerTmin_1, rastLayerTmin_2= rastLayerTmin_2,
                    rastLayerRH_1= rastLayerRH_1, rastLayerRH_2= rastLayerRH_2,
                    rastLayerSR_1= rastLayerSR_1, rastLayerSR_2= rastLayerSR_2,
                    rastLayerWS_1= rastLayerWS_1, rastLayerWS_2= rastLayerWS_2, year = year)

katete <- read.csv('~/transform-modelling/grid_Katete.csv')
colnames(katete) <- tolower(colnames(katete))

test_katete <- summarize_pointdata(rastLayerRF_1=rastLayerRF_1,rastLayerRF_2=rastLayerRF_2,
                            rastLayerTmax_1 = rastLayerTmax_1, rastLayerTmax_2 = rastLayerTmax_2, gpsdata=katete, pl_j=pl_j, hv_j=hv_j, planting_harvest_sameYear = planting_harvest_sameYear, rastLayerTmin_1 = rastLayerTmin_1, rastLayerTmin_2= rastLayerTmin_2,
                            rastLayerRH_1= rastLayerRH_1, rastLayerRH_2= rastLayerRH_2,
                            rastLayerSR_1= rastLayerSR_1, rastLayerSR_2= rastLayerSR_2,
                            rastLayerWS_1= rastLayerWS_1, rastLayerWS_2= rastLayerWS_2, year= year)

# Benue
benue <- read.csv("~/transform-modelling/Nigeria/NOT_Benue_EiA2022_Data_gps.csv")[,7:8]
colnames(benue) <- c('Latitude', 'Longitude')
colnames(benue) <- tolower(colnames(benue))
pl_j=120; hv_j=300; planting_harvest_sameYear = TRUE
year = 2022
harvest_year = year
rastLayerRF_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/", year,".nc")
rastLayerRF_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/", harvest_year,".nc")
rastLayerTmax_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMax/AgEra/",year,".nc")
rastLayerTmax_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMax/AgEra/",harvest_year,".nc")
rastLayerTmin_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMin/AgEra/",year,".nc")
rastLayerTmin_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMin/AgEra/",harvest_year,".nc")
rastLayerRH_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/RelativeHumidity/AgEra/",year,".nc")
rastLayerRH_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/RelativeHumidity/AgEra/",harvest_year,".nc")
rastLayerSR_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/SolarRadiation/AgEra/",year,".nc")
rastLayerSR_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/SolarRadiation/AgEra/",harvest_year,".nc")
rastLayerWS_1=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/WindSpeed/AgEra/",year,".nc")
rastLayerWS_2=paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/WindSpeed/AgEra/",harvest_year,".nc")

test_benue <- summarize_pointdata(rastLayerRF_1=rastLayerRF_1,rastLayerRF_2=rastLayerRF_2,
                                   rastLayerTmax_1 = rastLayerTmax_1, rastLayerTmax_2 = rastLayerTmax_2, gpsdata=benue, pl_j=pl_j, hv_j=hv_j, planting_harvest_sameYear = planting_harvest_sameYear, rastLayerTmin_1 = rastLayerTmin_1, rastLayerTmin_2= rastLayerTmin_2,
                                   rastLayerRH_1= rastLayerRH_1, rastLayerRH_2= rastLayerRH_2,
                                   rastLayerSR_1= rastLayerSR_1, rastLayerSR_2= rastLayerSR_2,
                                   rastLayerWS_1= rastLayerWS_1, rastLayerWS_2= rastLayerWS_2, year= year)


# 3. Function to get seasonal rainfall parameters for point data over the cropping season  -------------------------------------------
#' @description is a function to get total rainfall, number of rainy days and monthly rainfall, and working when the planting and harvest happen in different years
#' @param raster1 the .nc file for the planting year, within get_rf_pointdata function, this is provided by the function 
#' @param raster2 the .nc file for the harvest year, within get_rf_pointdata function, this is provided by the function 
#' @param gpsdata a data frame with longitude and latitude 
#' @param pl_j the planting date as the date of the year
#' @param hv_j the harvest date as the date of the year
#'
#' @return  a data frame with total rainfall, number of rainy days and monthly rainfall
#' @example summary_pointdata_rainfall(rastLayer1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1981.nc",
# raster2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1982.nc",
# gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)),  
# pl_j=35, hv_j=150, planting_harvest_sameYear = TRUE)
summarize_pointdata <- function(rastLayerRF_1=NULL, rastLayerRF_2=NULL, 
                                rastLayerTmax_1=NULL, rastLayerTmax_2=NULL,
                                rastLayerTmin_1=NULL, rastLayerTmin_2=NULL,
                                rastLayerRH_1=NULL, rastLayerRH_2=NULL,
                                rastLayerSR_1=NULL, rastLayerSR_2=NULL,
                                rastLayerWS_1=NULL, rastLayerWS_2=NULL,
                                gpsdata, pl_j, hv_j, planting_harvest_sameYear, year = NULL){
  
  # 3.1. Read the rainfall data and shape the ground data ####
  if(planting_harvest_sameYear == TRUE){
    PlHvD_RF <- terra::rast(rastLayerRF_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_Tmax <- terra::rast(rastLayerTmax_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_Tmin <- terra::rast(rastLayerTmin_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_RH <- terra::rast(rastLayerRH_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_SR <- terra::rast(rastLayerSR_1, lyrs=c(pl_j:hv_j)) 
    PlHvD_WS <- terra::rast(rastLayerWS_1, lyrs=c(pl_j:hv_j)) 
    
  }else{
    rastRF_i1 <- terra::rast(rastLayerRF_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerRF_1))))
    rastRF_i2 <- terra::rast(rastLayerRF_2, lyrs=c(1:hv_j))
    PlHvD_RF <- c(rastRF_i1, rastRF_i2)
    
    rastTmax_i1 <- terra::rast(rastLayerTmax_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerTmax_1))))
    rastTmax_i2 <- terra::rast(rastLayerTmax_2, lyrs=c(1:hv_j))
    PlHvD_Tmax <- c(rastTmax_i1, rastTmax_i2)
    
    rastTmin_i1 <- terra::rast(rastLayerTmin_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerTmin_1))))
    rastTmin_i2 <- terra::rast(rastLayerTmin_2, lyrs=c(1:hv_j))
    PlHvD_Tmin <- c(rastTmin_i1, rastTmin_i2)
    
    rastRH_i1 <- terra::rast(rastLayerRH_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerRH_1))))
    rastRH_i2 <- terra::rast(rastLayerRH_2, lyrs=c(1:hv_j))
    PlHvD_RH <- c(rastRH_i1, rastRH_i2)
    
    rastSR_i1 <- terra::rast(rastLayerSR_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerSR_1))))
    rastSR_i2 <- terra::rast(rastLayerSR_2, lyrs=c(1:hv_j))
    PlHvD_SR <- c(rastSR_i1, rastSR_i2)
    
    rastWS_i1 <- terra::rast(rastLayerWS_1, lyrs=c(pl_j:terra::nlyr(terra::rast(rastLayerWS_1))))
    rastWS_i2 <- terra::rast(rastLayerWS_2, lyrs=c(1:hv_j))
    PlHvD_WS <- c(rastWS_i1, rastWS_i2)
  }
  
  xy <- gpsdata[, c("longitude", "latitude")]
  
  RFi <- terra::extract(PlHvD_RF, xy, method='simple', cells=FALSE)
  RFi <- RFi[,-1]
  
  Tmaxi <- terra::extract(PlHvD_Tmax, xy, method='simple', cells=FALSE)
  Tmaxi <- Tmaxi[,-1]
  Tmaxi <- Tmaxi-274
  
  Tmini <- terra::extract(PlHvD_Tmin, xy, method='simple', cells=FALSE)
  Tmini <- Tmini[,-1]
  Tmini <- Tmini-274
  
  RHi <- terra::extract(PlHvD_RH, xy, method='simple', cells=FALSE)
  RHi <- RHi[,-1]
  
  SRi <- terra::extract(PlHvD_SR, xy, method='simple', cells=FALSE)
  SRi <- SRi[,-1]
  SRi <- SRi/1000000
  
  WSi <- terra::extract(PlHvD_WS, xy, method='simple', cells=FALSE)
  WSi <- WSi[,-1]
  
  
  
  # 3.2. Get the rainfall seasonal parameters at a location ####
  ## The total rainfall over the growing period
  
  rainiq <- t(RFi)
  gpsdata$totalRF <- colSums(rainiq)
  
  # Get rainy days only then get cessation date of rain
  rain_onset_cessation <- NULL
  for (m in 1:ncol(rainiq)){
    rainm <- as.vector(which(as.numeric(rainiq[,m]) > 2))
   
    # see the days when there was rain event of 2mm and above
    rain_r <- rainiq[rainm,m]
    rain_a <- as.numeric(rain_r)
    cumsum(rain_a)
    
    # get julian date
    day_r <- as.numeric(gsub("precip_","",names(rainiq[rainm,m])))
    
    diff <- diff(day_r)
    diff <- ifelse(diff<0, diff + 365, diff)
    diff <-cbind(day_r,c(diff,0),rain_a, round(cumsum(rain_a),1))

    # Get cumulative rainfall 
    onset <- which(diff[,4] > 25)[1]
    ifelse(max(diff[onset:(onset+5),2])>=20, rain_a <- rain_a[-c(1:(onset + which(max(day_r[onset:(onset+5)])>=20)))],rain_a <- rain_a)
    ifelse(max(diff[onset:(onset+5),2])>=20, day_r <- day_r[-c(1:onset + which(max(day_r[onset:(onset+5)])>=20))], day_r <- day_r)
    
    diff <- diff(day_r)
    diff <- ifelse(diff<0, diff + 365, diff)
    diff <-cbind(day_r,c(diff,0),c(rain_a,0), c(round(cumsum(rain_a),1),0))
    
    # Get cumulative rainfall 
    onset <- which(diff[,4] > 25)[1]
    ifelse(max(diff[onset:(onset+5),2])>=20, rain_a <- rain_a[-c(1:(onset + which(max(day_r[onset:(onset+5)])>=20)))],rain_a <- rain_a)
    ifelse(max(diff[onset:(onset+5),2])>=20, day_r <- day_r[-c(1:onset + which(max(day_r[onset:(onset+5)])>=20))], day_r <- day_r)
    
    onset_date_j <- as.numeric(gsub("precip_","",names(rain_r)[onset]))
    #onset_date_j <- diff[onset,1]
    
    # Check whether the difference between onset and cessation is greater than 50 days otherwise reset onset after the cessation evaluated from the first onset rainfall
    # Convert Julian date to calender date
    onset_date_c <- as.Date(onset_date_j, origin = as.Date(paste0(year,"-01-01")))
    
    #recreate diff after offsetting with onset
    diff <- diff[-c(1:onset),] 
    cess <- which(diff[,2] >= 20)[1]# Cessation date is when there is no rainfall event with the next 20 days
    ifelse(is.na(cess) == TRUE, cess <- which(diff[,2] == max(diff[,2])), cess <- cess)
    cessation <- names(rain_r[-c(1:onset)][cess])[1]
    cessation_j <-  as.numeric(gsub("precip_","",cessation))
    ifelse(is.na(cessation_j)== TRUE ,cessation_j <- 101, ifelse(cessation_j > 300, cessation_j <- 60, ifelse(cessation_j < 40, cessation_j <- 60,cessation_j <- cessation_j))) # If cess happens soon after onset ignore it and set value == 60 days (a conservative value)
    #paste0("day ",as.numeric(gsub("precip_","",cessation_j)))
    cessation_c <- as.Date(cessation_j, origin = as.Date(paste0(year + 1,"-01-01")))
    
    # number of dry spell days
    if(planting_harvest_sameYear == TRUE){
    day_d <- as.numeric(gsub("precip_","",names(rainiq[-rainm,m])))
    dd0 <- which(day_d > onset_date_j) ; dd1 <-  which(day_d+365  < cessation_j+365)

    # dry_days <- day_d[c(dd0,dd1)]
    # length_dry_days <- diff(wet_days)
    # length_wet_days <- diff(dry_days)
    season_length <- cessation_j - onset_date_j
    }else{
      day_d <- as.numeric(gsub("precip_","",names(rainiq[-rainm,m])))
      dd0 <- which(day_d > onset_date_j) ; dd1 <-  which(day_d  < cessation_j)

      # dry_days <- day_d[c(dd0,dd1)]
      
     # wd0 <-  which(day_r >= onset_date_j); wd1 <- which(day_r < cessation_j)
     #  wet_days <- day_r[c(wd0,wd1)]
     #  length_dry_days <- ifelse(diff(wet_days) < 0, (diff(wet_days) + 365), diff(wet_days))
     #  length_wet_days <- ifelse(diff(dry_days) < 0, (diff(dry_days) + 365), diff(dry_days))
     #  max_length_wet <- max(length_wet_days)
     #  max_length_dry <- max(length_dry_days)
     #  dry_spell <- round(sum(ifelse(length_dry_days/10 >1,length_dry_days/10,0)))
     #  season_length <- (365 - onset_date_j) + cessation_j
    }
    
    #rain_onset_cessation <- rbind(rain_onset_cessation, c(onset_date_j, as.character(onset_date_c), cessation_j, as.character(cessation_c), max_length_wet, max_length_dry, season_length, dry_spell))
    
  }
  #rain_onset_cessation <- as.data.frame(rain_onset_cessation)
  #names(rain_onset_cessation) <- c('j_onsetr', 'c_onsetr', 'j_cessr', 'c_cessr','max_wet_days', 'max_dry_days', 'season_length', 'dry_spell')
  #names(rain_onset_cessation) <- c('j_onsetr', 'c_onsetr', 'j_cessr', 'c_cessr', 'season_length', 'dry_spell')
  
  #gpsdata$j_onsetr <- rain_onset_cessation$j_onsetr
  #gpsdata$c_onsetr <- rain_onset_cessation$c_onsetr
  #gpsdata$j_cessr <- rain_onset_cessation$j_cessr
  #gpsdata$c_cessr <- rain_onset_cessation$c_cessr
  #gpsdata$max_consecutive_wet_days <- rain_onset_cessation$max_wet_days
  #gpsdata$max_consecutive_dry_days <- rain_onset_cessation$max_dry_days
  #gpsdata$season_length <- rain_onset_cessation$season_length
  #gpsdata$dry_spell <- rain_onset_cessation$dry_spell
  
  
  
  ## The number of rainy days (thr >= 2 mm) over the growing period 
  gpsdata$nrRainyDays <- NULL
  for (m in 1:nrow(RFi)){
    # print(m)
    mdata <- RFi[m, ]
    mdata[mdata < 2] <- 0
    mdata[mdata >= 2] <- 1
    gpsdata$nrRainyDays[m] <- sum(mdata)
    
    ## The monthly rainfall, at 31 days interval and the remaining  days at the end, over the growing period
    mrdi <- RFi[m, ]
    mtmaxi <- Tmaxi[m,]
    mtmini <- Tmini[m,]
    mrhi <- RHi[m,]
    msri <- SRi[m,]
    mwsi <- WSi[m,]
    
    
    
    mdiv <- unique(c(seq(1, length(mrdi), 30), length(mrdi)))
    
    mrf <- c()
    mtmax <- c()
    mtmin <- c()
    mrh <- c()
    msr <- c()
    mws <- c()
    
    for (k in 1:(length(mdiv)-1)) {
      # print(k)
      if(k == 1){
        mrf <- c(mrf, sum(mrdi[c(mdiv[k]:mdiv[k+1])]))
        mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c(mdiv[k]:mdiv[k+1])])))
        mtmin <- c(mtmin, mean(as.numeric(mtmini[c(mdiv[k]:mdiv[k+1])])))
        mrh <- c(mrh, mean(as.numeric(mrhi[c(mdiv[k]:mdiv[k+1])])))
        msr <- c(msr, mean(as.numeric(msri[c(mdiv[k]:mdiv[k+1])])))
        mws <- c(mws, mean(as.numeric(mwsi[c(mdiv[k]:mdiv[k+1])])))
      }else{
        mrf <- c(mrf, sum(mrdi[c((mdiv[k]+1):(mdiv[k+1]))]))
        mtmax <- c(mtmax, mean(as.numeric(mtmaxi[c((mdiv[k]+1):(mdiv[k+1]))])))
        mtmin <- c(mtmin, mean(as.numeric(mtmini[c((mdiv[k]+1):(mdiv[k+1]))])))
        mrh <- c(mrh, mean(as.numeric(mrhi[c((mdiv[k]+1):(mdiv[k+1]))])))
        msr <- c(msr, mean(as.numeric(msri[c((mdiv[k]+1):(mdiv[k+1]))])))
        mws <- c(mws, mean(as.numeric(mwsi[c((mdiv[k]+1):(mdiv[k+1]))])))
      }
    }
    
    if(length(mrf) > 15){## if the crop is > 15 months on the field ( to account for cassava as well)
      mrf <- c(mrf, rep("NA", 15 -length(mrf)))
      mtmax <- c(mtmax, rep("NA", 15 - length(mtmax)))
      mtmin <- c(mtmin, rep("NA", 15 -length(mtmin)))
      mrh <- c(mrh, rep("NA", 15 -length(mrh)))
      msr <- c(msr, rep("NA", 15 -length(msr)))
      mws <- c(mws, rep("NA", 15 -length(mws)))
    }
    
    mrf_names <- c(paste0("Rain_month", c(1:15)))
    mtmax_names <- c(paste0("Tmax_month", c(1:15)))
    mtmin_names <- c(paste0("Tmin_month", c(1:15)))
    mrh_names <- c(paste0("relativeHumid_month", c(1:15)))
    msr_names <- c(paste0("solarRad_month", c(1:15)))
    mws_names <- c(paste0("windSpeed_month", c(1:15)))
    
    
    for (h in 1:length(mrf_names)) {
      colname <- mrf_names[h]
      gpsdata[[colname]][m] <- mrf[h]
      
      colname <- mtmax_names[h]
      gpsdata[[colname]] <- mtmax[h]
      
      colname <- mtmin_names[h]
      gpsdata[[colname]] <- mtmin[h]
      
      colname <- mrh_names[h]
      gpsdata[[colname]] <- mrh[h]
      
      colname <- msr_names[h]
      gpsdata[[colname]] <- msr[h]
      
      colname <- mws_names[h]
      gpsdata[[colname]] <- mws[h]
    }
  }
  
  
  if(planting_harvest_sameYear== TRUE){
    gpsdata$plantingYear <- str_extract(rastLayerRF_1, "[[:digit:]]+")
    gpsdata$harvestYear <- str_extract(rastLayerRF_1, "[[:digit:]]+")
  }else{
    gpsdata$plantingYear <- str_extract(rastLayerRF_1, "[[:digit:]]+")
    gpsdata$harvestYear <- str_extract(rastLayerRF_2, "[[:digit:]]+")
  }
  
  gpsdata <- gpsdata %>% 
    select_if(~sum(!is.na(.)) > 0)
  
  return(gpsdata)
}


