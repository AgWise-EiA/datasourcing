summarize_pointdata(rastLayerRF_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1981.nc",rastLayerRF_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1982.nc",
                           gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)), pl_j=35, hv_j=150, planting_harvest_sameYear = TRUE)

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

test <- summarize_pointdata(rastLayerRF_1="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1981.nc",rastLayerRF_2="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps/1982.nc",
                    rastLayerTmax_1 = rastLayerTmax_1, rastLayerTmax_2 = rastLayerTmax_2, gpsdata=data.frame(longitude = c(29.375, 30.125), latitude = c(-2.825, -2.425)), pl_j=35, hv_j=150, planting_harvest_sameYear = TRUE, rastLayerTmin_1 = rastLayerTmin_1, rastLayerTmin_2= rastLayerTmin_2,
                    rastLayerRH_1= rastLayerRH_1, rastLayerRH_2= rastLayerRH_2,
                    rastLayerSR_1= rastLayerSR_1, rastLayerSR_2= rastLayerSR_2,
                    rastLayerWS_1= rastLayerWS_1, rastLayerWS_2= rastLayerWS_2 )


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
                                gpsdata, pl_j, hv_j, planting_harvest_sameYear){
  
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


