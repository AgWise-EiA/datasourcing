#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("terra", "sf", "rgl", "rgdal", "sp", "geodata", "tidyverse", "geosphere", "countrycode")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))



#################################################################################################################
# Extract geo-spatial data with time dimension 
#' @description this functions loops through all .nc files (~30 - 40 years) for rain. temperature, solar radiation, wind speed and relative humidity. 
#' 
#' @param country country name to be used to extract the first two level of administrative units to attach to the data. 
#' @param inputData is a data frame and must have the c(lat, lon, plantingDate, harvestDate). For field observations, plantingDate  harvestDate should be given in yyyy-mm-dd format. 
#' @param AOI TRUE if data for multiple years is required. FALSE if data is required for field trials, for which the actual interval between the planting and harvest dates will be used. 
#' @param Planting_month_date if AOI is TRUE, Planting_month_date should be provided in mm-dd format. weather data across years between Planting_month_date and Harvest_month_date will be provided
#' @param Harvest_month_date if AOI is TRUE, Harvest_month_date should be provided in mm-dd format.  weather data across years between Planting_month_date and Harvest_month_date will be provided
#' @param varName is the name of the variable for which data is required and it is one of c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed")
#' @param jobs defines how many cores to use for parallel data sourcing
#' 
#' @return based on the provided variable name, this function returns, daily data for every GPS together with longitude, latitude, planting Date, harvest Date, NAME_1, NAME_2 and 
#' daily data with columns labelled with the concatenation of variable name and Julian day of data.  When AOI is set to FALSE, every GPS location is allowed to have 
#' its own unique planting and harvest dates and in this case, because the different GPS location can have non-overlapping dates, NA values are filled for dates prior to
#' planting and later than harvest dates. When AOI is true, the user defined Planting_month_date and Harvest_month_date is considered for all locations and data is provided across the years. 
#' 
#' @examples: get_summaries(fd <- get_weather_pointData(inputData = readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Rwanda_RAB/Potato/result/compiled_fieldData.RDS"), 
#' country = "Rwanda", AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, ID="TLID", varName="temperatureMin", jobs=10)
get_weather_pointData <- function(country, inputData,  AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, varName, jobs){
  
  ## 1. read all the raster files 
  if(varName == "Rainfall"){
    listRaster <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Rainfall/chirps", pattern=".nc$", full.names = TRUE)
  }else if (varName == "temperatureMax"){
    listRaster <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMax/AgEra", pattern=".nc$", full.names = TRUE)
  }else if (varName == "temperatureMin"){
    listRaster <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/TemperatureMin/AgEra", pattern=".nc$", full.names = TRUE)
  }else if(varName == "relativeHumidity"){
    listRaster <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/RelativeHumidity/AgEra", pattern=".nc$", full.names = TRUE)
  }else if(varName == "solarRadiation"){
    listRaster <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/SolarRadiation/AgEra", pattern=".nc$", full.names = TRUE)
  }else if(varName == "windSpeed"){
    listRaster <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/WindSpeed/AgEra", pattern=".nc$", full.names = TRUE)
  }
  
  
  ## 2. format the input data with GPS, dates and ID and add adminstrative unit info
  if(AOI == TRUE){
    if(is.null(Planting_month_date) | is.null(Harvest_month_date)){
      print("with AOL=TURE, Planting_month_date, Harvest_month_date can not be null, please refer to the documentation and provide mm-dd for both parameters")
      return(NULL)
    }
    countryCoord <- unique(inputData[, c("lon", "lat")])
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    
    ## check if both planting and harvest dates are in the same year
    Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
    harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
    if(Planting_month < harvest_month){
      planting_harvest_sameYear <- TRUE
    }else{
      planting_harvest_sameYear <- FALSE
    }
    
    if (planting_harvest_sameYear ==TRUE){ #is used only to get the date of the year so the years 2001 and 2002 have no value except for formating 
      countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2001, Harvest_month_date, sep="-")
    }else{
      countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
      countryCoord$harvestDate <- paste(2002, Harvest_month_date, sep="-")
    }
    countryCoord <- countryCoord[complete.cases(countryCoord), ]
    names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate")
    ground <- countryCoord[, c("longitude", "latitude", "plantingDate", "harvestDate")]
    
  }else{
    inputData <- unique(inputData[, c("lon", "lat", "plantingDate", "harvestDate")])
    if(is.null(inputData$ID)){
      inputData$ID <- c(1:nrow(inputData)) 
    }
    inputData <- inputData[complete.cases(inputData), ]
    names(inputData) <- c("longitude", "latitude", "plantingDate", "harvestDate", "ID")
    ground <- inputData
  }
  
  ground$plantingDate <- as.Date(ground$plantingDate, "%Y-%m-%d")
  ground$harvestDate <- as.Date(ground$harvestDate, "%Y-%m-%d")
  countryShp <- geodata::gadm(country, level = 3, path='.')
  dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
  ground$NAME_1 <- dd2$NAME_1
  ground$NAME_2 <- dd2$NAME_2
  
  ## 3.get the seasonal rainfall parameters for AOI
  
  if(AOI == TRUE){
    
    # Convert planting Date and harvesting in Julian Day 
    pl_j <-as.POSIXlt(unique(ground$plantingDate))$yday
    hv_j <-as.POSIXlt(unique(ground$harvestDate))$yday
    
    
    if (planting_harvest_sameYear ==  TRUE) {
      
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      
      
      rf_result <- foreach(i=1:length(listRaster), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        rasti <- listRaster[i]
        PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        if(varName %in% c("temperatureMax","temperatureMin")){
          raini <- raini-274
        }else if (varName == "solarRadiation"){
          raini <- raini/1000000
        }
        names(raini) <- paste(varName, sub("^[^_]+", "", names(raini)), sep="")
        ground_adj <- ground
        lubridate::year(ground_adj$plantingDate) <- as.numeric(str_extract(rasti, "[[:digit:]]+"))
        lubridate::year(ground_adj$harvestDate) <- as.numeric(str_extract(rasti, "[[:digit:]]+"))
        ground_adj$plantingDate <- as.character(ground_adj$plantingDate)
        ground_adj$harvestDate <- as.character(ground_adj$harvestDate)
        ground2 <- cbind(ground_adj, raini)
      }
      
      data_points <- dplyr::bind_rows(rf_result)

      stopCluster(cls)
    }
    
    
    if (planting_harvest_sameYear ==  FALSE) {
      cls <- makeCluster(jobs)
      doParallel::registerDoParallel(cls)
      ## Rainfall
      rf_result2 <- foreach(i = 1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
        listRaster_RF <- listRaster_RF[order(listRaster_RF)]
        rast1 <- listRaster_RF[i]
        rast2 <- listRaster_RF[i+1]
        rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
        rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
        PlHvD <- c(rasti1, rasti2)
        xy <- ground[, c("longitude", "latitude")]
        raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
        raini <- raini[,-1]
        if(varName %in% c("temperatureMax","temperatureMin")){
          raini <- raini-274
        }else if (varName == "solarRadiation"){
          raini <- raini/1000000
        }
        names(raini) <- paste(varName,  sub("^[^_]+", "", names(raini)), sep="")
        if(length(grep("_366", names(raini))) > 0){
          raini <- raini[,-grep("_366", names(raini))]
        }
        ground_adj <- ground
        lubridate::year(ground_adj$plantingDate) <- as.numeric(str_extract(rast1, "[[:digit:]]+"))
        lubridate::year(ground_adj$harvestDate) <- as.numeric(str_extract(rast2, "[[:digit:]]+"))
        ground2 <- cbind(ground_adj, raini)
        
      }
      
      data_points <- dplyr::bind_rows(rf_result)
      stopCluster(cls)
    }
    
  } else {
    
    
    # Get the Year
    ground$yearPi <- format(as.POSIXlt(ground$plantingDate), "%Y")
    ground$yearHi <- format(as.POSIXlt(ground$harvestDate), "%Y")
    
    # Convert planting date and harvesting date in Julian Day
    ground$ pl_j <-as.POSIXlt(ground$plantingDate)$yday
    ground$hv_j <-as.POSIXlt(ground$harvestDate)$yday
    
    # get the max number of days on the field to be used as column names. 
    ground$growinglength <- ifelse(ground$yearPi == ground$yearHi, 
                                   ground$hv_j - ground$pl_j,
                                   365 - ground$pl_j + ground$hv_j)
    
    # create list of all possible column names to be able to rbind data from different sites with different planting and harvest dates ranges
    rf_names <- c(paste0(varName, "_",  c(min(ground$pl_j):max(ground$hv_j))))
    rf_names2 <-  as.data.frame(matrix(nrow=length(rf_names), ncol=1))
    colnames(rf_names2) <- "dataDate"
    rf_names2[,1] <- rf_names
    rf_names2$ID <- c(1:nrow(rf_names2))
 
    
    data_points <- NULL
    for(i in 1:nrow(ground)){
      print(i)
      groundi <- ground[i, c("longitude", "latitude", "plantingDate", "harvestDate","ID", "NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j", "growinglength")]
      yearPi <- groundi$yearPi
      yearHi <- groundi$yearHi
      pl_j <- groundi$pl_j
      hv_j <- groundi$hv_j

      # Case planting and harvesting dates span the same year
      if (yearPi == yearHi) {
        rasti<-listRaster[which(grepl(yearPi, listRaster, fixed=TRUE) == T)]
        rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
      }
      
      # Case planting and harvesting dates span two different years
      if (yearPi < yearHi) {
        rasti1<-listRaster[which(grepl(yearPi, listRaster, fixed=TRUE) == T)]
        rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
        rasti2 <-listRaster[which(grepl(yearHi, listRaster, fixed=TRUE) == T)]
        rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
        rasti <- c(rasti1, rasti2)
        
      }
      
      ### Extract the information for the i-th row 
      
      xy <- groundi[, c("longitude", "latitude")]
      xy <- xy %>%
        mutate_if(is.character, as.numeric)
      
      raini <- terra::extract(rasti, xy,method='simple', cells=FALSE)
      raini <- raini[,-1]
      if(varName %in% c("temperatureMax","temperatureMin")){
        raini <- raini-274
      }else if (varName == "solarRadiation"){
        raini <- raini/1000000
      }
      names(raini) <- paste(varName,  sub("^[^_]+", "", names(raini)), sep="")
      raini <- as.data.frame(t(raini))
      raini$dataDate <- rownames(raini)
      rownames(raini) <- NULL
      
      ## merging data for different trials with differing growing period requires having data for the whole period of time
      raini <- merge(raini, rf_names2, by="dataDate", all.y=TRUE)
      raini <- raini[order(raini$ID),]
      rownames(raini) <- raini$dataDate
      raini <- raini %>% dplyr::select(-c(ID,dataDate))
      raini2 <- as.data.frame(t(raini))
      rownames(raini2) <- NULL
      raini2 <- cbind(groundi, raini2)
      data_points <- rbind(data_points, raini2)
    }
  }
 
  
  return(data_points)
}


################################################################################


#' @description Extract geo-spatial data with no temporal dimension, i,e,. soil properties and topography variables
#' 
#' @param country country name to be sued to extract the first two level of administrative units to attach to the data. 
#' @param inputData is a data frame and must have the c(lat, lon) 
#' @param profile is true/false, if true data, isirc data for the six soil profiles will be processed. This is required for DSSAT and other crop models. 
#' @param pathOut is path used to download the DEM layers temporarily and these layers can be removed after obtaining the data from this function. 
#' 
#' 
#' @return a data frame with lon, lat,teh top two admistrnative zones, soil properties with columns named with variable names attached with depth,  
#' elevations variables attached for every GPS location 
#' @examples: get_soil_DEM_pointData(country = "Rwanda", profile = TRUE, 
#'                                  pathOut = "~/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/transform/topography",
#'                                 inputData = data.frame(lon=c(-1.538, -1.534), lat=c(29.37, 29.35)))
get_soil_DEM_pointData <- function(country, inputData, profile =FALSE, pathOut){
  
 
  ## 1. read soil global data
  if(profile == TRUE){
    listRaster_soil <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/soilGrids/profile", pattern=".tif$")
    readLayers_soil <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/soilGrids/profile", listRaster_soil, sep="/"))
  }else{
    listRaster_soil <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/iSDA", pattern=".tif$")
    readLayers_soil <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/iSDA", listRaster_soil, sep="/"))
 
    listRaster_soil_isric <-list.files(path="/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/soilGrids", pattern=".tif$")
    readLayers_soil_isric <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/soilGrids", listRaster_soil_isric, sep="/"))
    
  }
  
 
  ## 2. read the shape file of the country and crop the global data
  countryShp <- geodata::gadm(country, level = 3, path='.')
  croppedLayer_soil <- terra::crop(readLayers_soil, countryShp)
  
  
 
  ## 3. apply pedo-transfer functions to get soil organic matter and soil hydraulics variables 
  if (profile == TRUE){
    
    depths <- c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")  
    ## get soil organic matter as a function of organic carbon
    for(i in 1:length(depths)) {
      croppedLayer_soil[[paste0("SOM_",depths[i])]] <- (croppedLayer_soil[[paste0("soc_",depths[i])]] * 2)/10
    }
    
    
    ##### permanent wilting point ####
    for(i in 1:length(depths)) {
      croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (-0.024 * croppedLayer_soil[[paste0("sand_",depths[i])]]/100) + 0.487 *
        croppedLayer_soil[[paste0("clay_",depths[i])]]/100 + 0.006 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
        0.005*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
        0.013*(croppedLayer_soil[[paste0("clay_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) +
        0.068*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay_",depths[i])]]/100 ) + 0.031
      croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (croppedLayer_soil[[paste0("PWP_",depths[i])]] + 
                                                         (0.14 * croppedLayer_soil[[paste0("PWP_",depths[i])]] - 0.02))
    }
    
    ##### FC ######
    for(i in 1:length(depths)) {
      croppedLayer_soil[[paste0("FC_",depths[i])]] <- -0.251 * croppedLayer_soil[[paste0("sand_",depths[i])]]/100 + 0.195 * 
        croppedLayer_soil[[paste0("clay_",depths[i])]]/100 + 0.011 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
        0.006*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
        0.027*(croppedLayer_soil[[paste0("clay_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) + 
        0.452*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay_",depths[i])]]/100) + 0.299
      croppedLayer_soil[[paste0("FC_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]] + (1.283 * croppedLayer_soil[[paste0("FC_",depths[i])]]^2 - 0.374 * croppedLayer_soil[[paste0("FC_",depths[i])]] - 0.015))
      
    }
    
    
    ##### soil water at saturation ######
    for(i in 1:length(depths)) {
      croppedLayer_soil[[paste0("SWS_",depths[i])]] <- 0.278*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100)+0.034*
        (croppedLayer_soil[[paste0("clay_",depths[i])]]/100)+0.022*croppedLayer_soil[[paste0("SOM_",depths[i])]] -
        0.018*(croppedLayer_soil[[paste0("sand_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])- 0.027*
        (croppedLayer_soil[[paste0("clay_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])-
        0.584 * (croppedLayer_soil[[paste0("sand_",depths[i])]]/100*croppedLayer_soil[[paste0("clay_",depths[i])]]/100)+0.078
      croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("SWS_",depths[i])]] +(0.636*croppedLayer_soil[[paste0("SWS_",depths[i])]]-0.107))
      croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]]+croppedLayer_soil[[paste0("SWS_",depths[i])]]-(0.097*croppedLayer_soil[[paste0("sand_",depths[i])]]/100)+0.043)
      
    }
    
    ##### saturated conductivity ######
    for(i in 1:length(depths)) {
      b = (log(1500)-log(33))/(log(croppedLayer_soil[[paste0("FC_",depths[i])]])-log(croppedLayer_soil[[paste0("PWP_",depths[i])]]))
      lambda <- 1/b
      croppedLayer_soil[[paste0("KS_",depths[i])]] <- 1930*((croppedLayer_soil[[paste0("SWS_",depths[i])]]-croppedLayer_soil[[paste0("FC_",depths[i])]])^(3-lambda))
    }
     
    soilData <- croppedLayer_soil
    
  }else{
   
    depths <- c("0-20cm","20-50cm")  
    
    ## get soil organic matter as a function of organic carbon
    for(i in 1:length(depths)) {
      croppedLayer_soil[[paste0("SOM_",depths[i])]] <- (croppedLayer_soil[[paste0("oc_",depths[i])]] * 2)/10
    }

    ##### permanent wilting point ####
    for(i in 1:length(depths)) {
      croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (-0.024 * croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100) + 0.487 *
        croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 + 0.006 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
        0.005*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
        0.013*(croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) +
        0.068*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 ) + 0.031
      croppedLayer_soil[[paste0("PWP_",depths[i])]] <- (croppedLayer_soil[[paste0("PWP_",depths[i])]] + (0.14 * croppedLayer_soil[[paste0("PWP_",depths[i])]] - 0.02))
    }
    

    
   ##### FC ######
    for(i in 1:length(depths)) {
      croppedLayer_soil[[paste0("FC_",depths[i])]] <- -0.251 * croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 + 0.195 * 
        croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 + 0.011 * croppedLayer_soil[[paste0("SOM_",depths[i])]] + 
        0.006*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) - 
        0.027*(croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("SOM_",depths[i])]]) + 
        0.452*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100 * croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100) + 0.299
      croppedLayer_soil[[paste0("FC_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]] + (1.283 * croppedLayer_soil[[paste0("FC_",depths[i])]]^2 - 0.374 * croppedLayer_soil[[paste0("FC_",depths[i])]] - 0.015))
      
    }
    
    
    ##### soil water at saturation ######
    for(i in 1:length(depths)) {
      croppedLayer_soil[[paste0("SWS_",depths[i])]] <- 0.278*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100)+0.034*
        (croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100)+0.022*croppedLayer_soil[[paste0("SOM_",depths[i])]] -
        0.018*(croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])- 0.027*
        (croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("SOM_",depths[i])]])-
        0.584 * (croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100*croppedLayer_soil[[paste0("clay.tot.psa_",depths[i])]]/100)+0.078
      croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("SWS_",depths[i])]] +(0.636*croppedLayer_soil[[paste0("SWS_",depths[i])]]-0.107))
      croppedLayer_soil[[paste0("SWS_",depths[i])]] <- (croppedLayer_soil[[paste0("FC_",depths[i])]]+croppedLayer_soil[[paste0("SWS_",depths[i])]]-(0.097*croppedLayer_soil[[paste0("sand.tot.psa_",depths[i])]]/100)+0.043)
      
    }
    
    ##### saturated conductivity ######
    for(i in 1:length(depths)) {
      b = (log(1500)-log(33))/(log(croppedLayer_soil[[paste0("FC_",depths[i])]])-log(croppedLayer_soil[[paste0("PWP_",depths[i])]]))
      lambda <- 1/b
      croppedLayer_soil[[paste0("KS_",depths[i])]] <- 1930*((croppedLayer_soil[[paste0("SWS_",depths[i])]]-croppedLayer_soil[[paste0("FC_",depths[i])]])^(3-lambda))
    }
    
    names(croppedLayer_soil) <- gsub("0-20cm", "top", names(croppedLayer_soil))
    names(croppedLayer_soil) <- gsub("20-50cm", "bottom", names(croppedLayer_soil))
    names(croppedLayer_soil) <- gsub("_0-200cm", "", names(croppedLayer_soil))
    names(croppedLayer_soil) <- gsub("\\.", "_",  names(croppedLayer_soil)) 
    croppedLayer_isric <- terra::crop(readLayers_soil_isric, countryShp)
    names(croppedLayer_isric) <- gsub("0-30cm", "0_30", names(croppedLayer_isric))
    
    soilData <- c(croppedLayer_soil, croppedLayer_isric)
  }
  
  
  
  ## 4. Extract point soil data 
  gpsPoints <- unique(inputData[, c("lon", "lat")])
  gpsPoints <- gpsPoints[complete.cases(gpsPoints), ]
  gpsPoints$x <- as.numeric(gpsPoints$lon)
  gpsPoints$y <- as.numeric(gpsPoints$lat)
  gpsPoints <- gpsPoints[, c("x", "y")]
  

  pointDataSoil <- as.data.frame(raster::extract(soilData, gpsPoints))
  pointDataSoil <- subset(pointDataSoil, select=-c(ID))
  names(gpsPoints) <- c("lon", "lat")
  pointDataSoil <- cbind(gpsPoints, pointDataSoil)
  
 
  dd2 <- raster::extract(countryShp, gpsPoints)[, c("NAME_1", "NAME_2")]
  pointDataSoil$NAME_1 <- dd2$NAME_1
  pointDataSoil$NAME_2 <- dd2$NAME_2
  
  
  
  ## 5. Extract DEM data 
  countryExt <- terra::ext(countryShp)
  listRaster_dem1 <-geodata::elevation_3s(lon=countryExt[1], lat=countryExt[3], path=pathOut) #xmin - ymin
  listRaster_dem2 <-geodata::elevation_3s(lon=countryExt[1], lat=countryExt[4], path=pathOut) #xmin - ymax
  listRaster_dem3 <-geodata::elevation_3s(lon=countryExt[2], lat=countryExt[3], path=pathOut) #xmax - ymin
  listRaster_dem4 <-geodata::elevation_3s(lon=countryExt[2], lat=countryExt[4], path=pathOut) #xmax - ymax
  listRaster_dem <- terra::mosaic(listRaster_dem1, listRaster_dem2, listRaster_dem3, listRaster_dem4, fun='mean')
  
  dem <- terra::crop(listRaster_dem, countryShp)
  slope <- terra::terrain(dem, v = 'slope', unit = 'degrees')
  tpi <- terra::terrain(dem, v = 'TPI')
  tri <- terra::terrain(dem, v = 'TRI')
  
  topoLayer <- terra::rast(list(dem, slope, tpi, tri))
  datatopo <- terra::extract(topoLayer, gpsPoints, method='simple', cells=FALSE)
  datatopo <- subset(datatopo, select=-c(ID))
  topoData <- cbind(gpsPoints, datatopo)
  names(topoData) <- c("lon", "lat", "altitude", "slope", "TPI", "TRI" )
  pointDataSoil <- unique(merge(pointDataSoil, topoData, by=c("lon", "lat")))
  
  return(pointDataSoil)
}





################################################################################
#' Title Extract soil, DEM and daily weather data
#'
#' @param country country name to be used for cropping, extracting the top two administrative region names and to define input and output paths
#' @param useCaseName use case name or a project name, and this is used to define input and output paths
#' @param Crop is crop name and is used to define input and output paths
#' @param AOI is TRUE is the input data has defined planting and harvest dates otherwise FALSE
#' @param Planting_month_date planting month and date in mm-dd format and must be provided if AOI is TRUE 
#' @param Harvest_month_date harvest month and date in mm-dd format and must be provided if AOI is TRUE 
#' @param weatherData is TRUE is weather data is required otherwise FALSE
#' @param soilData is TRUE if soil data is required otherwise FALSE
#' @param soilProfile is TRUE if soil data from the six profile of ISRIC is required, otherwise set to FALSE
#' @param jobs number of cores used to parallel weather data extraction
#'
#' @return If weatherData is TRUE, list of data frames with daily data for c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed") is returned. 
#' If soilData is set TRUE, soil properties at different depth plus elevation and derivatives of DEM are returned. These results are written out in paths defined by country, useCaseName, and crop 
#' and either raw or result of the different AgWise modules space in CG Labs. If AOI is set TRUE, the weather data between the Planting_month_date and Harvest_month_date and for 1979 - 2022 data will be returned. 

#' @examples extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
#' soilData = TRUE, weatherData = TRUE,soilProfile = FALSE, jobs =10)
extract_geoSpatialPointData <- function(country, useCaseName, Crop, 
                                        AOI=FALSE,Planting_month_date=NULL, Harvest_month_date=NULL, 
                                        weatherData = TRUE, soilData = TRUE, soilProfile = FALSE,jobs =10){
  
  #require("the R package to be developed from get_geoSpatialData.R")
  
  if(AOI == TRUE){
    inputData <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_", useCaseName,"/", Crop, "/result/AOI_GPS.RDS", sep=""))
  }else{
    inputData <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_", useCaseName,"/", Crop, "/result/compiled_fieldData.RDS", sep=""))
  }
  
  
  if(soilProfile == TRUE){
    pathOut1 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName,"/", Crop, "/result/profile/", sep="")
    pathOut2 <- paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/profile/", sep="")
    pathOut3 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/profile", sep="")
    pathOut4 <- paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/profile", sep="")
  }else{
    pathOut1 <- paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_", useCaseName,"/", Crop, "/result/", sep="")
    pathOut2 <- paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/", sep="")
    pathOut3 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/", sep="")
    pathOut4 <- paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/", sep="")
  }
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut4)){
    dir.create(file.path(pathOut4), recursive = TRUE)
  }
  
  
  
  if(weatherData == TRUE){
    i=1
    wData <- list()
    for(varName in c("Rainfall", "temperatureMax", "temperatureMin", "relativeHumidity", "solarRadiation", "windSpeed")){
      vData <- get_weather_pointData(inputData = inputData, 
                                     country = country, AOI=AOI, Planting_month_date=Planting_month_date, 
                                     Harvest_month_date=Harvest_month_date, varName=varName, jobs=jobs)
      
      # w_name <- ifelse(AOI == TRUE, paste(varName, "_PointData_AOI.RDS", sep=""), paste(varName, "_PointData_trial.RDS", sep=""))
      # saveRDS(vData, paste(pathOut1, w_name, sep="/"))
      # saveRDS(vData, paste(pathOut2, w_name, sep="/"))
      # saveRDS(vData, paste(pathOut3, w_name, sep="/"))
      # saveRDS(vData, paste(pathOut4, w_name, sep="/"))
      
      wData[[i]] <-  vData
      i=i+1
    }
    
  }
  
  if(soilData == TRUE){
    sData <- get_soil_DEM_pointData(country = country, profile = soilProfile, 
                                    pathOut = pathOut2, inputData = inputData)
    
    # s_name <- ifelse(AOI == TRUE, "SoilDEM_PointData_AOI.RDS",  "SoilDEM_PointData_trial.RDS")
    # saveRDS(sData, paste(pathOut1, s_name, sep="/"))
    # saveRDS(sData, paste(pathOut2, s_name, sep="/"))
    # saveRDS(sData, paste(pathOut3, s_name, sep="/"))
    # saveRDS(sData, paste(pathOut4, s_name, sep="/"))
    
    
  }
  
  if(weatherData == TRUE & soilData == TRUE){
    wData[[7]] <- sData
    return(wData)
  }else if (weatherData == TRUE & soilData == FALSE){
    return(wData)
  }else if(weatherData == FALSE & soilData == TRUE) {
    return(sData)
  }
  
}