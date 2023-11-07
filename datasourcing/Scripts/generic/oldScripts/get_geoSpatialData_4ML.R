# 1. Sourcing required packages -------------------------------------------
#################################################################################################################
packages_required <- c("doParallel", "foreach", "chirps", "tidyverse", "dplyr", "lubridate", "stringr", "terra", "countrycode", "sf")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

# 2. Join Geospatial for ML based on point data ---------------------------
###########################################################################


## Except for soil and topography data, for AOI, both the daily data to be used for crop models and the summaries to be used for ML, are by planting and harvest dates
## at this point the summaries are computed by year for the growing season. When the dry, wet and average years are implemented we could aggregate across years matching the three classes
## for trial sites it is using actual planting and harvest dates of every trial.

#' Title
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param Planting_month_date is needed only for AOI and should be Rain_monthly provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param dataSource is among c("CHIRPS", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#'
#'
#' @return
#' @export
#'
#' @examples join_geospatial_4ML(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = TRUE , 
#' dataSource = "CHIRPS", Planting_month_date = "02_05", ID = "TLID")

join_geospatial_4ML <- function(country, useCaseName, Crop, AOI, Planting_month_date, dataSource=NULL, ID, overwrite = TRUE){
  
  
  ## create directories to save output
  pathOut1 <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/geo_4ML", sep="")
  pathOut2 <- paste("/home/jovyan/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/geo_4ML", sep="")
  
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut2)){
    dir.create(file.path(pathOut2), recursive = TRUE)
  }
  
  
  if (AOI == FALSE){
    ## trial sites info
    GPS_Data <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))
    GPS_Data$Yield <- round(GPS_Data$TY, 3)
    GPS_Data <- subset(GPS_Data, select=-c(TY))
    ## geo spatial point data with no time dimension
    Soil_Topo_PointData <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/SoilDEM_PointData_trial.RDS", sep=""))
    
    ## daily geo spatial point data
    Rainfall_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_trial_CHIRPS.RDS", sep=""))
    WindSpeed_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/WindSpeed/WindSpeed_summaries_trial_AgEra.RDS", sep=""))
    Tmax_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmax_summaries_trial_AgEra.RDS", sep=""))
    Tmin_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmin_summaries_trial_AgEra.RDS", sep=""))
    SolarRadiation_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/SolarRadiation/SolarRadiation_summaries_trial_AgEra.RDS", sep=""))
    RelativeHumidity_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/RelativeHumidity/RelativeHumidity_summaries_trial_AgEra.RDS", sep=""))
    
    
    ### drop NA columns
    Rainfall_summaries <- Rainfall_summaries %>% select_if(~all(!is.na(.)))
    WindSpeed_summaries <- WindSpeed_summaries %>% select_if(~all(!is.na(.)))
    Tmax_summaries <- Tmax_summaries %>% select_if(~all(!is.na(.)))
    Tmin_summaries <- Tmin_summaries %>% select_if(~all(!is.na(.)))
    SolarRadiation_summaries <- SolarRadiation_summaries %>% select_if(~all(!is.na(.)))
    RelativeHumidity_summaries <- RelativeHumidity_summaries %>% select_if(~all(!is.na(.)))
    
    ### keep locations where we have field data
    GPS_Data$location <- paste(GPS_Data$lon, GPS_Data$lat, sep="_")
    Rainfall_summaries$location <- paste(Rainfall_summaries$longitude, Rainfall_summaries$latitude, sep="_")
    Tmax_summaries$location <- paste(Tmax_summaries$longitude, Tmax_summaries$latitude, sep="_")
    Tmin_summaries$location <- paste(Tmin_summaries$longitude, Tmin_summaries$latitude, sep="_")
    RelativeHumidity_summaries$location <- paste(RelativeHumidity_summaries$longitude, RelativeHumidity_summaries$latitude, sep="_")
    WindSpeed_summaries$location <- paste(WindSpeed_summaries$longitude, WindSpeed_summaries$latitude, sep="_")
    SolarRadiation_summaries$location <- paste(SolarRadiation_summaries$longitude, SolarRadiation_summaries$latitude, sep="_")
    Topography_PointData$location <- paste(Topography_PointData$longitude, Topography_PointData$latitude, sep="_")
    Soil_PointData$location <- paste(Soil_PointData$longitude, Soil_PointData$latitude, sep="_")
    
    Rainfall_summaries <- droplevels(Rainfall_summaries[Rainfall_summaries$location  %in% GPS_Data$location, ])
    Tmax_summaries <- droplevels(Tmax_summaries[Tmax_summaries$location  %in% GPS_Data$location, ])
    Tmin_summaries <- droplevels(Tmin_summaries[Tmin_summaries$location  %in% GPS_Data$location, ])
    RelativeHumidity_summaries <- droplevels(RelativeHumidity_summaries[RelativeHumidity_summaries$location  %in% GPS_Data$location, ])
    WindSpeed_summaries <- droplevels(WindSpeed_summaries[WindSpeed_summaries$location  %in% GPS_Data$location, ])
    SolarRadiation_summaries <- droplevels(SolarRadiation_summaries[SolarRadiation_summaries$location  %in% GPS_Data$location, ])
    Topography_PointData <- droplevels(Topography_PointData[Topography_PointData$location  %in% GPS_Data$location, ])
    Soil_PointData <- droplevels(Soil_PointData[Soil_PointData$location  %in% GPS_Data$location, ])
    
    
    ## merge data
    Rainfall_summaries <- subset(Rainfall_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate, NAME_1, NAME_2))
    Tmax_summaries <- subset(Tmax_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2))
    Tmin_summaries <- subset(Tmin_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate, NAME_1, NAME_2))
    RelativeHumidity_summaries <- subset(RelativeHumidity_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2))
    WindSpeed_summaries <- subset(WindSpeed_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2))
    SolarRadiation_summaries <- subset(SolarRadiation_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate))
    Topography_PointData$TLID <- Topography_PointData$ID
    Topography_PointData <- subset(Topography_PointData, select=-c(location, longitude, latitude, ID))
    Soil_PointData$TLID <- Soil_PointData$ID
    Soil_PointData <- subset(Soil_PointData, select=-c(location, longitude, latitude, ID))
    
    
    df_list1 <- list(GPS_Data, Rainfall_summaries, Tmax_summaries, Tmin_summaries, RelativeHumidity_summaries, SolarRadiation_summaries)
    merged_df1 <- Reduce(function(x, y) merge(x, y, by = ID), df_list1)
    head(merged_df1)
    
    merged_df2 <- Topography_PointData %>%
      left_join(Soil_PointData) %>%
      left_join(GPS_Data) %>%
      left_join(merged_df1)
    
  }else{
    Planting_month_date <- gsub("-", "_", Planting_month_date)
    
    ## geo-spatial point data with no time dimension
    Topography_PointData <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Topography/Topography_PointData_AOI.RDS", sep=""))
    Soil_PointData <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Soil/Soil_PointData_AOI.RDS", sep=""))
    
    ## daily geo spatial point data
    if(dataSource == "CHIRPS"){
      Rainfall_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_AOI_", Planting_month_date, "_CHIRPS.RDS", sep=""))
    }else{
      Rainfall_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_AOI_",Planting_month_date, "_AgEra.RDS", sep=""))
    }
    Tmax_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmax_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    Tmin_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmin_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    SolarRadiation_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/SolarRadiation/SolarRadiation_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    RelativeHumidity_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/RelativeHumidity/RelativeHumidity_summaries_AOI_",Planting_month_date, "_AgEra.RDS", sep=""))
    
    ## merging data
    Meta_summaries <- unique(Rainfall_summaries[, c("Planting", "Harvesting", "plantingDate", "harvestDate")])
    Rainfall_summaries <- subset(Rainfall_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    Tmax_summaries <- subset(Tmax_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    Tmin_summaries <- subset(Tmin_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    RelativeHumidity_summaries <- subset(RelativeHumidity_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    SolarRadiation_summaries <- subset(SolarRadiation_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    
    
    df_list1 <- list(Rainfall_summaries, Tmax_summaries, Tmin_summaries, RelativeHumidity_summaries, SolarRadiation_summaries)
    merged_df1 <- Reduce(function(x, y) merge(x, y, by = c("longitude", "latitude", "plantingYear", "NAME_1", "NAME_2"), all = TRUE), df_list1)
    head(merged_df1)
    
    df_list2 <- list(merged_df1, Topography_PointData, Soil_PointData)
    merged_df2 <- Reduce(function(x, y) merge(x, y, by = c("longitude", "latitude", "NAME_1", "NAME_2"), all = TRUE), df_list2)
    
    
    merged_df2 <- merged_df2[!merged_df2$plantingYear %in% c(1979, 1980, 2023), ] ## not all layers have data for 1979 and 1980 and 2023 is incomplete
    merged_df2 <- merged_df2[order(merged_df2$plantingYear), ]
    
  }
  
  # merged_df2 <- merged_df2[complete.cases(merged_df2), ]
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  fname <- ifelse(AOI == TRUE, paste("geoSpatial_4ML_AOI_", Planting_month_date, ".RDS", sep=""), "geoSpatial_4ML_trial.RDS")
  
  saveRDS(merged_df2, paste(pathOut1, fname , sep="/"))
  # saveRDS(object = merged_df2, file=paste(pathOut2, fname, sep=""))
  saveRDS(merged_df2, paste(pathOut3, fname, sep="/"))
  
  return(merged_df2)
  
  
}



## Except for soil and topography data, for AOI, both the daily data to be used for crop models and the summaries to be used for ML, are by planting and harvest dates
## at this point the summaries are computed by year for the growing season. When the dry, wet and average years are implemented we could aggregate across years matching the three classes
## for trial sites it is using actual planting and harvest dates of every trial.

#' Title
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for trial sites
#' @param Planting_month_date is needed only for AOI and should be Rain_monthly provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param dataSource is among c("CHIRPS", "AgEra")
#' @param ID only when AOI  = FALSE, it is the column name Identifying the trial ID in compiled_fieldData.RDS
#'
#'
#' @return
#' @export
#'
#' @examples join_geospatial_4ML(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = TRUE , 
#' dataSource = "CHIRPS", Planting_month_date = "02_05", ID = "TLID")

join_geospatial_4ML <- function(country, useCaseName, Crop, AOI, Planting_month_date, dataSource=NULL, ID, overwrite = TRUE){
  
 
  
  
  ## create directories to save output 
  pathOut1 <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/data_4ML", sep="")
  # pathOut2 <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/data_4ML", sep="")
  pathOut3 <- paste("/home/jovyan/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/data_4ML", sep="")
  

  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  # if (!dir.exists(pathOut2)){
  #   dir.create(file.path(pathOut2), recursive = TRUE)
  # }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  
  
  if (AOI == FALSE){
    ## trial sites info
    GPS_Data <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))  
    GPS_Data$Yield <- round(GPS_Data$TY, 3)
    GPS_Data <- subset(GPS_Data, select=-c(TY))
    ## geo spatial point data with no time dimension
    Topography_PointData <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Topography/Topography_PointData_trial.RDS", sep=""))
    Soil_PointData <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Soil/Soil_PointData_trial.RDS", sep=""))
    ## daily geo spatial point data 
    if(dataSource == "CHIRPS"){
      Rainfall_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_trial_CHIRPS.RDS", sep=""))
    }else{
      Rainfall_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_trial_AgEra.RDS", sep=""))
    }
    WindSpeed_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/WindSpeed/WindSpeed_summaries_trial_AgEra.RDS", sep=""))
    Tmax_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmax_summaries_trial_AgEra.RDS", sep=""))
    Tmin_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmin_summaries_trial_AgEra.RDS", sep=""))
    SolarRadiation_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/SolarRadiation/SolarRadiation_summaries_trial_AgEra.RDS", sep=""))
    RelativeHumidity_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/RelativeHumidity/RelativeHumidity_summaries_trial_AgEra.RDS", sep=""))
    
    
    ### drop NA columns
    Rainfall_summaries <- Rainfall_summaries %>% select_if(~all(!is.na(.)))
    WindSpeed_summaries <- WindSpeed_summaries %>% select_if(~all(!is.na(.)))
    Tmax_summaries <- Tmax_summaries %>% select_if(~all(!is.na(.)))
    Tmin_summaries <- Tmin_summaries %>% select_if(~all(!is.na(.)))
    SolarRadiation_summaries <- SolarRadiation_summaries %>% select_if(~all(!is.na(.)))
    RelativeHumidity_summaries <- RelativeHumidity_summaries %>% select_if(~all(!is.na(.)))
    
    ### keep locations where we have field data
    GPS_Data$location <- paste(GPS_Data$lon, GPS_Data$lat, sep="_")
    Rainfall_summaries$location <- paste(Rainfall_summaries$longitude, Rainfall_summaries$latitude, sep="_")
    Tmax_summaries$location <- paste(Tmax_summaries$longitude, Tmax_summaries$latitude, sep="_")
    Tmin_summaries$location <- paste(Tmin_summaries$longitude, Tmin_summaries$latitude, sep="_")
    RelativeHumidity_summaries$location <- paste(RelativeHumidity_summaries$longitude, RelativeHumidity_summaries$latitude, sep="_")
    WindSpeed_summaries$location <- paste(WindSpeed_summaries$longitude, WindSpeed_summaries$latitude, sep="_")
    SolarRadiation_summaries$location <- paste(SolarRadiation_summaries$longitude, SolarRadiation_summaries$latitude, sep="_")
    Topography_PointData$location <- paste(Topography_PointData$longitude, Topography_PointData$latitude, sep="_")
    Soil_PointData$location <- paste(Soil_PointData$longitude, Soil_PointData$latitude, sep="_")
    
    Rainfall_summaries <- droplevels(Rainfall_summaries[Rainfall_summaries$location  %in% GPS_Data$location, ])
    Tmax_summaries <- droplevels(Tmax_summaries[Tmax_summaries$location  %in% GPS_Data$location, ])
    Tmin_summaries <- droplevels(Tmin_summaries[Tmin_summaries$location  %in% GPS_Data$location, ])
    RelativeHumidity_summaries <- droplevels(RelativeHumidity_summaries[RelativeHumidity_summaries$location  %in% GPS_Data$location, ])
    WindSpeed_summaries <- droplevels(WindSpeed_summaries[WindSpeed_summaries$location  %in% GPS_Data$location, ])
    SolarRadiation_summaries <- droplevels(SolarRadiation_summaries[SolarRadiation_summaries$location  %in% GPS_Data$location, ])
    Topography_PointData <- droplevels(Topography_PointData[Topography_PointData$location  %in% GPS_Data$location, ])
    Soil_PointData <- droplevels(Soil_PointData[Soil_PointData$location  %in% GPS_Data$location, ])
    
    
    ## merge data 
    Rainfall_summaries <- subset(Rainfall_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate, NAME_1, NAME_2))
    Tmax_summaries <- subset(Tmax_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2))
    Tmin_summaries <- subset(Tmin_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate, NAME_1, NAME_2))
    RelativeHumidity_summaries <- subset(RelativeHumidity_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2))
    WindSpeed_summaries <- subset(WindSpeed_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate,NAME_1, NAME_2))
    SolarRadiation_summaries <- subset(SolarRadiation_summaries, select=-c(location, longitude, latitude, plantingDate, harvestDate))
    Topography_PointData$TLID <- Topography_PointData$ID
    Topography_PointData <- subset(Topography_PointData, select=-c(location, longitude, latitude, ID))
    Soil_PointData$TLID <- Soil_PointData$ID
    Soil_PointData <- subset(Soil_PointData, select=-c(location, longitude, latitude, ID))
    
 
    df_list1 <- list(GPS_Data, Rainfall_summaries, Tmax_summaries, Tmin_summaries, RelativeHumidity_summaries, SolarRadiation_summaries)
    merged_df1 <- Reduce(function(x, y) merge(x, y, by = ID), df_list1)
    head(merged_df1)
    
    merged_df2 <- Topography_PointData %>% 
      left_join(Soil_PointData) %>%
      left_join(GPS_Data) %>%
      left_join(merged_df1)
    
   }else{
    Planting_month_date <- gsub("-", "_", Planting_month_date)
    
    ## geo-spatial point data with no time dimension
    Topography_PointData <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Topography/Topography_PointData_AOI.RDS", sep=""))
    Soil_PointData <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Soil/Soil_PointData_AOI.RDS", sep=""))
    
    ## daily geo spatial point data 
    if(dataSource == "CHIRPS"){
      Rainfall_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_AOI_", Planting_month_date, "_CHIRPS.RDS", sep=""))
    }else{
      Rainfall_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_summaries_AOI_",Planting_month_date, "_AgEra.RDS", sep=""))
    }
    Tmax_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmax_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    Tmin_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Tmin_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    SolarRadiation_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/SolarRadiation/SolarRadiation_summaries_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
    RelativeHumidity_summaries <- readRDS(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/RelativeHumidity/RelativeHumidity_summaries_AOI_",Planting_month_date, "_AgEra.RDS", sep=""))
  
    ## merging data
    Meta_summaries <- unique(Rainfall_summaries[, c("Planting", "Harvesting", "plantingDate", "harvestDate")])
    Rainfall_summaries <- subset(Rainfall_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    Tmax_summaries <- subset(Tmax_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    Tmin_summaries <- subset(Tmin_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    RelativeHumidity_summaries <- subset(RelativeHumidity_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    SolarRadiation_summaries <- subset(SolarRadiation_summaries, select=-c(Planting, Harvesting, harvestYear, plantingDate, harvestDate))
    
    
    df_list1 <- list(Rainfall_summaries, Tmax_summaries, Tmin_summaries, RelativeHumidity_summaries, SolarRadiation_summaries)
    merged_df1 <- Reduce(function(x, y) merge(x, y, by = c("longitude", "latitude", "plantingYear", "NAME_1", "NAME_2"), all = TRUE), df_list1)
    head(merged_df1)
    
    df_list2 <- list(merged_df1, Topography_PointData, Soil_PointData)
    merged_df2 <- Reduce(function(x, y) merge(x, y, by = c("longitude", "latitude", "NAME_1", "NAME_2"), all = TRUE), df_list2)
   
    
    merged_df2 <- merged_df2[!merged_df2$plantingYear %in% c(1979, 1980, 2023), ] ## not all layers have data for 1979 and 1980 and 2023 is incomplete
    merged_df2 <- merged_df2[order(merged_df2$plantingYear), ]
   
  }
  
  # merged_df2 <- merged_df2[complete.cases(merged_df2), ]
  Planting_month_date <- gsub("-", "_", Planting_month_date)
  
  fname <- ifelse(AOI == TRUE, paste("geoSpatial_4ML_AOI_", Planting_month_date, ".RDS", sep=""), "geoSpatial_4ML_trial.RDS")
  
  saveRDS(merged_df2, paste(pathOut1, fname , sep="/"))
  # saveRDS(object = merged_df2, file=paste(pathOut2, fname, sep=""))
  saveRDS(merged_df2, paste(pathOut3, fname, sep="/"))
  
  return(merged_df2)
  
  
}




#' Title read the training (trial) and prediction(target area) data, add AEZ on both and aggregate the prediction data across years.
#' the aggregation across years at this point has no purpose, but later when selected years matching the season forecast is implemented 
#' it will be used to aggregte data across those selected years. This is yet to be tested and revised.   
#'
#' @param country 
#' @param useCaseName 
#' @param Crop 
#' @param weather if true it will merge whether data otherwise it returns only soil and DEM variables 
#' @param AOI area of interest or area for which we are developing the recommendation for
#' @param Planting_month_date the rest data weather data is to be defined by the planting and months and date
#'
#' @return
#'
#' @examples ML_dataPrepartion(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", Planting_month_date = "08-08")
ML_dataPrepartion <- function (country, useCaseName, Crop, weather = FALSE, AOI = FALSE, Planting_month_date = NULL){
  
  ###############################
  # 1. define path for input and output
  ###############################
  
  pathIn <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/data_4ML/", sep="")
  
  pathOut1 <- paste("~/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")

  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  
  ###############################
  # 2. read data
  ###############################
  
  
  if(AOI == TRUE){
    Planting_month_date <- gsub("-", "_", Planting_month_date)
    ML_Data <- readRDS(paste(pathIn, "geoSpatial_4ML_AOI_", Planting_month_date, ".RDS", sep=""))
  }else{
    ML_Data <- readRDS(paste(pathIn, "geoSpatial_4ML_trial.RDS", sep=""))
  }
  
  
  fixedVars <- c("lon", "lat", "NAME_1", "NAME_2", "altitude", "slope", "TPI", "TRI",
                 "c_tot_top", "c_tot_bottom", "ca_top", "ca_bottom", "clay_tot_psa_top", "clay_tot_psa_bottom", 
                 "db_od_top", "db_od_bottom", "ecec_f_top", "ecec_f_bottom", "k_top", "k_bottom", "mg_top", "mg_bottom", 
                 "n_tot_ncs_top", "n_tot_ncs_bottom", "oc_top", "oc_bottom", "p_top", "p_bottom", "ph_h2o_top", "ph_h2o_bottom",
                 "s_top", "s_bottom", "sand_tot_psa_top", "sand_tot_psa_bottom", "silt_tot_psa_top", "silt_tot_psa_bottom", "SOM_top",
                 "SOM_bottom", "PWP_top", "PWP_bottom", "FC_top", "FC_bottom", "SWS_top", "SWS_bottom", "K_0_30", "N_0_30", "P_0_30", 
                 "Ptot_0_30", "texture_class_top", "texture_class_bottom")
  
  weatherVars <- c("longitude", "latitude", "plantingYear" ,"totalRF", "nrRainyDays", "Rain_month1", "Rain_month2", "Rain_month3", "Rain_month4", 
                   "Tmax_mean", "Tmax_month1", "Tmax_month2", "Tmax_month3", "Tmax_month4", 
                   "Tmin_mean", "Tmin_month1", "Tmin_month2", "Tmin_month3", "Tmin_month4")
  
  
  ###############################
  # 3. aggregate weather data across years 
  ###############################
  if(AOI ==TRUE){
    
    ML_Data_fixVars <- unique(ML_Data[, fixedVars])
    ML_Data_weatherVars <- ML_Data[, weatherVars]
    ## the aggregation will be done only for selected years based on 
    ## the season forecast: dry, wet or average years
    ML_Data_weatherVarsS <- suppressWarnings(as.data.frame(ML_Data_weatherVars %>% 
                                                             group_by(longitude, latitude) %>%
                                                             summarise_all(median) %>%
                                                             select(-plantingYear)))
    ML_Data <- unique(merge(ML_Data_fixVars, ML_Data_weatherVarsS, by=c("longitude", "latitude")))
  }
  
  if(AOI == FALSE){
    ML_Data <- ML_Data %>%
      mutate(longitude = as.numeric(lon),
             latitude = as.numeric(lat))
  } 
  
  ###############################
  # 4. the AOI data should be filtered for the desired AEZ in RAB 
  ###############################
  AEZ <- readOGR(dsn="~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/Landing/AEZ",  layer="AEZ_DEM_Dissolve")
  RW_aez <- spTransform(AEZ, CRS( "+proj=longlat +ellps=WGS84 +datum=WGS84"))
  RAW_AEZ_AOI <- suppressWarnings(raster::extract(RW_aez, ML_Data[, c("longitude", "latitude")]))
  ML_Data <- cbind(RAW_AEZ_AOI, ML_Data)
  ML_Data <- ML_Data %>%
    dplyr::select(-c(id.y, AEZs_no)) %>%
    mutate(AEZ = as.factor(Names_AEZs))
  
  fname <- ifelse(AOI == TRUE, paste("geoSpatial_Vmore_AOI_", Planting_month_date, ".RDS", sep=""), "modelReady_trial.RDS")
  
  saveRDS(ML_Data, paste(pathOut1, fname, sep=""))
  
  
  return(ML_Data)
  
}


























# 3. Join Geospatial for ML based on raster data ---------------------------
###########################################################################
# Will be used for spatialization and prediction.

#' Title
#'
#' @param country country name
#' @param useCaseName use case name  name
#' @param Crop the name of the crop to be used in creating file name to write out the result.
#' @param AOI True if the data is required for target area, and false if it is for the entire country
#' @param Planting_month_date is needed only for AOI and should be Rain_monthly provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' @param dataSourceR is among c("CHIRPS", "AgEra") for rainfall
#' @param dataSourceT is among c("chirts", "AgEra") for temperature
#' @param scenario TRUE if we want to have the information for dry, average, wet years
#' @param Year Only if Scenario = FALSE to work on a specific year
#' @param season for the season
#'
#'
#' @return
#' @export
#'
#' @examples join_geospatial_4ML_raster(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = TRUE , 
#' dataSource = "CHIRPS", Planting_month_date = "02_05", ID = "TLID")

join_geospatial_4ML_raster <- function(country, useCaseName, Crop, AOI, Planting_month_date, dataSourceR, 
                                       dataSourceT,  scenario, Year, season){
  
  # 3.1. output data ####
  
  ## Create directories to save output 
  pathOut1 <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/", sep="")
  pathOut3 <- paste("/home/jovyan/agwise-responsefunctions/dataops/responsefunctions/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/", sep="")
  
  
  if (!dir.exists(pathOut1)){
    dir.create(file.path(pathOut1), recursive = TRUE)
  }
  
  if (!dir.exists(pathOut3)){
    dir.create(file.path(pathOut3), recursive = TRUE)
  }
  
  # 3.2. input data ####
  
  ## Case AOI = FALSE (targeted area is the country)
  if (AOI == FALSE){
    
    ## Topography
    Topography_dem<- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Topography/dem.tif", sep=""))
    Topography_slope <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Topography/slope.tif", sep=""))
    Topography_tpi<- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Topography/tpi.tif", sep=""))
    Topography_tri<- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Topography/tri.tif", sep=""))
    
    ## Soil (47 bandes)
    Soils <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Soil/soils_transformed.tif", sep=""))
    
    ## Data with temporal dimensions
    ## Case Scenario = FALSE
    if (scenario == FALSE){

      ## Rainfall
      if(dataSourceR == "CHIRPS"){
        Rainfall_summaries <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_Raster_summaries_Country_", season, "_", Planting_month_date, "_CHIRPS_", Year, ".tif", sep=""))
      }else{
        Rainfall_summaries <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_Raster_summaries_Country_", season, "_", Planting_month_date, "_AgEra_", Year, ".tif", sep=""))
      }
      
      ## Temperature
      if(dataSourceT == "AgEra"){
        Temperature_summaries <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Temperature_Raster_summaries_Country_", season, "_", Planting_month_date, "_AgEra_", Year, ".tif", sep=""))
      }else{
        Temperature_summaries <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Temperature_Raster_summaries_Country_", season, "_", Planting_month_date, "_chirts_", Year, ".tif", sep=""))
      }
    }
  
      ## Case Scenario = TRUE
      if (scenario == TRUE) {
        
        ## Rainfall
        if(dataSourceR == "CHIRPS"){
          Rainfall_summaries_below <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Below_Rainfall_Raster_scenario_Country_", season, "_", Planting_month_date, "_CHIRPS.tif", sep=""))
          Rainfall_summaries_average <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Average_Rainfall_Raster_scenario_Country_", season, "_", Planting_month_date, "_CHIRPS.tif", sep=""))
          Rainfall_summaries_above <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Above_Rainfall_Raster_scenario_Country_", season, "_", Planting_month_date, "_CHIRPS.tif", sep=""))
        }else{
          Rainfall_summaries_below <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Below_Rainfall_Raster_scenario_Country_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
          Rainfall_summaries_average <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Average_Rainfall_Raster_scenario_Country_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
          Rainfall_summaries_above <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Above_Rainfall_Raster_scenario_Country_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
        }
        
        ## Temperature
        if(dataSourceT == "AgEra"){
          Temperature_summaries_below <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Below_Temperature_Raster_scenario_Country_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
          Temperature_summaries_average <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Average_Temperature_Raster_scenario_Country_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
          Temperature_summaries_above <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Above_Temperature_Raster_scenario_Country_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
        }else{
          Temperature_summaries_below <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Below_Temperature_Raster_scenario_Country_", season, "_", Planting_month_date, "_chirts.tif", sep=""))
          Temperature_summaries_average <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Average_Temperature_Raster_scenario_Country_", season, "_", Planting_month_date, "_chirts.tif", sep=""))
          Temperature_summaries_above <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Above_Temperature_Raster_scenario_Country_", season, "_", Planting_month_date, "_chirts.tif", sep=""))
        }
      }
  }
  
  ## Case AOI = TRUE (targeted area is the AOI)
  if (AOI == TRUE) {
    
    ## Topography
    Topography_dem<- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Topography/dem.tif", sep=""))
    Topography_slope <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Topography/slope.tif", sep=""))
    Topography_tpi<- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Topography/tpi.tif", sep=""))
    Topography_tri<- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Topography/tri.tif", sep=""))
    
    ## Soil (47 bandes)
    Soils <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/transform/Soil/soils_transformed.tif", sep=""))
    
    ## Case Scenario = FALSE
    if (scenario == FALSE){
      
      ## Rainfall
      if(dataSourceR == "CHIRPS"){
        Rainfall_summaries <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_Raster_summaries_AOI_", season, "_", Planting_month_date, "_CHIRPS_", Year, ".tif", sep=""))
      }else{
        Rainfall_summaries <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Rainfall_Raster_summaries_AOI_", season, "_", Planting_month_date, "_AgEra_", Year, ".tif", sep=""))
      }
      
      ## Temperature
      if(dataSourceT == "AgEra"){
        Temperature_summaries <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Temperature_Raster_summaries_AOI_", season, "_", Planting_month_date, "_AgEra_", Year, ".tif", sep=""))
      }else{
        Temperature_summaries <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Temperature_Raster_summaries_AOI_", season, "_", Planting_month_date, "_chirts_", Year, ".tif", sep=""))
      }
    }
    
    ## Case Scenario = TRUE
    if (scenario == TRUE) {
      
      ## Rainfall
      if(dataSourceR == "CHIRPS"){
        Rainfall_summaries_below <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Below_Rainfall_Raster_scenario_AOI_", season, "_", Planting_month_date, "_CHIRPS.tif", sep=""))
        Rainfall_summaries_average <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Average_Rainfall_Raster_scenario_AOI_", season, "_", Planting_month_date, "_CHIRPS.tif", sep=""))
        Rainfall_summaries_above <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Above_Rainfall_Raster_scenario_AOI_", season, "_", Planting_month_date, "_CHIRPS.tif", sep=""))
      }else{
        Rainfall_summaries_below <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Below_Rainfall_Raster_scenario_AOI_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
        Rainfall_summaries_average <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Average_Rainfall_Raster_scenario_AOI_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
        Rainfall_summaries_above <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Rainfall/Above_Rainfall_Raster_scenario_AOI_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
      }
      
      ## Temperature
      if(dataSourceT == "AgEra"){
        Temperature_summaries_below <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Below_Temperature_Raster_scenario_AOI_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
        Temperature_summaries_average <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Average_Temperature_Raster_scenario_AOI_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
        Temperature_summaries_above <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Above_Temperature_Raster_scenario_AOI_", season, "_", Planting_month_date, "_AgEra.tif", sep=""))
      }else{
        Temperature_summaries_below <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Below_Temperature_Raster_scenario_AOI_", season, "_", Planting_month_date, "_chirts.tif", sep=""))
        Temperature_summaries_average <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Average_Temperature_Raster_scenario_AOI_", season, "_", Planting_month_date, "_chirts.tif", sep=""))
        Temperature_summaries_above <- terra::rast(paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/Temperature/Above_Temperature_Raster_scenario_AOI_", season, "_", Planting_month_date, "_chirts.tif", sep=""))
      }
    }
  }
  
  # 3.3. Reshape and merge data ####
  ## Case AOI = FALSE (targeted area is the country)
  if (AOI == FALSE){
    
    ## Case Scenario = FALSE
    if (scenario ==FALSE){
      
      # Topography
      Topography_dem_r<-terra::resample(Topography_dem, Temperature_summaries, threads=TRUE, method="near")
      names(Topography_dem_r)<-"dem"
      Topography_slope_r <- terra::resample(Topography_slope, Temperature_summaries, threads=TRUE, method="near")
      Topography_tpi_r <- terra::resample(Topography_tpi, Temperature_summaries, threads=TRUE, method="near")
      Topography_tri_r <- terra::resample(Topography_tri, Temperature_summaries, threads=TRUE, method="near")
      
      # Soils 
      Soils_r <-terra::resample(Soils, Temperature_summaries, threads=TRUE, method="near")
      
      # Rainfall
      Rainfall_summaries_r <- terra::resample(Rainfall_summaries, Temperature_summaries, threads=TRUE, method="near")
      
      # Merge and save
      merged.raster<-c(Topography_dem_r, Topography_slope_r, Topography_tpi_r, Topography_tri_r, Soils_r, Rainfall_summaries_r, Temperature_summaries)
      fname<- paste("linked_geospatial_Raster_summaries_Country_",Planting_month_date, "_", Year,".tif",sep="")
      terra::writeRaster(merged.raster, paste(pathOut1, fname, sep="/"), filetype="GTiff", overwrite=T)
      terra::writeRaster(merged.raster, paste(pathOut3, fname, sep="/"), filetype="GTiff", overwrite=T)
    }
    
    ## Case Scenario = TRUE
    if (scenario ==TRUE){
      
      # Topography
      Topography_dem_r<-terra::resample(Topography_dem, Temperature_summaries_below, threads=TRUE, method="near")
      names(Topography_dem_r)<-"dem"
      Topography_slope_r <- terra::resample(Topography_slope, Temperature_summaries_below, threads=TRUE, method="near")
      Topography_tpi_r <- terra::resample(Topography_tpi, Temperature_summaries_below, threads=TRUE, method="near")
      Topography_tri_r <- terra::resample(Topography_tri, Temperature_summaries_below, threads=TRUE, method="near")
      
      # Soils 
      Soils_r <-terra::resample(Soils, Temperature_summaries_below, threads=TRUE, method="near")
      
      # Rainfall
      Rainfall_summaries_r1 <- terra::resample(Rainfall_summaries_below, Temperature_summaries_below, threads=TRUE, method="near")
      Rainfall_summaries_r2 <- terra::resample(Rainfall_summaries_average, Temperature_summaries_below, threads=TRUE, method="near")
      Rainfall_summaries_r3 <- terra::resample(Rainfall_summaries_above, Temperature_summaries_below, threads=TRUE, method="near")
      
       # Merge and save
      merged.raster1<-c(Topography_dem_r, Topography_slope_r, Topography_tpi_r, Topography_tri_r, Soils_r, Rainfall_summaries_r1, Temperature_summaries_below)
      fname1<- paste("Below_linked_geospatial_Raster_scenario_Country_",Planting_month_date, ".tif",sep="")
      terra::writeRaster(merged.raster1, paste(pathOut1, fname1, sep="/"), filetype="GTiff", overwrite=T)
      terra::writeRaster(merged.raster1, paste(pathOut3, fname1, sep="/"), filetype="GTiff", overwrite=T)

      merged.raster2<-c(Topography_dem_r, Topography_slope_r, Topography_tpi_r, Topography_tri_r, Soils_r, Rainfall_summaries_r2, Temperature_summaries_average)
      fname2<- paste("Average_linked_geospatial_Raster_scenario_Country_",Planting_month_date, ".tif",sep="")
      terra::writeRaster(merged.raster2, paste(pathOut1, fname2, sep="/"), filetype="GTiff", overwrite=T)
      terra::writeRaster(merged.raster2, paste(pathOut3, fname2, sep="/"), filetype="GTiff", overwrite=T)
      
      merged.raster3<-c(Topography_dem_r, Topography_slope_r, Topography_tpi_r, Topography_tri_r, Soils_r, Rainfall_summaries_r3, Temperature_summaries_above)
      fname3<- paste("Above_linked_geospatial_Raster_scenario_Country_",Planting_month_date, ".tif",sep="")
      terra::writeRaster(merged.raster3, paste(pathOut1, fname3, sep="/"), filetype="GTiff", overwrite=T)
      terra::writeRaster(merged.raster3, paste(pathOut3, fname3, sep="/"), filetype="GTiff", overwrite=T)
  }
      
        
  }
  
  ## Case AOI = TRUE (targeted area is the AOI) - Need to crop Topography and Soil variables on AOI extent
  if (AOI == TRUE){
    
    ## Case Scenario = FALSE
    if (scenario ==FALSE){
      
      # Topography
      Topography_dem_r<- terra::crop(Topography_dem, Temperature_summaries)
      Topography_dem_r<-terra::resample(Topography_dem_r, Temperature_summaries, threads=TRUE, method="near")
      names(Topography_dem_r)<-"dem"
      Topography_slope_r<- terra::crop(Topography_slope, Temperature_summaries)
      Topography_slope_r <- terra::resample(Topography_slope_r, Temperature_summaries, threads=TRUE, method="near")
      Topography_tpi_r<- terra::crop(Topography_tpi, Temperature_summaries)
      Topography_tpi_r <- terra::resample(Topography_tpi_r, Temperature_summaries, threads=TRUE, method="near")
      Topography_tri_r<- terra::crop(Topography_tri, Temperature_summaries)
      Topography_tri_r <- terra::resample(Topography_tri_r, Temperature_summaries, threads=TRUE, method="near")
      
      # Soils 
      Soils_r <-terra::crop(Soils, Temperature_summaries)
      Soils_r <-terra::resample(Soils_r, Temperature_summaries, threads=TRUE, method="near")
      
      # Rainfall
      Rainfall_summaries_r <- terra::resample(Rainfall_summaries, Temperature_summaries, threads=TRUE, method="near")
      
      # Merge and save
      merged.raster<-c(Topography_dem_r, Topography_slope_r, Topography_tpi_r, Topography_tri_r, Soils_r, Rainfall_summaries_r, Temperature_summaries)
      fname<- paste("linked_geospatial_Raster_summaries_AOI_",Planting_month_date, "_", Year,".tif",sep="")
      terra::writeRaster(merged.raster, paste(pathOut1, fname, sep="/"), filetype="GTiff", overwrite=T)
      terra::writeRaster(merged.raster, paste(pathOut3, fname, sep="/"), filetype="GTiff", overwrite=T)
    }
    
    ## Case Scenario = TRUE
    if (scenario ==TRUE){
      
      # Topography
      Topography_dem_r<- terra::crop(Topography_dem, Temperature_summaries_below)
      Topography_dem_r<-terra::resample(Topography_dem_r, Temperature_summaries_below, threads=TRUE, method="near")
      names(Topography_dem_r)<-"dem"
      Topography_slope_r<- terra::crop(Topography_slope, Temperature_summaries_below)
      Topography_slope_r <- terra::resample(Topography_slope_r, Temperature_summaries_below, threads=TRUE, method="near")
      Topography_tpi_r<- terra::crop(Topography_tpi, Temperature_summaries_below)
      Topography_tpi_r <- terra::resample(Topography_tpi_r, Temperature_summaries_below, threads=TRUE, method="near")
      Topography_tri_r<- terra::crop(Topography_tri, Temperature_summaries_below)
      Topography_tri_r <- terra::resample(Topography_tri_r, Temperature_summaries_below, threads=TRUE, method="near")
      
      # Soils 
      Soils_r<- terra::crop(Soils, Temperature_summaries_below)
      Soils_r <-terra::resample(Soils_r, Temperature_summaries_below, threads=TRUE, method="near")
      
      # Rainfall
      Rainfall_summaries_r1 <- terra::resample(Rainfall_summaries_below, Temperature_summaries_below, threads=TRUE, method="near")
      Rainfall_summaries_r2 <- terra::resample(Rainfall_summaries_average, Temperature_summaries_below, threads=TRUE, method="near")
      Rainfall_summaries_r3 <- terra::resample(Rainfall_summaries_above, Temperature_summaries_below, threads=TRUE, method="near")
      
      # Merge and save
      merged.raster1<-c(Topography_dem_r, Topography_slope_r, Topography_tpi_r, Topography_tri_r, Soils_r, Rainfall_summaries_r1, Temperature_summaries_below)
      fname1<- paste("Below_linked_geospatial_Raster_scenario_AOI_",Planting_month_date, ".tif",sep="")
      terra::writeRaster(merged.raster1, paste(pathOut1, fname1, sep="/"), filetype="GTiff", overwrite=T)
      terra::writeRaster(merged.raster1, paste(pathOut3, fname1, sep="/"), filetype="GTiff", overwrite=T)
      
      merged.raster2<-c(Topography_dem_r, Topography_slope_r, Topography_tpi_r, Topography_tri_r, Soils_r, Rainfall_summaries_r2, Temperature_summaries_average)
      fname2<- paste("Average_linked_geospatial_Raster_scenario_AOI_",Planting_month_date, ".tif",sep="")
      terra::writeRaster(merged.raster2, paste(pathOut1, fname2, sep="/"), filetype="GTiff", overwrite=T)
      terra::writeRaster(merged.raster2, paste(pathOut3, fname2, sep="/"), filetype="GTiff", overwrite=T)
      
      merged.raster3<-c(Topography_dem_r, Topography_slope_r, Topography_tpi_r, Topography_tri_r, Soils_r, Rainfall_summaries_r3, Temperature_summaries_above)
      fname3<- paste("Above_linked_geospatial_Raster_scenario_AOI_",Planting_month_date, ".tif",sep="")
      terra::writeRaster(merged.raster3, paste(pathOut1, fname3, sep="/"), filetype="GTiff", overwrite=T)
      terra::writeRaster(merged.raster3, paste(pathOut3, fname3, sep="/"), filetype="GTiff", overwrite=T)
    }
    
  }

}
# 
# country = "Rwanda"
# useCaseName = "RAB"
# Crop = "Potato"
# AOI = TRUE  
# dataSourceR = "CHIRPS"
# dataSourceT = "AgEra"
# Planting_month_date = "02_05"
# scenario =TRUE
# season = "season1"
# Year = 1982
# 
# join_geospatial_4ML_raster(country, useCaseName, Crop, AOI, Planting_month_date, dataSourceR, dataSourceT, scenario, Year, season)
