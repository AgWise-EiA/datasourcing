#' 
#' #################################################################################################################
#' ## sourcing required packages 
#' #################################################################################################################
#' packages_required <- c("doParallel", "foreach", "chirps", "tidyverse", "dplyr", "lubridate", "stringr","sf","purrr","DSSAT")
#' 
#' # check and install packages that are not yet installed
#' installed_packages <- packages_required %in% rownames(installed.packages())
#' if(any(installed_packages == FALSE)){
#'   install.packages(packages_required[!installed_packages])}
#' 
#' # load required packages
#' invisible(lapply(packages_required, library, character.only = TRUE))
#' 
#' 
#' 
#' 
#' 
#' #' Title readin the data for crop model
#' #'
#' #' @param country country name
#' #' @param useCaseName use case name  name
#' #' @param Crop the name of the crop to be used in creating file name to write out the result.
#' #' @param AOI True if the data is required for target area, and false if it is for trial sites
#' #' @param Planting_month_date is needed only for AOI and should be provided as month_date, for trial locations the actual planting date is be used so no need to change the default value
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples readGeo_CM(country = "Rwanda",  useCaseName = "RAB", Crop = "Maize", AOI = FALSE,  Planting_month_date = NULL)
#' readGeo_CM <- function(country, useCaseName, Crop, AOI = FALSE, Planting_month_date=NULL,jobs=10){
#'   
#'   # pathIn <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/geo_4cropModel/", sep="")
#'   # pathInSoil <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_",  country, "_",useCaseName, "/", Crop, "/raw/Soil/profile/", sep="")
#'   # pathInDEM <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_",  country, "_",useCaseName, "/", Crop, "/raw/Topography/", sep="")
#'   
#'   pathIn <- paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/profile", sep="")
#'   
#' 
#'   if(AOI == TRUE){
#'     Rainfall <- readRDS(paste(pathIn, "Rainfall/Rainfall_4CM_AOI_", Planting_month_date, "_CHIRPS.RDS", sep=""))
#'     SolarRadiation <- readRDS(paste(pathIn, "SolarRadiation/SolarRadiation_4CM_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
#'     TemperatureMax <- readRDS(paste(pathIn, "TemperatureMax/TemperatureMax_4CM_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
#'     Temperaturemin <- readRDS(paste(pathIn, "TemperatureMin/TemperatureMin_4CM_AOI_", Planting_month_date, "_AgEra.RDS", sep=""))
#'     Soil <- readRDS(paste(pathInSoil, "Soil_PointData_AOI.RDS", sep=""))
#'     DEM <- readRDS(paste(pathInDEM, "Topography_PointData_AOI.RDS", sep=""))
#'   }else{
#'     Rainfall <- readRDS(paste(pathIn, "Rainfall/Rainfall_4CM_trial_CHIRPS.RDS", sep=""))
#'     SolarRadiation <- readRDS(paste(pathIn, "SolarRadiation/SolarRadiation_4CM_trial_AgEra.RDS", sep=""))
#'     TemperatureMax <- readRDS(paste(pathIn, "TemperatureMax/TemperatureMax_4CM_trial_AgEra.RDS", sep=""))
#'     TemperatureMin <- readRDS(paste(pathIn, "TemperatureMin/TemperatureMin_4CM_trial_AgEra.RDS", sep=""))
#'     Soil <- readRDS(paste(pathInSoil, "Soil_PointData_trial.RDS", sep=""))
#'     DEM <- readRDS(paste(pathInDEM, "Topography_PointData_trial.RDS", sep=""))
#'   }
#'   
#' 
#'   metaDataWeather <- as.data.frame(t(Rainfall[1:5, ]))
#'   metaDataWeather$RowNames <- rownames(metaDataWeather)
#'   head(metaDataWeather)
#'   metaData_Soil <-Soil[,c("longitude", "latitude","ID","NAME_1","NAME_2")]
#'   
#'   metaData <- merge(metaDataWeather,metaData_Soil)
#'   rownames(metaData) <- metaData$RowNames
#'   organized <-colnames(Rainfall)[!(colnames(Rainfall) %in% c("MetaDVar","Date","Month","Year"))]
#'   sortingIndices <- match(organized, rownames(metaData))
#'     # Sort metaData based on sorting indices
#'   metaData <- metaData[sortingIndices, ]
#' 
#'   #Keep all the soil data with rainfall data
#'   Soil <- merge(metaData,Soil)
#'   sortingIndices <- match(organized, Soil$RowNames)
#'   Soil <- Soil[sortingIndices, ]
#'   
#'   test <- as.data.frame(t(metaData))
#'   #Keep all the weather data that has soil data
#'   a <- c(colnames(test), c("MetaDVar","Date","Month","Year"))
#'   Rainfall <- Rainfall[,colnames(Rainfall) %in% a]
#'   SolarRadiation <- SolarRadiation[,colnames(SolarRadiation) %in% a]
#'   TemperatureMax <- TemperatureMax[,colnames(TemperatureMax) %in% a]
#'   TemperatureMin <- TemperatureMin[,colnames(TemperatureMin) %in% a]
#'   
#'   #Eliminate metadata from weather data
#'   Rainfall <- Rainfall[6:nrow(Rainfall), ]
#'   SolarRadiation <- SolarRadiation[6:nrow(SolarRadiation), ]
#'   TemperatureMax <- TemperatureMax[6:nrow(TemperatureMax), ]
#'   TemperatureMin <- TemperatureMin[6:nrow(TemperatureMin), ]
#'   
#'   #return(list(Rainfall, SolarRadiation, TemperatureMax, TemperatureMin,Soil,metaData))
#'   #cls <- parallel::makePSOCKcluster(jobs)
#'   #doParallel::registerDoParallel(cls)
#'   #Set working directory to save the results
#'   path.to.extdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/DSSAT", sep="")
#'   
#'   #Define working directory with template data
#'   path.to.temdata <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/Landing/DSSAT", sep="")
#'   
#'   
#'   
#'   if (!dir.exists(path.to.extdata)){
#'     dir.create(file.path(path.to.extdata), recursive = TRUE)
#'   }
#'   setwd(path.to.extdata)
#'   
#'   #Bring data together
#'   Tmaxdata <- TemperatureMax
#'   Tmindata <- TemperatureMin
#'   Sraddata <- SolarRadiation
#'   Rainfalldata <- Rainfall
#'   coords <- metaData
#'   grid <- as.matrix(coords)
#'   
#'   
#'   
#'   process_grid_element <- function(i) {
#'     if (!dir.exists(file.path(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")))){
#'       dir.create(file.path(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/")), recursive = TRUE)
#'     }
#'     setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
#'     
#'     DATE <-Tmaxdata$MetaDVar
#'     DATE <- gsub("_", "-",DATE)
#'     DATE <- as.Date(DATE, format="%d-%m-%Y")
#'     TMAX <-Tmaxdata[,i]
#'     TMIN <-Tmindata[,i]
#'     SRAD <-Sraddata[,i]
#'     RAIN <-Rainfalldata[,i]
#'     
#'     tst <- data.frame(DATE,TMAX,TMIN,SRAD,RAIN)
#'     tst  <- mutate(tst , across(c(TMAX,TMIN,SRAD,RAIN), as.numeric))
#'     ##########################################
#'     #Read data from local machine
#'     tst$DATE <- as.POSIXct(tst$DATE, format = "%Y-%m-%d", tz = "UTC")
#'     
#'     # Calculate long-term average temperature (TAV)
#'     tav <- tst %>%
#'       summarize(TAV=mean((TMAX+TMIN)/2,na.rm=T))
#'     
#'     # Calculate monthly temperature amplitude (AMP)
#'     amp <- tst %>%
#'       # Extract month from DATE column
#'       mutate(month = month(as.Date(tst$DATE,format = "%y%j"))) %>%
#'       # Group data by month
#'       group_by(month) %>%
#'       # Calculate monthly means
#'       summarize(monthly_avg = mean((TMAX+TMIN)/2,na.rm=T)) %>%
#'       # Calculate AMP as half the difference between minimum and
#'       #     maximum monthly temperature
#'       summarize(AMP = (max(monthly_avg, na.rm=T)-min(monthly_avg,na.rm=T))/2)
#'     #Convert SRAD from Jm-2d-1 to MJm-2d-1 
#'     tst$SRAD <- tst$SRAD*1e-6
#'     
#'     #Get elevation
#'     elev <- DEM$altitude[which(DEM$longitude==as.numeric(coords[i, 1]) & DEM$latitude==as.numeric(coords[i, 2]))]
#'     elev <-ifelse(length(elev) >0,
#'                    DEM$altitude[which(DEM$longitude==as.numeric(coords[i, 1]) & DEM$latitude==as.numeric(coords[i, 2]))],
#'                    -99)
#'     # Generate new general information table
#'     general_new <- tibble(
#'       INSI = "RWAN",
#'       LAT = as.numeric(coords[i, 2]),
#'       LONG = as.numeric(coords[i, 1]),
#'       ELEV = elev,
#'       TAV = tav,
#'       AMP = amp,
#'       REFHT = 2,
#'       WNDHT = 2
#'     )
#'     
#'     # Add station information 
#'     attr(tst, "GENERAL") <- general_new
#'     
#'     DSSAT::write_wth(tst, paste0("WHTE", formatC(width = 4, (as.integer(i)), flag = "0"), ".WTH"))
#'     
#'     ##########################################
#'     # Get soil ISRIC data from server
#'     Depth<-c(5,15,30,60,100,200)
#'     LL15 <-as.numeric(Soil[i,c("PWP_0-5cm","PWP_5-15cm","PWP_15-30cm","PWP_30-60cm","PWP_60-100cm","PWP_100-200cm")])
#'     DUL  <-as.numeric(Soil[i,c("FC_0-5cm","FC_5-15cm","FC_15-30cm","FC_30-60cm","FC_60-100cm","FC_100-200cm")])
#'     SAT  <-as.numeric(Soil[i,c("SWS_0-5cm","SWS_5-15cm","SWS_15-30cm","SWS_30-60cm","SWS_60-100cm","SWS_100-200cm")])
#'     SKS  <-as.numeric(Soil[i,c("KS_0-5cm","KS_5-15cm","KS_15-30cm","KS_30-60cm","KS_60-100cm","KS_100-200cm")])
#'     SSS  <-round(as.numeric(SKS), digits = 1)
#'     BDM  <- as.numeric(Soil[i,c("bdod_0-5cm","bdod_5-15cm","bdod_15-30cm","bdod_30-60cm","bdod_60-100cm","bdod_100-200cm")])
#'     LOC  <- as.numeric((Soil[i,c("soc_0-5cm","soc_5-15cm","soc_15-30cm","soc_30-60cm","soc_60-100cm","soc_100-200cm")])/10)
#'     LCL  <- as.numeric(Soil[i,c("clay_0-5cm","clay_5-15cm","clay_15-30cm","clay_30-60cm","clay_60-100cm","clay_100-200cm")])
#'     LSI  <- as.numeric(Soil[i,c("silt_0-5cm","silt_5-15cm","silt_15-30cm","silt_30-60cm","silt_60-100cm","silt_100-200cm")])
#'     LNI  <- c(as.numeric((Soil[i,c("nitrogen_0-5cm","nitrogen_5-15cm","nitrogen_15-30cm","nitrogen_30-60cm")])/10),-99,-99)
#'     LHW  <- as.numeric(Soil[i,c("phh2o_0-5cm","phh2o_5-15cm","phh2o_15-30cm","phh2o_30-60cm","phh2o_60-100cm","phh2o_100-200cm")])
#'     
#'     
#'     
#'     ##### Runoff curve no. [Soil Conservation Service/NRCS] #####
#'     ##### Equations from apsimx package but using second layer for soil texture
#'     #' @description Texture triangle as equations
#'     #' @details It requires the silt and clay percentages to define the texture class
#'     #
#'     #' Title getting the texture class 
#'     #'
#'     #' @param usda_clay percentage of clay (as index or /100)
#'     #' @param usda_silt percentage of silt (as index or /100)
#'     #' @return class (texture class)
#'     #' @examples texture_class(clay,silt)
#'     #' 
#'     texture_class <- function (usda_clay, usda_silt ) {
#'       
#'       if(usda_clay < 0 || usda_clay > 1) stop("usda_clay should be between 0 and 1")
#'       if(usda_silt < 0 || usda_silt > 1) stop("usda_silt should be between 0 and 1")
#'       
#'       intl_clay <- usda_clay
#'       intl_silt <- usda_silt
#'       intl_sand <- 1.0 - intl_clay - intl_silt
#'       
#'       if ((intl_sand < 0.75 - intl_clay) && (intl_clay >= 0.40)) {
#'         class <- "silty clay"
#'       } else if ((intl_sand < 0.75 - intl_clay) && (intl_clay >= 0.26)) {
#'         class <- "silty clay loam"
#'       } else if (intl_sand < 0.75 - intl_clay) {
#'         class <- "silty loam"
#'       } else if ((intl_clay >= 0.40 + (0.305-0.40)/(0.635-0.35) * (intl_sand-0.35)) && (intl_clay < 0.50 + (0.305-0.50)/(0.635-0.50) * (intl_sand - 0.50))) {
#'         class <- "clay"
#'       } else if (intl_clay >= 0.26 + (0.305-0.26)/(0.635-0.74) * (intl_sand-0.74)) {
#'         class <- "sandy clay"
#'       } else if ((intl_clay >= 0.26 + (0.17-0.26)/(0.83-0.49) * (intl_sand-0.49)) && (intl_clay < 0.10 + (0.305-0.10)/(0.635-0.775) * (intl_sand - 0.775))) {
#'         class <- "clay loam"
#'       } else if (intl_clay >= 0.26 + (0.17-0.26)/(0.83-0.49) * (intl_sand-0.49)) {
#'         class <- "sandy clay loam"
#'       } else if ((intl_clay >= 0.10 + (0.12-0.10)/(0.63-0.775) * (intl_sand-0.775)) && (intl_clay < 0.10 + (0.305-0.10)/(0.635-0.775) * (intl_sand - 0.775))) {
#'         class <- "loam"
#'       } else if (intl_clay >= 0.10 + (0.12-0.10)/(0.63-0.775) * (intl_sand-0.775)) {
#'         class <- "sandy loam"
#'       } else if (intl_clay < 0.00 + (0.08-0.00)/(0.88-0.93) * (intl_sand-0.93)) {
#'         class <- "loamy sand"
#'       } else {
#'         class <- "sand"
#'       }  
#'       
#'       return( class )
#'     }
#'     
#'     #Currently texture based on layer 2 but it could be updated with layer one (specially for albedo)
#'     texture <- texture_class((LCL[2]/100), (LSI[2]/100))
#'     
#'     textureClasses <- c("clay", "silty clay", "sandy clay", "clay loam", "silty clay loam", "sandy clay loam", "loam", "silty loam", "sandy loam", "silt", "loamy sand", "sand", "NO DATA")  
#'     textureClasses_sum <- c("C", "SIC", "SC", "CL", "SICL", "SCL", "L", "SIL", "SL", "SI", "LS", "S", "NO DATA")  
#'     
#'     Albedo <- c(0.12, 0.12, 0.13, 0.13, 0.12, 0.13, 0.13, 0.14, 0.13, 0.13, 0.16, 0.19, 0.13)
#'     CN2 <- c(73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 73.0, 68.0, 73.0, 68.0, 68.0, 73.0)
#'     SWCON <- c(0.25, 0.3, 0.3, 0.4, 0.5, 0.5, 0.5, 0.5, 0.6, 0.5, 0.6, 0.75, 0.5)
#'     
#'     wtc <- which(textureClasses == texture)
#'     #Soil albedo
#'     ALB <- Albedo[wtc]
#'     #Runoff curve
#'     LRO <- CN2[wtc]
#'     
#'     texture_soil <- textureClasses_sum[wtc]
#'     
#'     #' Evaporation limit function from Ritchie et al. (1989); cited in Allen et al. (2005)
#'     #' @param clay1 Clay percentage for the top soil horizon
#'     #' @param sand1 Sand percentage for the top soil horizon
#'     #' @keywords internal
#'     #' @export 
#'     slu1 <- function(clay1,sand1) {
#'       ifelse(sand1>=80, (20-0.15*sand1), 
#'              ifelse(clay1>=50,(11-0.06*clay1),
#'                     (8-0.08*clay1)))
#'     }
#'     
#' 
#'     SLU <- slu1(Soil[i,c("clay_0-5cm")],Soil[i,c("sand_0-5cm")])
#' 
#'     shapefile <- st_read("~/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/HC27/HC27 CLASSES.shp", quiet= T)%>%
#'       st_make_valid()
#'     coordinates_df <- data.frame(latitude=as.numeric(Soil[i,c("latitude")]), longitude=as.numeric(Soil[i,c("longitude")]))
#'     coordinates_sf <- st_as_sf(coordinates_df, coords = c("longitude", "latitude"), crs = 4326)
#'     intersecting_polygons <- st_intersection(shapefile, coordinates_sf)
#'     
#'     hccode <- seq(1:27)
#'     LDRvalue  <- c(rep(0.2,9),rep(0.5,9),rep(0.75,9))
#'     hc <- which(hccode == intersecting_polygons$GRIDCODE)
#'     #Drainage rate
#'     LDR <- LDRvalue[hc]
#'     
#'     ex_profile <- suppressWarnings(DSSAT::read_sol(paste(path.to.temdata, "soil.sol", sep="/"), id_soil = "IBPN910025"))
#'     
#'     
#'     soilid <- ex_profile %>%
#'       mutate(PEDON=paste0('TRAN', formatC(width = 5, (as.integer(i)), flag = "0")),
#'              SOURCE = "ISRIC V2",
#'              TEXTURE = texture_soil,
#'              DESCRIPTION = texture,
#'              LAT = as.numeric(Soil[i,c("latitude")]),
#'              LONG = as.numeric(Soil[i,c("longitude")]),
#'              SALB = ALB,
#'              SLU1 = SLU,
#'              SLRO = LRO,
#'              SLDR = LDR, 
#'              SLB=Depth,
#'              SLLL=LL15,
#'              SSAT=SAT,
#'              SDUL=DUL,
#'              SSKS=SSS,
#'              SBDM=BDM,
#'              SLOC=LOC,
#'              SLCL=LCL,
#'              SLSI=LSI,
#'              SLNI=LNI,
#'              SLHW=LHW)
#'     
#'     setwd(paste(path.to.extdata,paste0('EXTE', formatC(width = 4, (as.integer(i)), flag = "0")), sep = "/"))
#'     DSSAT::write_sol(soilid, 'SOIL.SOL', append = FALSE)
#'   }
#'   results <- map(seq_along(grid[,1]), process_grid_element) %||% print("Progress:")
#' }




# get_data_4CropModels <- function(country, useCaseName, Crop, AOI = FALSE, jobs = 10){
# 
#   ## read point data for soil and weather
# 
#   if(AOI == TRUE){
#     SoilDEM_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/SoilDEM_PointData_AOI.RDS", sep=""))
#     windSpeed_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/windSpeed_PointData_AOI.RDS", sep=""))
#     solarRadiation_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/solarRadiation_PointData_AOI.RDS", sep=""))
#     relativeHumidity_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/relativeHumidity_PointData_AOI.RDS", sep=""))
#     temperatureMin_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/temperatureMin_PointData_AOI.RDS", sep=""))
#     temperatureMax_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/temperatureMax_PointData_AOI.RDS", sep=""))
#     Rainfall_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/Rainfall_PointData_AOI.RDS", sep=""))
# 
# 
#   }else{
#     SoilDEM_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/SoilDEM_PointData_trial.RDS", sep=""))
#     windSpeed_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/windSpeed_PointData_trial.RDS", sep=""))
#     solarRadiation_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/solarRadiation_PointData_trial.RDS", sep=""))
#     relativeHumidity_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/relativeHumidity_PointData_trial.RDS", sep=""))
#     temperatureMin_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/temperatureMin_PointData_trial.RDS", sep=""))
#     temperatureMax_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/temperatureMax_PointData_trial.RDS", sep=""))
#     Rainfall_PointData <- readRDS(paste("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/profile/Rainfall_PointData_trial.RDS", sep=""))
# 
# 
#   }
# 
# 
# 
# 
#   # Input rainfall
#   if(dataSource == "CHIRPS"){
#     listRaster_RF <-list.files(path=paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/", varName,"/chirps",sep=""), pattern=".nc$", full.names = TRUE)
#   }else{
#     listRaster_RF <-list.files(path=paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/",varName,"/AgEra", sep=""), pattern=".nc$", full.names = TRUE)
#   }
# 
#   # Creation of the output dir
#   pathOut1 <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/geo_4cropModel/",varName, sep="")
#   pathOut2 <- paste("/home/jovyan/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/geo_4cropModel/",varName, sep="")
# 
#   if (!dir.exists(pathOut1)){
#     dir.create(file.path(pathOut1), recursive = TRUE)
#   }
# 
#   if (!dir.exists(pathOut2)){
#     dir.create(file.path(pathOut2), recursive = TRUE)
#   }
# 
# 
# 
#   # Input point data AOI / Trial
#   if(AOI == TRUE){
#     # countryCoord <- readRDS(paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", Crop, "/raw/AOI_GPS.RDS", sep=""))
#     countryCoord <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_",useCaseName, "/", Crop, "/result/AOI_GPS.RDS", sep=""))
# 
#     countryCoord <- unique(countryCoord[, c("longitude", "latitude")])
#     countryCoord <- countryCoord[complete.cases(countryCoord), ]
# 
#     ## check if both planting and harvest dates are in the same year
#     Planting_month <- as.numeric(str_extract(Planting_month_date, "[^-]+"))
#     harvest_month <- as.numeric(str_extract(Harvest_month_date, "[^-]+"))
#     if(Planting_month < harvest_month){
#       planting_harvest_sameYear <- TRUE
#     }else{
#       planting_harvest_sameYear <- FALSE
#     }
# 
#     if (planting_harvest_sameYear ==TRUE){ #is used only to get the date of the year so the years 2001 and 2002 have no value except for formating
#       countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
#       countryCoord$harvestDate <- paste(2001, Harvest_month_date, sep="-")
#     }else{
#       countryCoord$plantingDate <- paste(2001, Planting_month_date, sep="-")
#       countryCoord$harvestDate <- paste(2002, Harvest_month_date, sep="-")
#     }
#     countryCoord <- countryCoord[complete.cases(countryCoord), ]
#     ground <- countryCoord[, c("longitude", "latitude", "plantingDate", "harvestDate")]
# 
#   }else{
#     GPS_fieldData <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country, "_",useCaseName, "/", Crop, "/result/compiled_fieldData.RDS", sep=""))
#     countryCoord <- unique(GPS_fieldData[, c("lon", "lat", "plantingDate", "harvestDate", ID)])
#     countryCoord <- countryCoord[complete.cases(countryCoord), ]
#     names(countryCoord) <- c("longitude", "latitude", "plantingDate", "harvestDate", ID)
#     ground <- countryCoord
#   }
# 
# 
# 
#   ground$Planting <- as.Date(ground$plantingDate, "%Y-%m-%d") # Planting date in Date format
#   ground$Harvesting <- as.Date(ground$harvestDate, "%Y-%m-%d") # Harvesting date in Date format
# 
#   countryShp <- geodata::gadm(country, level = 3, path='.')
#   dd2 <- raster::extract(countryShp, ground[, c("longitude", "latitude")])[, c("NAME_1", "NAME_2")]
#   ground$NAME_1 <- dd2$NAME_1
#   ground$NAME_2 <- dd2$NAME_2
# 
#   # 4.2. Compute the seasonal rainfall parameters for AOI ####
# 
#   if(AOI == TRUE){
# 
#     # Convert planting Date and harvesting in Julian Day
#     pl_j <-as.POSIXlt(unique(ground$Planting))$yday
#     hv_j <-as.POSIXlt(unique(ground$Harvesting))$yday
# 
#     ## 4.2.1. Case planting and harvesting dates span the same year ####
#     if (planting_harvest_sameYear ==  TRUE) {
# 
#       cls <- makeCluster(jobs)
#       doParallel::registerDoParallel(cls)
# 
#       # Loop on all the year
#       rf_4CP <- foreach(i=1:length(listRaster_RF), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
#         rasti <- listRaster_RF[i]
#         PlHvD <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
#         xy <- ground[, c("longitude", "latitude")]
#         raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
#         raini <- raini[,-1]
# 
#         if( varName %in% c("TemperatureMin", "TemperatureMax")){
#           raini <- raini-273
#         }
# 
#         ## name the columns by date in the form of dd_mm_yyyy
#         rainyDssat <- raini
#         dates_nr <- sub("^[^_]+", "", names(rainyDssat))
#         dates_nr <- as.numeric(gsub("_", "", dates_nr))
#         year_nr <- str_extract(rasti, "[[:digit:]]+")
#         startday <- paste(strsplit(Planting_month_date, "-")[[1]][2], strsplit(Planting_month_date, "-")[[1]][1], year_nr, sep="_")
# 
#         cnames <- startday
#         for(h in 1:length(dates_nr)){
#           dd <- as.character(as.Date(h, origin = paste(year_nr, strsplit(Planting_month_date, "-")[[1]][1], strsplit(Planting_month_date, "-")[[1]][2], sep="-")))
#           cnames <- c(cnames, paste(strsplit(dd, "-")[[1]][3], strsplit(dd, "-")[[1]][2], strsplit(dd, "-")[[1]][1], sep="_"))
#         }
#         names(rainyDssat) <- cnames[-length(cnames)]
# 
#         rainyDssat_t <- as.data.frame(t(rainyDssat))
#         colnames(rainyDssat_t) <- paste("Point", c(1:ncol(rainyDssat_t)), sep="_")
#         rainyDssat_t$MetaDVar <- rownames(rainyDssat_t)
#         return(rainyDssat_t)
#         # CP <- cbind(ground, rainyDssat_t)
#       }
# 
#       rainfall_points <- do.call(rbind, rf_4CP)
#       stopCluster(cls)
# 
#       metadata_D <- as.data.frame(t(data.frame(longitude = ground$longitude, latitude = ground$latitude,
#                                                NAME_1 = ground$NAME_1, NAME_2 = ground$NAME_2)))
#       metadata_D$MetaDVar <- rownames(metadata_D)
#       colnames(metadata_D) <- colnames(rainfall_points)
#       rainfall_points <- rbind(metadata_D, rainfall_points)
#     }
# 
#     ## 4.2.2. Case planting and harvesting dates span two different years ####
#     if (planting_harvest_sameYear ==  FALSE) {
# 
#       cls <- makeCluster(jobs)
#       doParallel::registerDoParallel(cls)
#       ## RelativeHumidity
#       rf_4CP <- foreach(i = 1:(length(listRaster_RF)-1), .packages = c('terra', 'plyr', 'stringr','tidyr')) %dopar% {
#         listRaster_RF <- listRaster_RF[order(listRaster_RF)]
#         rast1 <- listRaster_RF[i]
#         rast2 <- listRaster_RF[i+1]
#         rasti1 <- terra::rast(rast1, lyrs=c(pl_j:terra::nlyr(terra::rast(rast1))))
#         rasti2 <- terra::rast(rast2, lyrs=c(1:hv_j))
#         PlHvD <- c(rasti1, rasti2)
#         xy <- ground[, c("longitude", "latitude")]
#         raini <- terra::extract(PlHvD, xy, method='simple', cells=FALSE)
#         raini <- raini[,-1]
# 
#         if( varName %in% c("TemperatureMin", "TemperatureMax")){
#           raini <- raini-273
#         }
# 
#         ## name the columns by date in the form of dd_mm_yyyy
#         rainyDssat <- raini
#         dates_nr <- sub("^[^_]+", "", names(rainyDssat))
#         dates_nr <- as.numeric(gsub("_", "", dates_nr))
#         year_nr <- str_extract(rasti, "[[:digit:]]+")
#         startday <- paste(strsplit(Planting_month_date, "-")[[1]][2], strsplit(Planting_month_date, "-")[[1]][1], year_nr, sep="_")
# 
#         cnames <- startday
#         for(h in 1:length(dates_nr)){
#           dd <- as.character(as.Date(h, origin = paste(year_nr, strsplit(Planting_month_date, "-")[[1]][1], strsplit(Planting_month_date, "-")[[1]][2], sep="-")))
#           cnames <- c(cnames, paste(strsplit(dd, "-")[[1]][3], strsplit(dd, "-")[[1]][2], strsplit(dd, "-")[[1]][1], sep="_"))
#         }
#         names(rainyDssat) <- cnames[-length(cnames)]
# 
#         rainyDssat_t <- as.data.frame(t(rainyDssat))
#         colnames(rainyDssat_t) <- paste("Point", c(1:ncol(rainyDssat_t)), sep="_")
#         rainyDssat_t$MetaDVar = rownames(rainyDssat_t)
#         return(rainyDssat_t)
#       }
#       rainfall_points <- do.call(rbind, rf_4CP)
#       stopCluster(cls)
# 
#       metadata_D <- as.data.frame(t(data.frame(longitude = ground$longitude, latitude = ground$latitude,
#                                                NAME_1 = ground$NAME_1, NAME_2 = ground$NAME_2)))
#       metadata_D$MetaDVar <- rownames(metadata_D)
#       colnames(metadata_D) <- colnames(rainfall_points)
#       rainfall_points <- rbind(metadata_D, rainfall_points)
#     }
# 
# 
#     # 4.3. Compute the seasonal rainfall parameters for trial data ####
#     # when the planting and harvest dates varies for every row of data because it is actual trial data
#   }else {
# 
#     ## 4.3.1. Get the planting and harvesting dates ####
#     # Get the Year
#     ground$yearPi <- format(as.POSIXlt(ground$Planting), "%Y")
#     ground$yearHi <- format(as.POSIXlt(ground$Harvesting), "%Y")
# 
#     # Convert planting date and harvesting date in Julian Day
#     ground$ pl_j <-as.POSIXlt(ground$Planting)$yday
#     ground$hv_j <-as.POSIXlt(ground$Harvesting)$yday
# 
# 
#     ##Loop on all the trial location to calculate the seasonal rainfall parameters ####
# 
#     rainfall_points <- NULL
#     for(i in 1:nrow(ground)){
#       print(i)
# 
#       # Extract the i-th row
#       groundi <- ground[i, c("longitude", "latitude", "plantingDate", "harvestDate",ID,"NAME_1", "NAME_2","yearPi", "yearHi", "pl_j", "hv_j")]
# 
#       # Extract the Year
#       yearPi <- groundi$yearPi
#       yearHi <- groundi$yearHi
# 
#       # Convert planting Date and harvesting in Julian Day
#       pl_j <- groundi$pl_j
#       hv_j <- groundi$hv_j
# 
# 
#       ### 4.3.2.1. Subset the rainfall data according to the length of the growing season ####
#       # Case planting and harvesting dates span the same year
# 
#       if (yearPi == yearHi) {
#         rasti<-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
#         rasti <- terra::rast(rasti, lyrs=c(pl_j:hv_j))
#       }
# 
#       # Case planting and harvesting dates span two different years
#       if (yearPi < yearHi) {
#         rasti1<-listRaster_RF[which(grepl(yearPi, listRaster_RF, fixed=TRUE) == T)]
#         rasti1 <- terra::rast(rasti1, lyrs=c(pl_j:terra::nlyr(terra::rast(rasti1))))
#         rasti2 <-listRaster_RF[which(grepl(yearHi, listRaster_RF, fixed=TRUE) == T)]
#         rasti2 <- terra::rast(rasti2, lyrs=c(1:hv_j))
#         rasti <- c(rasti1, rasti2)
# 
#       }
# 
#       ### 4.3.2.2.Extract the information for the i-th row ####
# 
#       xy <- groundi[, c("longitude", "latitude")]
#       xy <- xy %>%
#         mutate_if(is.character, as.numeric)
# 
#       raini <- terra::extract(rasti, xy,method='simple', cells=FALSE)
#       raini <- raini[,-1]
# 
#       if( varName %in% c("TemperatureMin", "TemperatureMax")){
#         raini <- raini-273
#       }
# 
#       ##
# 
#       ## name the columns by date in the form of dd_mm_yyyy
#       rainyDssat <- raini
#       dates_nr <- sub("^[^_]+", "", names(rainyDssat))
#       dates_nr <- as.numeric(gsub("_", "", dates_nr))
# 
# 
#       plantingDate <- as.character(groundi$plantingDate)
#       startday <- paste(strsplit(plantingDate, "-")[[1]][3], strsplit(plantingDate, "-")[[1]][2], yearPi, sep="_")
#       cnames <- startday
#       for(h in 1:length(dates_nr)){
#         dd <- as.character(as.Date(h, origin = groundi$plantingDate))
#         cnames <- c(cnames, paste(strsplit(dd, "-")[[1]][3], strsplit(dd, "-")[[1]][2], strsplit(dd, "-")[[1]][1], sep="_"))
#       }
#       names(rainyDssat) <- cnames[-length(cnames)]
# 
#       rainyDssat_t <- as.data.frame(t(rainyDssat))
#       colnames(rainyDssat_t) <- paste("Point", i, sep="_")
#       rainyDssat_t$MetaDVar <- rownames(rainyDssat_t)
# 
# 
#       if(i == 1){
#         rainfall_points <- rainyDssat_t
#       }else{
#         rainfall_points <- merge(rainfall_points, rainyDssat_t, by="MetaDVar", all=TRUE)
#       }
#     }
# 
#     metadata_D <- as.data.frame(t(data.frame(longitude = ground$longitude, latitude = ground$latitude,
#                                              NAME_1 = ground$NAME_1, NAME_2 = ground$NAME_2, ID = ground[[ID]])))
#     colnames(metadata_D) <- gsub("V", "Point_",colnames(metadata_D))
#     metadata_D$MetaDVar <- rownames(metadata_D)
# 
#     rainfall_points <- rbind(metadata_D, rainfall_points)
#   }
# 
# 
#   x <- gsub("_", "-",rainfall_points$MetaDVar[6:nrow(rainfall_points)])
#   y <- as.Date(x, format="%d-%m-%Y")
# 
#   Date <- format(y, "%d")
#   Month <- format(y, "%m")
#   Year <- format(y, "%Y")
# 
#   # Date <- ifelse(Date < 10, paste(0,Date,sep=""), as.character(Date))
#   # Month <- ifelse(Month < 10, paste(0,Month,sep=""), as.character(Month))
#   #
# 
#   rainfall_points$Date <- c(rep(NA, 5), Date)
#   rainfall_points$Month <- c(rep(NA, 5), Month)
#   rainfall_points$Year <- c(rep(NA, 5), Year)
# 
#   # 4.4. writing the output ####
# 
#   # Check if the directory exists
#   Planting_month_date <- gsub("-", "_", Planting_month_date)
#   fname_rain <- ifelse(AOI == "TRUE", paste(varName, "_4CM_AOI_", Planting_month_date, "_", dataSource, ".RDS",sep=""), paste(varName,"_4CM_trial_", dataSource, ".RDS", sep=""))
# 
#   saveRDS(object = rainfall_points, file=paste(pathOut1, fname_rain, sep="/"))
#   saveRDS(object = rainfall_points, file=paste(pathOut2, fname_rain, sep="/"))
# 
#   return(rainfall_points)
# }
# 
# 

