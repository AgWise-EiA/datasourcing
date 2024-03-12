source("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_V2.R")

country <- "Zambia"
useCaseName <- "Solidaridad"
Crop <- "Soybean"
Planting_month_date <- "12-01" ## the earliest possible plating mm-dd
Harvest_month_date <- "05-30" ## the earliest harvest date in mm-dd (https://www.apni.net/wp-content/uploads/2022/05/4R-Maize-Guide-0511.pdf)
season <- 1 ## has to be 1 or 2, is used to differntiate between the data for the differnt seasons with season added tothe file name
pathOut <- NULL ## it will define its own path in CG Labs using country, useCaseName and Crop
inputData <- NULL ## let it read from the correct folder where e.g. AOI_GPS.RDS is written 
jobs = 10
AOI= TRUE

dat_dir <- paste0('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_',country, '_', useCaseName,'/',Crop, '/result/geo_4cropModel')

dat_dir_geo_4ML <- paste0('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_',country, '_', useCaseName,'/',Crop, '/result/geo_4ML')

dat_yld <- paste0("~/agwise-potentialyield/dataops/potentialyield/Data/useCase_", country, "_", useCaseName,"/", Crop, "/raw/geo_4cropModel")


ld <- list.dirs(dat_dir, full.names = FALSE)

m <- which(!ld %in%  c("","gadm"))

if(length(m) >0){
  ld <- ld[m]
}

variables <- c('windSpeed', 'solarRadiation', 'relativeHumidity', 'temperatureMin', 'temperatureMax', 'Rainfall', 'SoilDEM')

# edist <- foreach(i = 1:length(variables)) %do% {trial_id = euc.dist(zm[i,],soilm[i,])}
# 
# edist <- do.call(rbind, edist)

for (v in 1:length(variables)){
  
  var_all <- NULL
  
  for (l in 1:length(ld)){
    
  vars <- list.files(paste0(dat_dir,'/',ld[l]), pattern = variables[v], recursive = TRUE, full.names = TRUE)
  
  vd <- readRDS(vars)
  
  var_all <- rbind(var_all, vd)}
  
    saveRDS(var_all, file = paste0(dat_dir,'/', paste0(gsub("_","",variables[v]),"_Season_1_PointData_AOI.RDS")))
    
    saveRDS(var_all, file = paste0(dat_dir_geo_4ML,'/', paste0(gsub("_","",variables[v]),"_Season_1_PointData_AOI.RDS")))
    
    saveRDS(var_all, file = paste0(dat_yld,'/', paste0(gsub("_","",variables[v]),"_Season_1_PointData_AOI.RDS")))
    
}

countryCoord <- readRDS(paste0("~/agwise-datacuration/dataops/datacuration/Data/","useCase_",country,"_",useCaseName,"/",Crop,"/result/AOI_GPS.RDS"))#[1:100,]
str(countryCoord)

##. Summarized weather data (totla and monthly summaries) for ML applications: for the AOI 
get_WeatherSummarydata(country = country, useCaseName = useCaseName, Crop = Crop, inputData = inputData, AOI = TRUE, 
                       Planting_month_date = Planting_month_date,  Harvest_month_date = Harvest_month_date, season = season, jobs=10)
