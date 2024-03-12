source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_V2.R")
country <- "Zambia"
useCaseName <- "Solidaridad"
Crop <- "Maize"
Planting_month_date <- "11-01" ## the earliest possible plating mm-dd
Harvest_month_date <- "07-30" ## the earliest harvest date in mm-dd (https://www.apni.net/wp-content/uploads/2022/05/4R-Maize-Guide-0511.pdf)
season <- 1 ## has to be 1 or 2, is used to differntiate between the data for the differnt seasons with season added tothe file name
pathOut <- NULL ## it will define its own path in CG Labs using country, useCaseName and Crop
inputData <- NULL ## let it read from the correct folder where e.g. AOI_GPS.RDS is written 
jobs = 10

#inputDataZ <- readRDS("~/agwise-datacuration/dataops/datacuration/Data/useCase_Mozambique_Solidaridad/Maize/result/AOI_GPS.RDS")

inputDataZ <- readRDS(paste0("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country,"_", useCaseName,"/", Crop,'/result/AOI_GPS.RDS'))
provinces <- which(!unique(inputDataZ$NAME_1) %in% c('Northern','Luapula'))
provinces <- unique(inputDataZ$NAME_1)[provinces]

names(inputDataZ)[2] <- 'Zone'
for(zones in 1: length(provinces)){
  print(provinces[zones])
  zone_inputData <- inputDataZ[inputDataZ$Zone == provinces[zones],]
  Planting_month_date <- Planting_month_date
  Harvest_month_date <- Harvest_month_date
  season <- 1
  plantingWindow <- 4
  
  
  pathOut = paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_",country,"_", useCaseName,"/", Crop,'/result/geo_4cropModel/',  provinces[zones], "/", sep="")
  #dir.create(pathOut)
  ## Data for crop models at AOI : weather + the 6 profiles of soil grids soil data
  Moz_Maize_AOI_profileS1 <- extract_geoSpatialPointData(country = country, useCaseName = useCaseName, Crop = Crop, inputData=zone_inputData,
                                                         AOI=TRUE, Planting_month_date = Planting_month_date,  Harvest_month_date = Harvest_month_date,
                                                         soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, plantingWindow = plantingWindow,
                                                         season = 1, jobs =10, pathOut=pathOut)
  
  
}
