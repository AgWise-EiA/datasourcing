#################################################################################################################
## 1. Create the grid coordiantes for Ghana Soybean
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_GridCoordinates.R")

#################################################################################################################
## 2. Sourcing the geo-spatial soil and weather data for Ghana soybean growing areas. The data is sourced for the experimental sites and for the area of interest (AOI) for scaling apart. 
#################################################################################################################

## source the generic script and define the use case specific variables and input data
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_V2.R")

country <- "Malawi"
useCaseName <- "Solidaridad"
Crop <- "Soybean"
Planting_month_date <- "12-01" ## the earliest possible plating mm-dd
Harvest_month_date <- "07-30" ## the earliest harvest date in mm-dd (https://www.apni.net/wp-content/uploads/2022/05/4R-Maize-Guide-0511.pdf)
season <- 1 ## has to be 1 or 2, is used to differntiate between the data for the differnt seasons with season added tothe file name
pathOut <- NULL ## it will define its own path in CG Labs using country, useCaseName and Crop
inputData <- NULL ## let it read from the correct folder where e.g. AOI_GPS.RDS is written 
jobs = 10
AOI= TRUE
Planting_month = 12
harvest_year = 07
## Make sure the Provinces or Districts names are spelled exactly in the same way as in the country shape file
countryShapefile <- geodata::gadm(country=country, level = 2, path='.')
unique(countryShapefile$NAME_1) ## for Provinces
unique(countryShapefile$NAME_2) ## for Districts

provinces <- unique(countryShapefile$NAME_1) ## for Provinces

##############
inputDataZ <- readRDS(paste0("~/agwise-datacuration/dataops/datacuration/Data/useCase_",country,"_", useCaseName,"/", Crop,'/result/AOI_GPS.RDS'))

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
  
  
}# ##. Summarized weather data (totla and monthly summaries) for ML applications: for the AOI 
get_WeatherSummarydata(country = country, useCaseName = useCaseName, Crop = Crop, inputData = inputData, AOI = TRUE, 
                       Planting_month_date = Planting_month_date,  Harvest_month_date = Harvest_month_date, season = season, jobs=10)


