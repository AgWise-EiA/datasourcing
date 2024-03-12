

#################################################################################################################
## Sourcing the geo-spatial soil and weather data for Rwanda Maize growing areas. 
## The data is sourced for the experimental sites and for the area of interest (AOI) for scaling apart. 
#################################################################################################################

## source the generic script and define the use case specific variables and input data
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_V2.R")
country <- "Ethiopia"
useCaseName <- "harmonizeDST"
Crop <- "Wheat"


#################################################################################################################
## for AOI: Given the crop growing window is defined for every zone differently we need to run the data sourcing by zone 
#################################################################################################################
inputDataZ <- readRDS("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Wheat/raw/Wheat_AOI.RDS")

for( zones in unique(inputDataZ$Zone)){
  print(zones)
  zone_inputData <- inputDataZ[inputDataZ$Zone == zones ,]
  Planting_month_date <- unique(zone_inputData$planting_month_date)
  Harvest_month_date <- unique(zone_inputData$harvest_month_date)
  season <- 1
  plantingWindow <- 4
  
  pathOut = paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Wheat/result/geo_4cropModel/", zones, "/", sep="")
  
  ## Data for crop models at AOI : weather + the 6 profiles of soil grids soil data
  Eth_Maize_AOI_profileS1 <- extract_geoSpatialPointData(country = country, useCaseName = useCaseName, Crop = Crop, inputData=zone_inputData,
                                                                       AOI=TRUE, Planting_month_date = Planting_month_date,  Harvest_month_date = Harvest_month_date,
                                                                       soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, plantingWindow = plantingWindow, 
                                                                       season = 1, jobs =10, pathOut=pathOut)
  
  
}


# 
# 
## Data for crop models at trial sites : weather + the 6 profiels of soil grids soil data
#################################################################################################################
## for trial locations  
#################################################################################################################
inputDataTrial <- readRDS("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Wheat/intermediate/Wheat_yield_diff_blup.rds")
inputDataTrial <- inputDataTrial %>% 
  dplyr::select(trial_id, NAME_1, NAME_2,long2, lat2) %>% 
  dplyr::rename(TLID = trial_id, lon=long2, lat=lat2) %>% 
  unique()
str(inputDataTrial)

## getting planting and harvest dates base on 


Wheat_weather_soil_trial_profile <- extract_geoSpatialPointData(country = country, useCaseName = useCaseName, Crop = Crop, inputData=inputDataTrial,
                                                 AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL,
                                                 soilData = TRUE, weatherData = FALSE, soilProfile = FALSE, season = season,
                                                 jobs =10)



# 
# ## Soil Data for machine learning for trial sites :  data from 0-20 cm and 20-50 cm of iSDA and 0-30 cm of isric
# RAB_Potato_weather_soil_trial_profile <- extract_geoSpatialPointData(country = country, useCaseName = useCaseName, Crop = Crop, inputData=inputData,
#                                                                      AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
#                                                                      soilData = TRUE, weatherData = TRUE, soilProfile = FALSE, 
#                                                                      jobs =10)
# 
# 
# ##. Summarized weather data (total and monthly summaries) for ML applications: for the trial sites 
# get_WeatherSummarydata(country = country, useCaseName = useCaseName, Crop = Crop, inputData = inputData, AOI = FALSE, 
#                 Planting_month_date=NULL, Harvest_month_date=NULL, pathOut= pathOut, jobs=10)
# 
# ##. Summarized weather data (totla and monthly summaries) for ML applications: for the AOI 
# get_WeatherSummarydata(country = country, useCaseName = useCaseName, Crop = Crop, inputData = inputData, AOI = TRUE, 
#                        Planting_month_date = Planting_month_date,  Harvest_month_date = Harvest_month_date, season = 1, jobs=10)
# 
# 
# 
