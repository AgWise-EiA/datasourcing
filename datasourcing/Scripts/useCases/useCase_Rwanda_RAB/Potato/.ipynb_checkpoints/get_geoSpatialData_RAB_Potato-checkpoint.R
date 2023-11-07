

#################################################################################################################
## Sourcing the geo-spatial soil and weather data for Rwanda Maize growing areas. The data is sourced apart for the experimental sites and for the area of interest (AOI) for scaling apart. 
#################################################################################################################

## source the generic script 
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R")


## Data for crop models at trial sites : weather + the 6 profiels of soil grids soil data
RAB_Potato_weather_soil_trial_profile <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", 
                                                 AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
                                                 soilData = FALSE, weatherData = TRUE, soilProfile = TRUE, season = 1, 
                                                 jobs =10)



## Data for crop models at AOI : weather + the 6 profiels of soil grids soil data
RAB_potato_weather_soil_AOI_profileS1 <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", 
                                                            AOI=TRUE, Planting_month_date = "08-08",  Harvest_month_date = "12-08",
                                                            soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, plantingWindow = 4, 
                                                            season = 1, jobs =10)


## Soil Data for machine learning for trial sites :  data from 0-20 cm and 20-50 cm of iSDA and 0-30 cm of isric
RAB_Potato_weather_soil_trial_profile <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", 
                                                                     AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
                                                                     soilData = TRUE, weatherData = FALSE, soilProfile = FALSE, 
                                                                     jobs =10)



##. Summarized weather data (totla and monthly summaries) for ML applications: for the trial sites 
get_WeatherSummarydata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = FALSE, 
                Planting_month_date=NULL, Harvest_month_date=NULL, jobs=10)

##. Summarized weather data (totla and monthly summaries) for ML applications: for the AOI 
get_WeatherSummarydata(country = "Rwanda", useCaseName = "RAB", Crop = "Potato", AOI = TRUE, 
                       Planting_month_date = "08-08",  Harvest_month_date = "12-08", season = 1, jobs=10)



