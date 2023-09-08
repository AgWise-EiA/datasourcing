

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain summary data 
##for Potato growing seasons are in early August to early December and early Feb to Early Jun (to test for optimal planting date the earliest plDte is set two weeks prior to early Aug and Feb)

#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R")

############### 1.  get the point data for crop models use ####################

## weather + the 6 depth ISRIC soil data  for trial sites
RAB_Rice_weather_soil_trial_profile <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", 
                                                 AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
                                                 soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, 
                                                 jobs =10)


## soil for ML
RAB_Rice_weather_soil_trial_profile <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", 
                                                                   AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
                                                                   soilData = TRUE, weatherData = FALSE, soilProfile = FALSE, 
                                                                   jobs =10)


## weather + the 6 depth ISRIC soil data  for AOI for season 1
RAB_Rice_weather_soil_AOI_profileS1 <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", 
                                                            AOI=TRUE, Planting_month_date = "07-15",  Harvest_month_date = "11-15",
                                                            soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, plantingWindow = 4, 
                                                            season = 1, jobs =10)


## weather + the 6 depth ISRIC soil data  for AOI for season 2
RAB_Rice_weather_soil_AOI_profileS1 <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", 
                                                                   AOI=TRUE, Planting_month_date = "01-15",  Harvest_month_date = "05-15",
                                                                   soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, plantingWindow = 4, 
                                                                   season = 2, jobs =10)


############## 2. summarized weather data for Machine learning use #####################

get_WeatherSummarydata(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", AOI = FALSE, 
                        Planting_month_date=NULL, Harvest_month_date=NULL, jobs=10)

get_WeatherSummarydata(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", AOI = TRUE, 
                       Planting_month_date = "08-01",  Harvest_month_date = "12-01", season = 1, jobs=10)

get_WeatherSummarydata(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", AOI = TRUE, 
                       Planting_month_date = "02-01",  Harvest_month_date = "06-01", season = 2, jobs=10)



