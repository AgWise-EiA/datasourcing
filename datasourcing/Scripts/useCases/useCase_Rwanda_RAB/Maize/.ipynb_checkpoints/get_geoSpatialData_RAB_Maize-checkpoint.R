

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain summary data 
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R")


RAB_maize_weather_soil_trial <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", 
                                 AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
                                 soilData = TRUE, weatherData = TRUE,soilProfile = FALSE, 
                                 jobs =10)

  
RAB_maize_weather_soil_AOI <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", 
                                AOI=TRUE, soilProfile = FALSE, Planting_month_date="02-01",
                                Harvest_month_date="05-30", soilData = TRUE, weatherData = TRUE,
                                jobs =10)
  
  
  

# Reading configuration file
confFile = read_yaml("conf/conf_geoSpatialData.yml")

RAB_maize_weather_soil_trial <- extract_geoSpatialPointData(country = confFile$arguments_global$country
                                                            , useCaseName = confFile$arguments_global$useCase
                                                            , Crop = confFile$arguments_global$crop
                                                            , AOI=confFile$arguments_weatherSoilTrial$AOI
                                                            , Planting_month_date=confFile$arguments_weatherSoilTrial$plantingMonthDate
                                                            , Harvest_month_date=confFile$arguments_weatherSoilTrial$haverstMonthDate
                                                            , soilData = confFile$arguments_weatherSoilTrial$soilData
                                                            , weatherData = confFile$arguments_weatherSoilTrial$weatherData
                                                            ,soilProfile = confFile$arguments_weatherSoilTrial$soilData, 
                                                            jobs =confFile$arguments_global$jobs)


RAB_maize_weather_soil_AOI <- extract_geoSpatialPointData(country = confFile$arguments_global$country
                                                          , useCaseName = confFile$arguments_global$useCase
                                                          , Crop = confFile$arguments_global$crop
                                                          , AOI=confFile$arguments_weatherSoilAOI$AOI
                                                          , soilProfile = confFile$arguments_weatherSoilAOI$soilProfile
                                                          , Planting_month_date= confFile$arguments_weatherSoilAOI$plantingMonthDate
                                                          , Harvest_month_date=confFile$arguments_weatherSoilAOI$haverstMonthDate
                                                          , soilData = confFile$arguments_weatherSoilAOI$soilData
                                                          , weatherData = confFile$arguments_weatherSoilAOI$weatherData
                                                          ,jobs =10)


typeof(confFile$arguments_weatherSoilTrial$weatherData)






