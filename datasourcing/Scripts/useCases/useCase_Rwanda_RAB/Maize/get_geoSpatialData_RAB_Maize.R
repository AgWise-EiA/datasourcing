

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain summary data 
##for Rwanda, maize grows in two season, season 1 =  early September - End of March and season 2 = early March  - end August,
## workflow: 
# 1. get point based weather (daily) + DEM + soil (from the six profiles of ISRIC) data for both trial and AOI sites (for AOI by season and for trial for exact growing period)
# 2. summarize the weather data to the seasonal parameters
# 3. format the data obtained at step 2 for ML use
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R")

##$$$$$$$$$$$ 1.  get the point data

## weather + the 6 depth ISRIC soil data  for trial sites
RAB_maize_weather_soil_trial_profile <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", 
                                                 AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, season = 1,
                                                 soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, 
                                                 jobs =10)



## weather + the 6 depth ISRIC soil data  for AOI for season 1
RAB_maize_weather_soil_AOI_profileS1 <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", 
                                                            AOI=TRUE, Planting_month_date="02-01", Harvest_month_date="07-31", 
                                                            soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, plantingWindow = 8, 
                                                            season = 1, jobs =4)


## weather for AOI for season 2, soil data is the same as season 1 so we do not need to re-extract that
RAB_maize_weather_soil_AOI_profileS2 <- extract_geoSpatialPointData(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", 
                                                                  AOI=TRUE, Planting_month_date="08-01", Harvest_month_date="03-01", 
                                                                  soilData = FALSE, weatherData = TRUE, soilProfile = TRUE,  plantingWindow = 8,
                                                                  season = 2, jobs =4)




  

##$$$$$$$$$ 2. summarizeed weather data

get_WeatherSummarydata(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI = FALSE, 
                Planting_month_date=NULL, Harvest_month_date=NULL, jobs=10)

get_WeatherSummarydata(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI = TRUE, 
                       Planting_month_date="09-01", Harvest_month_date="03-31", season = 1,
                       jobs=10)

get_WeatherSummarydata(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI = TRUE, 
                       Planting_month_date="03-01", Harvest_month_date="08-31", season = 2,
                       jobs=10)


## TODO check this out
##$$$$$$$$$$ 4. summarize the weather data and merge all variables to make it ready for ML, for both seasons and for AOI and trial sites. 
### Planting_month_date and Harvest_month_date must be teh optimal planting and harvest dates selected based on expert input, crop model output or remote sensing analysis
### in case of getting point data for crop models Planting_month_date is the earliest possible planting date, but here it is the optimal date
join_geospatial_4ML(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI = TRUE, season = 1,
                    Planting_month_date="09-01", Harvest_month_date="03-31")





######################################################################################################
## YAML
######################################################################################################


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






