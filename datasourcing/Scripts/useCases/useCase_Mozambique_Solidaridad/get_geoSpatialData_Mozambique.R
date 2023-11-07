

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain summary data 
##for Nigeria, maize is planted starting from early April https://www.researchgate.net/publication/273487329_Review_Maize_research_and_production_in_Nigeria


# 1. get point based weather (daily) + DEM + soil (from the six profiles of ISRIC) data for both trial and AOI sites (for AOI by season and for trial for exact growing period)
# 2. summarize the weather data to the seasonal parameters
# 3. format the data obtained at step 2 for ML use
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R")

##$$$$$$$$$$$ 1.  get the point data

## weather + the 6 depth ISRIC soil data  for trial sites : crop model
weather_soil_trial_profile <- extract_geoSpatialPointData(country = "Mozambique", useCaseName = "Solidaridad", Crop = "Soybean", 
                                                 AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
                                                 soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, 
                                                 jobs =10)



## weather + the 6 depth ISRIC soil data  for AOI for season 1: crop model
weather_soil_AOI_profileS1 <- extract_geoSpatialPointData(country = "Mozambique", useCaseName = "Solidaridad", Crop = "Soybean", 
                                                            AOI=TRUE, Planting_month_date="10-15", Harvest_month_date="05-15", 
                                                            soilData = TRUE, weatherData = FALSE, soilProfile = TRUE, plantingWindow = 7, 
                                                            season = 1, jobs =10)


##$$$$$$$$$ 2. summarized weather data for ML

trial_get_WeatherSummarydata(country = "Mozambique", useCaseName = "Solidaridad", Crop = "Soybean", AOI = FALSE, 
                Planting_month_date=NULL, Harvest_month_date=NULL, jobs=10)

AOI_get_WeatherSummarydata(country = "Mozambique", useCaseName = "Solidaridad", Crop = "Soybean", AOI = TRUE, 
                       Planting_month_date="---", Harvest_month_date="---", season = 1,
                       jobs=10)



