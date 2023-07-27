

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
  
  
  






