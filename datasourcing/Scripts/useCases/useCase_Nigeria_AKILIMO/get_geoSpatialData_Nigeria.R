

### Nigeria maize belt

require(rgdal)

maizebelt <- raster(readOGR(dsn = "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_Nigeria_AKILIMO/Maize/Landing", layer = "Maize_belt"))
ext(maizebelt)
countryShp <- geodata::gadm("Nigeria", level = 1, path='.')
croppedLayer <- terra::crop(countryShp, maizebelt)
plot(countryShp)
plot(croppedLayer, add=TRUE, col="green")
plot(croppedLayer[croppedLayer$NAME_1 == "Federal Capital Territory"], add=TRUE, col="red")

NGAEZ <- raster(readOGR(dsn = "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_Nigeria_AKILIMO/Maize/Landing", layer = "Nigeria_AEZ"))
ext(NGAEZ)
plot(NGAEZ)
# 
# c("Bauchi", "Borno", "Gombe", "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Yobe", "Zamfara", "Jigawa") ## 4 months growing period 
#  planting early June and early July and harvest in  end-Sep and end-Oct. 4 months 
Planting_month_date = 05-15; Harvest_month_date = 09-15; plantingWindow = 8
# 
# c("Niger", "Plateau")  Planting early April and end of May harvest late July to end Sep # 4 months 
Planting_month_date = 04-01; Harvest_month_date = 07-30; plantingWindow = 8
# 
# north_central <- c("Benue", "Kogi", "Kwara", "Nasarawa", "Taraba") # 4 months 
# Planting in the four states is mostly between early May and late June and harvest in this region goes between late-August and early Oct.
Planting_month_date = 05-01; Harvest_month_date = 08-30; plantingWindow = 8
# 
# c("Ondo", "Osun", "Oyo") ## 7 months 
# planting between early Feb and early April and harvest is between late July to late Sep"
Planting_month_date = 02-01; Harvest_month_date = 07-30; plantingWindow = 8

                                                                                                                                      



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
weather_soil_trial_profile <- extract_geoSpatialPointData(country = "Nigeria", useCaseName = "AKILIMO", Crop = "Maize", 
                                                 AOI=FALSE, Planting_month_date=NULL, Harvest_month_date=NULL, 
                                                 soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, 
                                                 jobs =10)



## weather + the 6 depth ISRIC soil data  for AOI for season 1: crop model
weather_soil_AOI_profileS1 <- extract_geoSpatialPointData(country = "Nigeria", useCaseName = "AKILIMO", Crop = "Maize", AOI=TRUE, 
                                                          # Planting_month_date="10-15", Harvest_month_date="05-15", plantingWindow = 7, 
                                                            soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, 
                                                            season = 1, jobs = 10, Regions=South)


extract_geoSpatialPointData(country = "Nigeria", useCaseName = "AKILIMO", Crop = "Maize", AOI=TRUE, 
                                                          # Planting_month_date="10-15", Harvest_month_date="05-15", plantingWindow = 7, 
                                                          soilData = TRUE, weatherData = FALSE, soilProfile = FALSE, 
                                                          season = 1, jobs = 10)

# North <-  c("Bauchi", "Borno", "Gombe", "Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Yobe", "Zamfara", "Jigawa") ## 4 months growing period 
# Planting_month_date = "05-15"; Harvest_month_date = "09-15"; plantingWindow = 8

# Regions <- c("Niger", "Plateau") # Planting early April and end of May harvest late July to end Sep # 4 months 
# Planting_month_date =" 04-01"; Harvest_month_date = "07-30"; plantingWindow = 8
# # 

# north_central <- c("Benue", "Kogi", "Kwara", "Nasarawa", "Taraba") # 4 months 
# Planting_month_date = "05-01"; Harvest_month_date ="08-30"; plantingWindow = 8

# South <- c("Ondo", "Osun", "Oyo") ## 7 months 
# Planting_month_date = "02-01"; Harvest_month_date = "07-30"; plantingWindow = 8


##$$$$$$$$$ 2. summarized weather data for ML

get_WeatherSummarydata(country = "Nigeria", useCaseName = "AKILIMO", Crop = "Maize", AOI = FALSE, 
                Planting_month_date=NULL, Harvest_month_date=NULL, jobs=10)

get_WeatherSummarydata(country = "Nigeria", useCaseName = "AKILIMO", Crop = "Maize", AOI = TRUE, 
                       Planting_month_date="---", Harvest_month_date="---", season = 1,
                       jobs=10)



