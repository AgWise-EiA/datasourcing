
#################################################################################################################
## Sourcing the geo-spatial soil and weather data for Rwanda Maize growing areas. The data is sourced apart for the experimental sites and for the area of interest (AOI) for scaling apart. 
#################################################################################################################

## source the generic script and define the use case specific variables and input data
#source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_V2.R")
#source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData.R")
source("~/Agwise/datasourcing/datasourcing/datasourcing/Scripts/generic/get_geoSpatialData_trial.R", echo=TRUE)
inputData <- readRDS('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ghana_Maize/raw/compiled_fieldData.RDS')

country <- "Ghana"
useCaseName <- "useCaseName"
Crop <- "Maize"
#Planting_month_date <- substr(inputData$plantingDate,6,20)
#Harvest_month_date <-  substr(inputData$harvestDate,6,20)
#Planting_month_date <- "03-01"
#Harvest_month_date <- "08-24"
season <- 1
pathOut = "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ghana_useCaseName/Maize/result/geo_4cropModel"
AOI <- FALSE

# country <- "Ghana"
# useCaseName <- "useCaseName"
# Crop <- "Maize"
# Planting_month_date <- substr(inputData$plantingDate,6,20) #"05-15" ## the earliest possible plating mm-dd
# Harvest_month_date <- substr(inputData$harvestDate,6,20)#"08-30" ## the earliest harvest date in mm-dd (https://www.apni.net/wp-content/uploads/2022/05/4R-Maize-Guide-0511.pdf)




## Data for crop models at trial sites : weather + the 6 profiels of soil grids soil data
Ghana_Mz_weather_soil_trial_profile <- extract_geoSpatialPointData(country = country, useCaseName = useCaseName, Crop = Crop, 
                                                 AOI=FALSE, Planting_month_date=Planting_month_date, Harvest_month_date=Harvest_month_date, 
                                                 soilData = TRUE, weatherData = TRUE, soilProfile = TRUE, season = season, 
                                                 jobs =10)


##. Summarized weather data (totla and monthly summaries) for ML applications: for the AOI 
get_WeatherSummarydata(country = country, useCaseName = useCaseName, Crop = Crop, inputData = inputData, AOI = TRUE, 
                       Planting_month_date = Planting_month_date,  Harvest_month_date = Harvest_month_date, season = 1, jobs=10)



