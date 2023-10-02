

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and Relative Humidity summary data 
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_4ML.R")



#################################################################################################################
## get geo-spatial data for the AOI sites: data in the format crop models can use
#################################################################################################################

#1. AOI
AOI_4ML_geospatial <- join_geospatial_4ML(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = TRUE,
                               overwrite = TRUE, Planting_month_date = "08-08", 
                               ID = NULL, dataSource = "CHIRPS")

#2. trial
trial_4ML_geospatialtrial <- join_geospatial_4ML(country = "Rwanda",  useCaseName = "RAB", Crop = "Potato", AOI = FALSE,
                                    overwrite = TRUE, Planting_month_date = NULL, 
                                    ID = "TLID", dataSource = "CHIRPS")

#run just to add an AEZ
trial_4ML_geospatialAEZ <- ML_dataPrepartion(country = country, useCaseName = useCaseName, Crop = "Potato", AOI = FALSE, 
                                   Planting_month_date = NULL)


