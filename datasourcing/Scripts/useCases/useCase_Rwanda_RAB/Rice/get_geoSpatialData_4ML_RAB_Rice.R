

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain and Relative Humidity summary data 
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialData_4ML.R")



#1. trial
trial_4ML <- join_geospatial_4ML(country = "Rwanda",  useCaseName = "RAB", Crop = "Rice", AOI = FALSE,
                                 overwrite = TRUE, Planting_month_date = NULL, 
                                 ID = "TLID", dataSource = "CHIRPS")

#run just to add an AEZ
trial_4ML_geospatialAEZ <- ML_dataPrepartion(country = country, useCaseName = useCaseName, Crop = "Rice", AOI = FALSE, 
                                             Planting_month_date = NULL)


#################################################################################################################
## get geo-spatial data for the AOI sites: data in the format crop models can use
#################################################################################################################
#1. AOI
AOI_4ML <- join_geospatial_4ML(country = "Rwanda",  useCaseName = "RAB", Crop = "Rice", AOI = TRUE,
                                           overwrite = TRUE, Planting_month_date = "02-05", 
                                           ID = NULL, dataSource = "CHIRPS")

#1. AOI
AOI_4ML <- join_geospatial_4ML(country = "Rwanda",  useCaseName = "RAB", Crop = "Rice", AOI = TRUE,
                               overwrite = TRUE, Planting_month_date = "08-08", 
                               ID = NULL, dataSource = "CHIRPS")


