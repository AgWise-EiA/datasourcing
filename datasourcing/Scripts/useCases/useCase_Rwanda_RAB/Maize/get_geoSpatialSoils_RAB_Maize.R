

#################################################################################################################
## source "get_geo-SpatialSoils.R" function and execute it for Rwanda RAB use case
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialSoils.R")


#################################################################################################################
## choose data source and crop the global layers to the target country shape file. 
#################################################################################################################
# 1. soils for ML
crop_geoSpatial_soil(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", dataSource= "iSDA", overwrite = TRUE, profile = FALSE)
crop_geoSpatial_soil(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", dataSource= "soilGrids", overwrite = TRUE, profile = FALSE)

# 2. isda and isric profiles
crop_geoSpatial_soil(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", dataSource= "soilGrids", overwrite = TRUE, profile = TRUE)


#################################################################################################################
## add derived soil variables like soil hydraulics data 
#################################################################################################################
# 1. soils for ML
transform_soils_data(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", resFactor=1, overwrite = TRUE)

# 2. soils for crop models = different profle depths
transform_soils_data_profile(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", resFactor=1, overwrite = TRUE)


#################################################################################################################
### extracting soil geo-spatial data for GPS locations (this is essential to get for location we have field/survey data)
#################################################################################################################
# 1. soils for ML
RAB_Maize_soil_trial <- extract_soil_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI = FALSE, ID = "TLID", profile = FALSE)
RAB_Maize_soil_AOI <- extract_soil_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI = TRUE, ID=NULL, profile = FALSE)

# 2. soils for crop models = different profile depths
RAB_Maize_soil_trial <- extract_soil_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI = FALSE, ID = "TLID", profile = TRUE)
RAB_Maize_soil_AOI   <- extract_soil_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Maize", AOI = TRUE, ID=NULL, profile = TRUE)








