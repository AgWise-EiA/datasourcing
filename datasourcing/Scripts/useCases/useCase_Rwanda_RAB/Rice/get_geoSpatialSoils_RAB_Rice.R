

#################################################################################################################
## source "get_geo-SpatialSoils.R" function and execute it for Rwanda RAB use case
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialSoils.R")


#################################################################################################################
## choose data source and crop the global layers to the target country shape file. 
#################################################################################################################

crop_geoSpatial_soil(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", dataSource= "iSDA", overwrite = TRUE)
crop_geoSpatial_soil(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", dataSource= "soilGrids", overwrite = TRUE)


#################################################################################################################
## add derived soil variables like soil hydraulics data 
#################################################################################################################

transform_soils_data(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", resFactor=1, overwrite = TRUE)


#################################################################################################################
### extracting soil geo-spatial data for GPS locations (this is essential to get for location we have field/survey data)
#################################################################################################################

RAB_rice_soil <- extract_soil_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", AOI = FALSE, ID = "TLID")

#################################################################################################################
## extracting soil geo-spatial data for GPS locations for predictions, for AOI
#################################################################################################################
RAB_rice_soil_AOI <- extract_soil_pointdata(country = "Rwanda", useCaseName = "RAB", Crop = "Rice", AOI = TRUE, ID=NULL)









