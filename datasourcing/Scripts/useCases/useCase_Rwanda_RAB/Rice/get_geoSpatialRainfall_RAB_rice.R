

#################################################################################################################
## source "get_rain_temp_summary.R" function and get rain summary data 
#################################################################################################################
source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_geoSpatialRainfall.R")


#################################################################################################################
## get daily rainfall 
## for trial sites from CHIRPS  and AgEra5 
#################################################################################################################

trial_point_Rf_CHIRPS <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Rice", AOI = FALSE,
              overwrite = TRUE, Planting_month_date = NULL, Harvest_month_date = NULL, jobs=10, 
              dataSource = "CHIRPS", ID = "TLID")


#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of rainy days)
## for trial locations from CHIRPS and AgEra
#################################################################################################################
Rain_summary_trialLoc_CHIRPS <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Rice", AOI = FALSE, 
                                                        overwrite = TRUE, Planting_month_date = NULL,
                                                        Harvest_month_date = NULL, jobs=10, dataSource = "CHIRPS", ID = "TLID")


#################################################################################################################
## get daily rainfall 
## for AOI from CHIRPS & AgEra: when planting and harvest year are the same
#################################################################################################################

AOI_point_Rf_sameYear_p1  <- get_rf_pointData(country = "Rwanda",  useCaseName = "RAB", Crop = "Rice", AOI = TRUE,
                         overwrite = TRUE, Planting_month_date = "08-08",  
                         Harvest_month_date = "12-08", jobs=10, dataSource = "CHIRPS", ID = NULL)




#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of raint days) 
## for AOI from CHIRPS and AgEra
#################################################################################################################

Rain_AOI_summary_s1_p1_Ch <- get_rf_pointSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Rice", AOI = TRUE,
                                            overwrite = TRUE,  Planting_month_date = "08-08",  
                                            Harvest_month_date = "12-08", jobs=10, dataSource = "CHIRPS", ID = NULL)

#################################################################################################################
## get rainfall summaries (total RF, monthly rf and nr of rainy days) 
## for Country from CHIRPS
#################################################################################################################
get_rf_rasterSummarydata(country = "Rwanda",  useCaseName = "RAB", Crop = "Rice", AOI = TRUE, overwrite = TRUE,
                         Planting_month_date = "02-05",  Harvest_month_date = "06-05", jobs=10,
                         dataSource = "CHIRPS", scenario=TRUE)





