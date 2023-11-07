#################################################################################################################
## source "get_MODISts_PreProc.R" function and execute it for Rwanda RAB use case
#################################################################################################################

source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_MODISts_PreProc.R")
smooth_rasterTS(country = "Rwanda", useCaseName = "RAB",Planting_year = 2021, Harvesting_year = 2021, overwrite = TRUE)
