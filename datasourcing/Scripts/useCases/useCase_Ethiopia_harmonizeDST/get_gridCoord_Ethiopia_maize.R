

source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_GridCoordinates.R")
Eth_maize_AOI <- readOGR(dsn="~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ethiopia_harmonizeDST/Maize/Landing", 
                         layer="Maize_growing_areas")
Eth_maize_AOI <- raster(Eth_maize_AOI)
countrySpVec <- geodata::gadm("Ethiopia", level = 2, path='.')
croppedLayer <- terra::crop(Eth_maize_AOI, countrySpVec)

Eth_miaze_coord <- getCoordinates(country = "Malawi", useCaseName = "Solidaridad", Crop = "Soybean", resltn = 0.05, provinces="Kasungu", district = NULL)