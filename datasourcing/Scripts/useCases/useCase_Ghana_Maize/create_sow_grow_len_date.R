library(terra)
library(tidyverse)
library(geodata)

setwd("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ghana_Maize/input/geospatial/")
gh1 <- geodata::gadm(path = ".", country = "Ghana", level = "1")

plot(gh1)

#planting date - early march = 70days = 7decads
#harvesting date - end of september = 27 decades

gh1$pl_date <- 7
gh1$hv_date <- 27
gh1$gr_len <- 20

r <- rast("./climate/Rainfall/monthly_total/1981.tif")
pl_date <- terra::rasterize(x = gh1, y = r, field = "pl_date", value=1:nrow(gh1))
hv_dat <- terra::rasterize(x = gh1, y = r, field = "hv_date", value=1:nrow(gh1))
gr_len <- terra::rasterize(x = gh1, y = r, field = "gr_len", value=1:nrow(gh1))

sow_date <- "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ghana_Maize/input/geospatial/sowing_Date"
grow_len <- "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ghana_Maize/input/geospatial/growing_Length"
hv_date <- "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Ghana_Maize/input/geospatial/harvesting_Date"

if(!dir.exists(sow_date)){
  dir.create(sow_date, recursive = T)
}

if(!dir.exists(grow_len)){
  dir.create(grow_len, recursive = T)
}

if(!dir.exists(hv_date)){
  dir.create(hv_date, recursive = T)
}

writeRaster(pl_date, paste(sow_date, "maize.tif", sep = "/"), filetype = "GTiff")
writeRaster(gr_len, paste(grow_len, "maize.tif", sep = "/"), filetype = "GTiff")
writeRaster(hv_dat, paste(hv_date, "maize.tif", sep = "/"), filetype = "GTiff")
