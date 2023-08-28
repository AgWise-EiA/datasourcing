

#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("MODIStsp", "mapedit", "leaflet", "shiny","shinydashboard","shinyFiles",
                       "shinyalert", "rappdirs","shinyjs",
                       "leafem", "mapedit", "magrittr")

installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

lapply(packages_required, library, character.only = TRUE)
#################################################################################################################
## get MODIS layers
#################################################################################################################

pathOut = "~/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/MODISdata" 
user = "amitcimmyt" 
password = "Ambica_81" 
start_date = "2022.04.24"
end_date = "2022.05.20" 
country = "Rwanda"

## we need a mechanism to get to the shape files 



# MODIS_intearctive_download(start_date = "2022.04.24", end_date = "2022.05.20", 
#                            pathOut = "~/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/MODISdata",
#                            user, password)

MODIS_intearctive_download <- function( start_date=NULL, end_date=NULL, pathOut, user, password){
  
  # countryShp <- geodata::gadm(country, level = 3, path='.')

  MODIStsp()
  
  setwd("~/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/MODISdata")
  MODIStsp_get_prodlayers("M*D13Q1")
  
  MODIStsp(gui             = FALSE, 
           out_folder      = pathOut, # "C:\\GIS\\define your output folder here", ## Path to download the images
           selprod         = "Vegetation Indexes_16Days_250m (M*D13Q1)",## define which product of MODIS images
           bandsel         = c("EVI"), 
           spatmeth        = "file",
           #quality_bandsel = "QA_usef", 
           sensor         = "Both",  ## define Terra or Aqua or both, do we need Aqua??
           #out_res_sel = "User Defined",
           #out_res = 0.002786033, 
           user            = user, 
           password        = password,
           start_date      = start_date, 
           end_date        = end_date, 
           verbose         = FALSE,
           spafile = "~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Rwanda_RAB/Potato/Landing/AEZ/gadm41_RWA_3.shp", ## the boundary file of the study area in .shp format
           out_projsel = "User Defined",
           output_proj = 4326,
           resampling = "bilinear")
  
}








### layer by layer
library(maptools)
library(MODISTools)
library(sf)

data(wrld_simpl)
world = st_as_sf(wrld_simpl)
lux = world[world$NAME=='Luxembourg',]
#Now we find the location (centroid) and size of the country:
  
  #find centroid of polygon in long-lat decimal degrees
  lux.cent = st_centroid(lux)

#find width and height of country in km
lux.proj = st_transform(lux, 
                        "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs")
lux.km_lr = diff(st_bbox(lux.proj)[c(1,3)])
lux.km_ab = diff(st_bbox(lux.proj)[c(2,4)])
#Using this info, we can download the correct Modis data (using leaf-area index, lai, as an example):
  
  #download the MODIS tiles for the area we defined

lux_lai <- mt_subset(product = "MOD15A2H",
                     lat = lux.cent$LAT, lon =  lux.cent$LON,
                     band = "Lai_500m",
                     start = "2004-01-01", end = "2004-02-01",
                     km_lr = lux.km_lr, km_ab = lux.km_ab,
                     site_name = "Luxembourg",
                     internal = TRUE, progress = TRUE)

# convert to a spatial raster
lux.rast = mt_to_raster(df = lux_lai, reproject = TRUE)
lux.rast = raster::mask(lux.rast, lux)
plot(lux.rast)
plot(st_geometry(lux),add=T)





