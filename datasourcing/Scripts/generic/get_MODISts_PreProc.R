# Preprocessing and smoothing of MODIS NDVI time series for the Use Case

# Introduction: 
# This script to pre-process the MODIS NDVI time series, and apply basic operations to make it analysis ready. This script has to be run
# after get_MODISdata.R. It covers :
# (1) - Reading multi-temporal datasets and stacking them 
# (2) - Subsetting the analysis year 
# (3) - Extracting the area of interest 
# (4) - Applying smoothing techniques to fill data gaps 
# (5) - Saving the raster 
# NOTE : This script is reading a crop mask that is downloaded through Google Earth Engine and need to be run before running this script.
#        Please follow the corresponding documention : get_ESACropland_fromGEE.html

#### Getting started #######

# 1. Sourcing required packages -------------------------------------------
packages_required <- c("plotly", "raster", "rgdal", "gridExtra", "sp", "ggplot2", "caret", "signal", "timeSeries", "zoo", "pracma", "rasterVis", "RColorBrewer", "dplyr", "terra")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}
library(SpaDES.tools)
# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

# 2. Preparing and Smoothing the raster time series -------------------------------------------

smooth_rasterTS<-function(country, useCaseName, Planting_year, Harvesting_year, overwrite = FALSE){
  
  #' @description Function that will preprocess the MODIS NDVI (8 days) time series, including a masking-out of the cropped area and a gap-filling & smoothing of the TS with a Savitzky-Golay filter. If the cropping season is on a civil year, the initial TS should have 46 images, if the cropping season is between two civil years, the initial TS should have 92 images.
  #' @param country country name
  #' @param useCaseName use case name  name
  #' @param overwrite default is FALSE 
  #' @param Planting_year the planting year in integer
  #' @param Harvesting_year the harvesting year in integer
  #'
  #' @return raster files cropped from global data and return a NDVI time series smoothed that is written out in agwise-datasourcing/dataops/datasourcing/Data/useCaseName/MODISdata/transform/NDVI
  #'
  #' @examples smooth_rasterTS(country = "Rwanda", useCaseName = "RAB", Planting_year = 2021, Harvesting_year = 2021, overwrite = TRUE)
  #' 
  #' 
  ## 2.1. Creating a directory to store the cropped and smoothed data ####
  pathOut <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", "MODISdata/transform/NDVI", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## 2.2. Read  and scale the raster and shape data ####
  ### 2.2.1. Get the country boundaries ####
  # Read the relevant shape file from gdam to be used to crop the data
  countryShp <- terra::vect(list.files(paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/","MODISdata/raw/"), pattern="Boundary.shp$", full.names=T))
  
  ### 2.2.2. Get the NDVI time series ####
  listRaster_EVI <-list.files(paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/","MODISdata/raw/useCase_", country, "_",useCaseName,"_Boundary/VI_16Days_250m_v61/NDVI/"), pattern=".tif$", full.names=T)
  stacked_EVI <- terra::rast(listRaster_EVI) #stack
  
  ## 2.3. Subsetting the year of analysis ####
  
  nblayers <- 46 # 1 year = 46 images
  
  # Case Planting Year = Harvesting Year
  if (Planting_year == Harvesting_year){
    stacked_EVI_s <- stacked_EVI[as.character(Planting_year)]
    
    # check that the required numbers of files are there (1 year = 46 images)
    if ( terra::nlyr(stacked_EVI_s) != nblayers){stop('The number of layers should be equal to 46!')}
  }
  
  # Case Planting Year < Harvesting Year
  if (Planting_year < Harvesting_year){
    stacked_EVI_P <- stacked_EVI[as.character(Planting_year)]
    stacked_EVI_H <- stacked_EVI[as.character(Harvesting_year)]
    
    # check that the required numbers of files are there (1 year = 46 images)
    if ( terra::nlyr(stacked_EVI_P) != nblayers){stop('The number of layers in Planting_year should be equal to 46!')}
    if ( terra::nlyr(stacked_EVI_H) != nblayers){stop('The number of layers in Harvesting_year should be equal to 46!')}
    
    if (terra::nlyr(stacked_EVI_P)==terra::nlyr(stacked_EVI_H)){
      stacked_EVI_s <- c(stacked_EVI_P, stacked_EVI_H)
    } else {
      stop("nlayers in Planting_year != nlayers in Harvesting_year ")
    }
    
  }
  # Case Planting Year > Harvesting Year
  if (Planting_year > Harvesting_year){
    stop( "Planting_year can't be > to Harvesting_year")
  }
  
  rm(listRaster_EVI)
  rm(stacked_EVI)
  
  ## 2.4. Masking out of the cropped area ####
  
  ### 2.4.1. Get the cropland mask and resample to NDVI ####
  cropmask <- list.files(paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/","MODISdata/raw/useCase_", country, "_",useCaseName,"_Boundary/CropMask"), pattern=".tif$", full.names=T)
  cropmask <- terra::rast(cropmask)
  cropmask <- terra::mask(cropmask, countryShp)
  ## reclassification 1 = crop, na = non crop
  m1 <- cbind(c(40), 1)
  cropmask <- terra::classify(cropmask, m1, others=NA)
  cropmask <- terra::resample(cropmask, stacked_EVI_s)
  
  ## crop and mask cropland
  #stacked_EVI_s <- stacked_EVI_s/10000 ## scaling to NDVI value ranges from -1 to +1
  #stacked_EVI_s <- stacked_EVI_s * cropmask

  ## 2.5. Applying Savitzky-Golay filter ####
  ## Split the raster into 16 parts to speed the process ##
  #y <- SpaDES.tools::splitRaster(stacked_EVI_s, nx=4,ny=4)
  
  ## create SG filter function  ##
  # Case Planting Year = Harvesting Year
  if (Planting_year == Harvesting_year){
    start <- c(Planting_year, 1)
    end <- c(Planting_year, terra::nlyr(stacked_EVI_s))
  }
  
  # Case Planting Year < Harvesting Year
  if (Planting_year < Harvesting_year){
    start <- c(Planting_year, 1)
    end <- c(Harvesting_year, terra::nlyr(stacked_EVI_H))
  }
  
  # Case Planting Year > Harvesting Year
  if (Planting_year > Harvesting_year){
    stop( "Planting_Year can't be > to Harvesting_year")
  }
  
  fun <- function(x) {
    v=as.vector(x)
    z<- timeSeries::substituteNA(v, type= "mean")
    MODIS.ts2 = ts(z, start=start, end=end, frequency=nblayers)
    sg=signal::sgolayfilt(MODIS.ts2, p=3, n=9, ts=1) # run savitzky-golay filter## edit the function if required
  }
  ## Apply function on data (split data set) ###

  EVI_SGfil <- terra::app(x=stacked_EVI_s, fun)
  EVI_SGfil <- EVI_SGfil*cropmask
  
  ## Rename the SG filter time series
  names(EVI_SGfil) <- names(stacked_EVI_s)
  
  ## 2.6. Saving the raster ####
  filename <- paste0(country,'_', useCaseName, '_MODIS_NDVI_', Planting_year,'_', Harvesting_year, '_SG.tif')
  terra::writeRaster(EVI_SGfil, paste(pathOut, filename, sep="/"), filetype="GTiff", overwrite=overwrite)
  
}