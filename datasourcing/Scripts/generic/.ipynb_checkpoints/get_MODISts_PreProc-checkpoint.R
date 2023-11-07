# Input data preparation 

# Introduction: 
#This script serves as a tutorial on how to work with MODIS time series datasets, pre-process them, and apply basic operations to make it analysis ready.It covers :
#i.  Reading multi-temporal datasets and stacking them 
#ii. Subsetting the analysis year 
#iii.Extracting the area of interest 
#iv. Applying smoothing techniques to fill data gaps 
#v.  Saving the raster 

#### Getting started #######

# 1. Sourcing required packages -------------------------------------------
packages_required <- c("plotly", "raster", "rgdal", "gridExtra", "sp", "ggplot2", "caret", "signal", "timeSeries", "zoo", "pracma", "rasterVis", "RColorBrewer", "dplyr", "terra")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

# 2. Preparing and Smoothing the raster time series -------------------------------------------

smooth_rasterTS<-function(country, useCaseName, Crop, Planting_year, Harvesting_year, overwrite){
  
  #' @description Function that will preprocess the MODIS EVI (8 days) time series, including a masking-out of the cropped area and a gap-filling & smoothing of the TS with a Savitzky-Golay filter. If the cropping season is on a civil year, the initial TS should have 46 images, if the cropping season is between two civil years, the initial TS should have 92 images.
  #' @param country country name
  #' @param useCaseName use case name  name
  #' @param overwrite default is FALSE 
  #' @param Planting_year the planting year in integer
  #' @param Harvesting_year the harvesting year in integer
  #'
  #' @return raster files cropped from global data and the result will be written out in agwise-datasourcing/dataops/datasourcing/Data/useCaseName/MODISdata/transform/EVI
  #'
  #' @examples smooth_rasterTS(country = "Rwanda", useCaseName = "RAB", Planting_year = 2021, Harvesting_year = 2021, overwrite = TRUE)
  #' 
  #' 
  ## 2.1. Creating a directory to store the cropped and smoothed data ####
  pathOut <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", "MODISdata/transform/EVI", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ## 2.2. Read  and scale the relevant raster time series ####
  listRaster_EVI <-list.files(paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/","MODISdata/raw/EVI"), pattern=".tif$", full.names=T)
  
  stacked_EVI <- terra::rast(listRaster_EVI) #stack
  stacked_EVI <- stacked_EVI/10000 ## scaling to EVI value ranges from -1 to +1
  #plot(stacked_EVI)
  
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
  
  ## 2.4. Masking out of the cropped area ####
  cropmask <-terra::rast(list.files(paste0("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/","MODISdata/raw/CropMask"), pattern=".tif$", full.names = T))
  plot(cropmask)
  
  ## crop mask to MODIS extent 
  terra::ext(cropmask) <- terra::ext(stacked_EVI_s)
  ## crop and mask cropland
  stacked_EVI_s <- terra::crop(stacked_EVI_s, cropmask)
  stacked_EVI_s <- terra::mask(stacked_EVI_s, cropmask)
  
  ## 2.5. Applying Savitzky-Golay filter ####
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
    z<- substituteNA(v, type = "mean")
    MODIS.ts2 = ts(z, start=start, end=end, frequency=nblayers)
    sg=sgolayfilt(MODIS.ts2, p=3, n=9, ts=1) # run savitzky-golay filter## edit the function if required
  }
  ## Apply function on data ###
  EVI_SGfil <- terra::app(x=stacked_EVI_s, fun)
  
  ## Rename the SG filter time series
  names(EVI_SGfil) <- names(stacked_EVI_s)
  
  ## 2.6. Saving the raster ####
  filename <- paste0(country,'_', useCaseName, '_MODIS_EVI_', Planting_year,'_', Harvesting_year, '_SG.tif')
  terra::writeRaster(EVI_SGfil, paste(pathOut, filename, sep="/"), filetype="GTiff", overwrite=overwrite)
  
}