# Downloading of MODIS NDVI data for the Use Case

# Introduction: 
# This script allows the downloading of MODIS NDVI data (MOD13Q1 and MYD13Q1) used for the planting date exercise and crop type mapping, it allows to : 
# (1) Preparing the environment for the downloading
# (2) Interactive download of MODIS 
# (3) Renaming of the MODIS file 

#### Getting started #######

# 1. Sourcing required packages -------------------------------------------
packages_required <- c("MODIStsp", "mapedit", "leaflet", "shiny","shinydashboard","shinyFiles",
                       "shinyalert", "rappdirs","shinyjs",
                       "leafem", "mapedit", "magrittr", "reticulate", "sf", "rgee", "terra",
                       "geodata", "dplyr")

installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

lapply(packages_required, library, character.only = TRUE)

# load required packages
suppressWarnings(suppressPackageStartupMessages(invisible(lapply(packages_required, library, character.only = TRUE))))

# 2. Downloading MODIS Data -------------------------------------------

download_MODIS<-function(country,useCaseName, level=0, admin_unit_name=NULL, Start_year, End_year, overwrite=FALSE){

  #' @description 
  #' @param country country name
  #' @param useCaseName use case name  name
  #' @param level the admin unit level, in integer, to be downloaded -  Starting with 0 for country, then 1 for the first level of subdivision (from 1 to 3). Default is zero
  #' @param admin_unit_name name of the administrative level to be download, default is NULL (when level=0) , else, to be specified as a vector (eg. c("Nandi"))
  #' @param overwrite default is FALSE 
  #' @param Start_year the first year of the period of interest in integer
  #' @param End_year the last year of the period of interest in integer
  #'
  #' @return one VI layer each 8 days (both TERRA and AQUA) over the period of interest, in WGS 84 (EPSG 4326) and the result will be written out in agwise-datasourcing/dataops/datasourcing/Data/useCaseName/MODISdata/raw/
  #'
  #' @examples download_MODIS (country = "Kenya", useCaseName = "KALRO", level= 2, admin_unit_name = c("Butula"), Start_year = 2021, End_year = 2021, overwrite = TRUE)
  #' 
  #' 
  ## 2.1. Preparing the environment ####
  ### 2.1.1. Creating a directory to store the downloaded data ####
  
  pathOut <- paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country, "_",useCaseName, "/", "MODISdata/raw/", sep="")
  
  if (!dir.exists(pathOut)){
    dir.create(file.path(pathOut), recursive = TRUE)
  }
  
  ### 2.1.2. Get the country boundaries ####
  
  # Read the relevant shape file from gdam to be used to crop the global data
  countryShp <- geodata::gadm(country, level, path='.')

  # Case admin_unit_name == NULL
  if (is.null(admin_unit_name)){
    countryShp <-countryShp
  }
  
  # Case admin_unit_name is not null 
  if (!is.null(admin_unit_name)) {
    if (level == 0) {
      print("admin_unit_name is not null, level can't be eq. to 0 and should be set between 1 and 3")
    }
    if (level == 1){
      countryShp <- subset(countryShp, countryShp$NAME_1 %in% admin_unit_name)
    } 
    if (level == 2){
      countryShp <- subset(countryShp, countryShp$NAME_2 %in% admin_unit_name)
    }
    if (level == 3){
      countryShp <- subset(countryShp, countryShp$NAME_3 %in% admin_unit_name)
    }
  }

  terra::writeVector(countryShp,paste0(pathOut,"useCase_", country, "_",useCaseName,"_Boundary.shp"), overwrite=overwrite)
  spafile <- paste0(pathOut,"/useCase_", country, "_",useCaseName,"_Boundary.shp")

  ### 2.1.3. Define the downloading parameters
  user = "amitcimmyt" 
  password = "Ambica_81" 
  start_date = paste0(Start_year,'.01.01')
  end_date = paste0(End_year, '.12.31') 

  ## 2.2. Interactive MODIS data download ####
  #MODIStsp()
  
  setwd(pathOut)
  MODIStsp::MODIStsp_get_prodlayers("M*D13Q1")
  
  MODIStsp::MODIStsp(gui             = FALSE, 
           out_folder      = pathOut, ## Path to download the images
           out_folder_mod = pathOut,
           selcat          = "Ecosystem Variables - Vegetation Indices",
           selprod         = "Vegetation Indexes_16Days_250m (M*D13Q1)",## define which product of MODIS images
           bandsel         = c("NDVI"), 
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
           spafile = spafile, ## path to the boundary file of the study area in .shp format
           out_projsel = "User Defined",
           output_proj = 4326,
           resampling = "bilinear",
           delete_hdf = TRUE)
  
  ## 2.3. Rename file ####
  # Case MOD
  files <- list.files(path = paste0(pathOut, "useCase_", country, "_",useCaseName,"_Boundary/VI_16Days_250m_v61/NDVI/"), pattern = "MOD13Q*", full.names = TRUE)
  new_names <- sub(pattern = "MOD13Q1", replacement = country, x = files)
  file.rename(from = files, to = new_names)
  
  # Case MYD
  files <- list.files(path = paste0(pathOut, "useCase_", country, "_",useCaseName,"_Boundary/VI_16Days_250m_v61/NDVI/"), pattern = "MYD13Q*", full.names = TRUE)
  new_names <- sub(pattern = "MYD13Q1", replacement = country, x = files)
  file.rename(from = files, to = new_names)
}

# country = "Kenya"
# useCaseName = "Test"
# level = 2
# admin_unit_name = c("Budalangi", "Butula")
# Start_year = "2021"
# End_year = "2021"
# overwrite = TRUE

