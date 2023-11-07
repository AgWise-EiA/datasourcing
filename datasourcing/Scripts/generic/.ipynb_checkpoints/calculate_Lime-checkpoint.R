##################################################################################################
# Calculate lime rate for different countries and different crops using soil data from ISRIC soil grids
# The data includes - bulk density and exchangeable acidity for three depths(0-5, 5-15 & 15-30cm),
# exchangeable potassium, exchangeable calcium, exchangeable magnesium and exchangeable sodium for 20cm depth.

#' @description - this function uses three different lime rate calculation methods from the limer package.
#' It uses predefined values of target aluminium saturation for different crops to calculate the lime rates
#' of target countries, crops and use cases.  
#' 
#' @param country country name to be used to mask the cropland raster file to be used to further mask the calculated lime rate. 
#' @param useCaseName is the Name of the usecase
#' @param Crop the name of the crop for selecting the target aluminium saturation values to be used in the lime rate calculation. 
#' @example - calculateLimeRate("Rwanda", "useCase_Rwanda_RAB", "Potato")

calculateLimeRate <- function(country, useCaseName, Crop){
  
  #install & load packages
  packages_required <- c("terra", "tidyverse", "geodata", "remotes")
  # check and install packages that are not yet installed
  installed_packages <- packages_required %in% rownames(installed.packages())
  if(any(installed_packages == FALSE)){
    install.packages(packages_required[!installed_packages])}
  
  # load required packages
  invisible(lapply(packages_required, library, character.only = TRUE))
  
  # install and load limer package
  limer_pkg <- "gaiafrica/limer"
  installed_git_packages <- limer_pkg %in% rownames(installed.packages())
  if(any(installed_git_packages == FALSE)){
    remotes::install_github(limer_pkg, quiet = T)}
  require("limer", quietly = TRUE)
  
  # load the isric layers from path
  pathIn <- "/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data/Global_GeoData/Landing/Soil/soilGrids/profile"
    
  # load the country shape with level 0 from geodata package
  isocodes <- geodata::country_codes()
  ssa <- isocodes |> dplyr::filter(NAME == country) |> dplyr::pull(ISO3)
  ctry <- geodata::gadm(ssa, level = 0, path = ".")
  
  # load cropland data of africa
  cropland <- geodata::cropland(source='QED',path = "./cropland")
  
  # mask the cropland data with the country extent 
  crop_ctry <- cropland |> terra::crop(ctry) |> terra::mask(ctry)
  
  # load bulk density of the first three depths and calculate weighted average
  bdod <- list.files(path = pathIn, pattern = ("bdod_0-5cm|bdod_5-15cm|bdod_15-30cm"), full.names = T) |> 
    rast()
  bdod_mean <- (5 * bdod[[1]] + 10 * bdod[[3]] + 15 * bdod[[2]]) / 30
  
  # load exchangeable acidity of the first three depths and calculate weighted average
  acid_exch <- list.files(path = pathIn, pattern = ("af_acid-exch_0-5cm|af_acid-exch_5-15cm|af_acid-exch_15-30cm"), full.names = T) |> 
    rast()
  acid_exch_mean <- (5*acid_exch[[1]] + 10*acid_exch[[3]] + 15*acid_exch[[2]]) / 30
  
  # load 20cm depth exchangeable K, Ca, Mg & Na 
  exch_cakmgna <- list.files(path = pathIn, pattern = ("af_ca|af_k|af_mg|af_na"), full.names = T) |> 
    rast()
  
  # exchangeable acidity saturation
  ecec <- acid_exch_mean + exch_cakmgna [[1]] + exch_cakmgna [[2]] +
    exch_cakmgna [[3]] + exch_cakmgna [[4]]
  hp_sat <- 100 * acid_exch_mean / ecec
  hp_sat_acid <- terra::classify(hp_sat, rcl = cbind(-1, 10, 0))
  hp_sat_acid <- terra::ifel(hp_sat_acid != 0, 1, hp_sat_acid)
  
  # calculate calcium saturation
  ca_sat <- 100 * exch_cakmgna[[1]] / ecec
  
  # assemble all raster
  x <-
    c(acid_exch_mean,
      exch_cakmgna[[2]],
      exch_cakmgna[[1]],
      exch_cakmgna[[3]],
      exch_cakmgna[[4]],
      bdod_mean)
  names(x) <- c('exch_ac', 'exch_k', 'exch_Ca', 'exch_mg', 'exch_na', 'SBD')  
  
  # create a data frame for target aluminium saturation values for crops
  crops <- c("Cabbage", "Sorghum", "Carrot", "Barely", "Tomato", "Wheat", 
             "Faba bean", "Sweet potato", "Sunflower", "Haricot bean", "Pepper",  
             "Maize",  "Cotton",  "Groundnut", " Rape seed", "Potato", "Onion", 
             "Teff") |> tolower()
  value <- c(1, 10, 1, 10, 1, 10, 5, 10, 5, 20, 5, 20, 5, 20, 5, 30, 5, 40)
  tas <- data.frame(crops, value)
  
  # select tas value from crop parameter
  t <- tas |> dplyr::filter(crops == tolower(Crop)) |>
    dplyr::pull(value)
  
  # calculate lime rate using different methods
  # kamprath - the same for all crops
  caco3_kamprath <-
    limer::limeRate(
      x,
      method = 'ka',
      check_Ca = FALSE,
      unit = 't/ha',
      SD = 20
    )
  caco3_kamprath_filter <-
            (caco3_kamprath * hp_sat_acid) |> 
            terra::crop(crop_ctry) |> 
            terra::mask(crop_ctry) # for soils with exch acidity saturation > 10% only
  # cochrane
  caco3_cochrane <-
    limer::limeRate(
      x,
      method = 'co',
      check_Ca = FALSE,
      unit = 't/ha',
      SD = 20,
      TAS = t
    ) |> 
    terra::crop(crop_ctry) |> 
    terra::mask(crop_ctry)
  
  # aramburu-merlos - (litas) method
  caco3_merlos <-
    limer::limeRate(
      x,
      method = 'my',
      check_Ca = FALSE,
      unit = 't/ha',
      SD = 20,
      TAS = t) |> 
    terra::crop(crop_ctry) |> 
    terra::mask(crop_ctry)
  
  # write the calculated lime rate raster
  if(!exists("Lime")){
    dir.create("Lime", recursive = F)
  }
  
  savetoPath = paste("/home/jovyan/agwise-datasourcing/dataops/datasourcing/Data", 
                     useCaseName, Crop, "raw", "Lime", sep = "/")

  terra::writeRaster(caco3_kamprath_filter,
                     paste(savetoPath, 'lime_kamprath.tif', sep = "/"),
                     overwrite = TRUE)
  
  terra::writeRaster(caco3_cochrane,
                     paste(savetoPath, 'lime_cochrane.tif', sep = "/"),
                     overwrite = TRUE)
  
  terra::writeRaster(caco3_merlos,
                     paste(savetoPath, 'lime_merlos.tif', sep = "/"),
                     overwrite = TRUE)
}