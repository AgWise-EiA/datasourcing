#################################################################################################################
## sourcing required packages 
#################################################################################################################
packages_required <- c("terra", "sf", "rgl", "rgdal", "sp", "geodata", "tidyverse", "lubridate")

# check and install packages that are not yet installed
installed_packages <- packages_required %in% rownames(installed.packages())
if(any(installed_packages == FALSE)){
  install.packages(packages_required[!installed_packages])}

# load required packages
invisible(lapply(packages_required, library, character.only = TRUE))


#################################################################################################################
## explore the data
#################################################################################################################
# V1data <- readRDS("D:Maize/dataset_V1.rds")

V2data <- read.csv("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Africa_Maize/raw/cropCalendar_V2.csv")
V2data$country <- toupper(V2data$country)
str(V2data)
head(V2data)
unique(V2data[V2data$yield < 100, ]$country)
unique(V2data[V2data$yield < 100, ]$nut_response_eval)


ds_geospatial <- V2data %>%
  dplyr::select(c(country, longitude, latitude, planting_date, harvest_date)) %>%
  unique() 
ds_geospatial <- ds_geospatial[complete.cases(ds_geospatial), ]


countrylocs <- as.data.frame(table(ds_geospatial$country))
countrylocs <- countrylocs[order(countrylocs$Freq, decreasing = TRUE),]

### starting with ...
countries <- c("ETHIOPIA", "NIGERIA", "MALAWI", "TANZANIA", "GHANA", "KENYA", "RWANDA", "MALI", "ZIMBABWE", "MOZAMBIQUE", "ZAMBIA")
ds_geospatial$plDate <- mdy(ds_geospatial$planting_date)
ds_geospatial$hvDate <- mdy(ds_geospatial$harvest_date)
ds_geospatial <- ds_geospatial[ds_geospatial$country %in% countries, ]
str(ds_geospatial)










