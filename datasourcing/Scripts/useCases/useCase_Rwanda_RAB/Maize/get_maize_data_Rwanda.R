


source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_ONA_data.R")

# set path to save output
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Potato"

pathOut1 <- paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_", useCaseName, "/", Crop, "/raw/", sep="")



if (!dir.exists(pathOut1)){
  dir.create(file.path(pathOut1), recursive = TRUE)
}

#######################################
# 1. Get the potato data from SAnDMan #
#######################################

#downloading the data
user <- "user" #replace by your ONA username
pw   <- "pw"   #replace by your ONA password
#get the list of all datasets of user...

dss <- findONAdatasets(user = user, pw = pw)

#download and decompose the assign field/trial/plot data:
id <- dss[dss$id_string == "Assign_FDTLPO",]$id
ad <- getONAdata(user = user, pw = pw, id = id) 
ad <- decomposeONAdata(ad)

#get the field identifiers
af <- ad[[1]] %>%
  dplyr::filter(grepl("FD", entity)) %>%
  dplyr::select(FDID2_new, FD_name_new, FD_owner, HHID, lat, lon) %>%
  dplyr::rename(FDID2 = FDID2_new,
                FD_name = FD_name_new)

#get the trial identifiers
at <- ad[[3]] %>%
  dplyr::left_join(ad[[1]] %>% dplyr::select(L1, entity, season, plantingDate, expCode)) %>%
  dplyr::filter(grepl("TL", entity),
                L2 == "trial") %>%
  dplyr::select(TLID2_new, TL_name_new, season, plantingDate, expCode) %>%
  dplyr::mutate(plantingDate = as.Date(plantingDate, format="%Y-%m-%d")) %>%
  dplyr::rename(TLID2 = TLID2_new,
                TL_name = TL_name_new)

#download and decompose the potato plot level data:
id <- dss[dss$id_string == "Measure_Potato_PO",]$id
pd <- getONAdata(user = user, pw = pw, id = id) 
pd <- decomposeONAdata(pd)

#get the potato plot yield data and merge with trial and field identifiers:
ds1 <- pd[[3]] %>% #plot level data
  dplyr::filter(!is.na(tubersFW) | !is.na(tubersMarketableFW)) %>%
  dplyr::left_join(pd[[1]] %>% dplyr::select(L1, projectCode, FDID2, TLID2, today, start)) %>% #field level data
  dplyr::mutate(harvestDate = as.Date(today, format="%Y-%m-%d"),
                start = as.POSIXct(gsub("\\+.*","", start), format="%Y-%m-%dT%H:%M:%S", tz="UTC")) %>%
  dplyr::select(projectCode, FDID2, TLID2, POID2, POID2_label, start, harvestDate, plotLength, plotWidth, nrPlants, tubersFW, tubersMarketableFW) %>%
  dplyr::left_join(af) %>%
  dplyr::left_join(at)

#extracting treatment from label
ds1 <- ds1 %>% 
  dplyr::mutate(treat = sub("_[^_]+$", "", POID2_label),
                treat = gsub("_rep1", "", treat),
                treat = gsub("_rep2", "", treat),
                treat = gsub("_repA", "", treat),
                treat = gsub("_repB", "", treat),
                plotSize = as.numeric(plotLength) * as.numeric(plotWidth),
                tubersFW = as.numeric(tubersFW),
                tubersMarketableFW = as.numeric(tubersMarketableFW),
                plantingDate = as.Date(plantingDate, format = "%Y-%m-%d")) %>%
  dplyr::filter(treat != "",
                expCode != "RS-PLR-1") #removing lime trials without varying NPK rates


saveRDS(ds1, paste(pathOut1, "SAnDMan_potato_fieldData.RDS", sep=""))


###################################
# 2. Get the RwaSIS season 1 data, this is added in Compiling all potato fieldData Rwanda.R
###################################

# ds2 <- readRDS(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_",useCaseName, "/",Crop, "/raw/RwaSIS_potato_2022A_fieldData.RDS",sep=""))
# 
# ds2 <- ds2 %>%
#   dplyr::mutate(expCode = "RS-PFR-1",
#                 plantingDate = as.Date(planting_date, format="%Y-%m-%d"),
#                 harvestDate = as.Date(harvest_date, format="%Y-%m-%d"))
# 
# saveRDS(ds2 ,(paste("~/agwise-datacuration/dataops/datacuration/Data/useCase_", country, "_",useCaseName, "/",Crop, "/raw/RwaSIS_potato_2022A_fieldData.RDS",sep="")))

#####################################
# 3. Preparing the IFDC potato data #
#####################################



ds3 <- read.csv(paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country,"_",useCaseName, "/", Crop, "/Landing/fieldData/IFDC_Rwanda potato 2014B season data subset.csv", sep=""))
ds3_nutrates <- read.csv(paste("~/agwise-datasourcing/dataops/datasourcing/Data/useCase_", country,"_",useCaseName, "/", Crop, "/Landing/fieldData/IFDC_Rwanda potato 2014B season treat nutrates.csv", sep=""))
ds3 <- ds3 %>%
  gather(treat, TY, control:all_redK) %>%
  mutate(season = "2014B",
         expCode = "IFDC",
         FDID = paste0("IFDC_", siteNr),
         TLID = FDID, 
         plantingDate = NA,
         harvestDate = NA) %>%
  join(ds3_nutrates) %>%
  dplyr:: select(expCode, FDID, TLID, lat, lon, season, plantingDate, harvestDate, treat, N, P, K, TY)

ds3[ds3$TLID == "IFDC_3",]$lon <- ds3[ds3$TLID == "IFDC_3",]$lon - 1 #wrong GPS entry 

saveRDS(ds3, paste(pathOut1, "IFDC_potato_2014B_fieldData.RDS", sep=""))
