
#######################################
# 1. Get the Rice data from SAnDMan #
#######################################

source("~/agwise-datasourcing/dataops/datasourcing/Scripts/generic/get_ONA_data.R")

require("plyr")
require("dplyr")
require("tidyr")

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
id <- dss[dss$id_string == "Measure_Rice_PO",]$id
pd <- getONAdata(user = user, pw = pw, id = id) 
pd <- decomposeONAdata(pd)

#get the potato plot yield data and merge with trial and field identifiers:
ds1 <- pd[[3]] %>% #plot level data
  dplyr::filter(!is.na(grainsFW )) %>%
  dplyr::left_join(pd[[1]] %>% dplyr::select(L1, projectCode, FDID2, TLID2, today, start)) %>% #field level data
  dplyr::mutate(harvestDate = as.Date(today, format="%Y-%m-%d"),
                start = as.POSIXct(gsub("\\+.*","", start), format="%Y-%m-%dT%H:%M:%S", tz="UTC")) %>%
  dplyr::select(projectCode, FDID2, TLID2, POID2, POID2_label, start, harvestDate, plotLengthGrainYield, plotWidthGrainYield, nrPlants, grainsFW, grainsMC ) %>%
  dplyr::left_join(af) %>%
  dplyr::left_join(at)

#extracting treatment from label
ds1 <- ds1 %>% 
  dplyr::mutate(treat = sub("_[^_]+$", "", POID2_label),
                treat = gsub("_rep1", "", treat),
                treat = gsub("_rep2", "", treat),
                treat = gsub("_repA", "", treat),
                treat = gsub("_repB", "", treat),
                plotSize = as.numeric(plotLengthGrainYield) * as.numeric(plotWidthGrainYield),
                grainsFW = as.numeric(grainsFW),
                grainsMC = as.numeric(grainsMC),
                plantingDate = as.Date(plantingDate, format = "%Y-%m-%d"),
                harvestDate = as.Date(harvestDate, format = "%Y-%m-%d")) 


### create the relevant directory to hold all unprocessed data and save the data
country <- "Rwanda"
useCaseName <- "RAB"
Crop <- "Rice"

pathOut1 <- paste("~/agwise-datasourcing/dataops/datasourcing/Scripts/useCases/UseCase_", country, "_",useCaseName, "/", Crop, "/raw/", sep="")

if (!dir.exists(pathOut1)){
  dir.create(file.path(pathOut1), recursive = TRUE)
}


saveRDS(ds1, paste(pathOut1, "SAnDMan_Rice_fieldData.RDS", sep=""))


ds1 <- readRDS("~/agwise/AgWise_Data/data_sourcing/UseCase_Rwanda_RAB/Rice/raw/SAnDMan_Rice_fieldData.RDS")

## Elke sent corrections
ds1[ds1$TLID2 == "RSTLRW026770541739", ]$harvestDate <- "2022-05-30"
ds1[ds1$TLID2 == "RSTLRW036919072098", ]$harvestDate <- "2022-05-18"
ds1[ds1$TLID2 == "RSTLRW281355781680", ]$harvestDate <- "2022-06-22" 
ds1[ds1$TLID2 == "RSTLRW294554102273", ]$harvestDate <- "2022-05-25" 
ds1[ds1$TLID2 == "RSTLRW385986732208", ]$harvestDate <- "2022-06-11" 
ds1[ds1$TLID2 == "RSTLRW403048511536", ]$harvestDate <- "2022-06-20" 
ds1[ds1$TLID2 == "RSTLRW449053139260", ]$harvestDate <- "2022-05-30" 
ds1[ds1$TLID2 == "RSTLRW454227113825", ]$harvestDate <- "2022-06-10"
ds1[ds1$TLID2 == "RSTLRW533208884803", ]$harvestDate <- "2022-06-11"
ds1[ds1$TLID2 == "RSTLRW629848002897", ]$harvestDate <- "2022-07-20"
ds1[ds1$TLID2 == "RSTLRW648986319981", ]$harvestDate <- "2022-05-18"
ds1[ds1$TLID2 == "RSTLRW670200104616", ]$harvestDate <- "2022-05-18"
ds1[ds1$TLID2 == "RSTLRW674837259905", ]$harvestDate <- "2022-06-21"
ds1[ds1$TLID2 == "RSTLRW830169613407", ]$harvestDate <- "2022-05-25"
ds1[ds1$TLID2 == "RSTLRW912728024007", ]$harvestDate <- "2022-06-23"
ds1[ds1$TLID2 == "RSTLRW958413951169", ]$harvestDate <- "2022-06-23"


saveRDS(ds1, paste(pathOut1, "SAnDMan_Rice_fieldData.RDS", sep=""))



# TLID2	              plantingDate	harvestDate
# RSTLRW026770541739	17/01/2022	 30/05/2022
# RSTLRW036919072098	17/01/2022	 18/05/2022
# RSTLRW281355781680	01/02/2022	 22/06/2022
# RSTLRW294554102273	25/01/2022	 25/05/2022
# RSTLRW385986732208	10/02/2022	 11/06/2022
# RSTLRW403048511536	01/02/2022	 20/06/2022
# RSTLRW449053139260	18/01/2022	 30/05/2022
# RSTLRW454227113825	10/02/2022	 10/06/2022
# RSTLRW533208884803	10/02/2022	 11/06/2022
# RSTLRW629848002897	11/02/2022	 20/07/2022
# RSTLRW648986319981	16/01/2022	 18/05/2022
# RSTLRW670200104616	18/01/2022	 18/05/2022
# RSTLRW674837259905	01/02/2022	 21/06/2022
# RSTLRW830169613407	23/01/2022	 25/05/2022
# RSTLRW912728024007	10/02/2022	 23/06/2022
# RSTLRW958413951169	10/02/2022	 23/06/2022


