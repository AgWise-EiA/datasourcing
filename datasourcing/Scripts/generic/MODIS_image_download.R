### ************************** ###

## Amit Srivastava, IRRI SOuth Asia Regional Centre, Varanasi, India

#install.packages("MODIStsp")
#install.packages("mapedit")
library(MODIStsp)
library(mapedit)
library(rgdal)
## interactive download
#MODIStsp()  ## if you want to use GUI, go for this command else use below mentioned scripts

## non interactive mode
setwd("Define\\your\\working\\directory")
MODIStsp_get_prodlayers("M*D13Q1")

MODIStsp(gui             = FALSE, 
         out_folder      = "Output folder where data needs to be stored", 
         selprod         = "Vegetation Indexes_16Days_250m (M*D13Q1)",
         bandsel         = c("EVI"), 
         spatmeth        = "file",
         #quality_bandsel = "QA_usef", 
         sensor         = "Both",
         #out_res_sel = "User Defined",
         # out_res = 0.002786033, 
         user            = "amitcimmyt" ,  ## create a login to download images of MODIS from NASA LP DAAC
         password        = "**********",
         start_date      = "2022.04.24", ## define start date
         end_date        = "2022.05.20", ## define end date
         verbose         = FALSE,
         spafile = "Path of boundary files\\HRPB_STATE.shp",
         out_projsel = "User Defined",
         output_proj = 4326,
         resampling = "bilinear")

# Outputs are in this case in subfolder "MODIStsp/VI_16Days_1Km_v6" of 
# `base::tempdir()`: 

##out_fold <- file.path("C:\\GIS\\Research_tasks\\Andy_fire\\MODIS_2015_2020_EVI_PH\\EVI3", "MODIStsp/VI_16Days_250m_v6/") 
##list.files(out_fold)





