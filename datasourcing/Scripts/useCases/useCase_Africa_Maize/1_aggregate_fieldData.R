
# Merge carob fertilizer with maize trials-ITTA genetic trials 
# and add fertilizer management
# Information about fertilizer management was provided by 
# Abebe and Olufemi
# These trials were to test resistance to striga and borer but we only keep
# the results of the ones that were not infected

setwd('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Africa_Maize/')

# path_in <- 
# path_out <- 
########
# Carob
#########
carob <- read.csv('Landing/fieldData/carob_fertilizer.csv')
carob$description <- NA
carob <- carob[carob$crop=='maize',]
carob$caveat <- 'Carob'
carob$caveat[carob$dataset_id == "ETH TAMASA data is not open source"] <- 'TAMASA-ETH'
carob$caveat[carob$dataset_id == "Ethiopia Wuletawu data is not open source"] <- 'Wuletawu-ETH'
carob$caveat[carob$dataset_id == "Ghana data from literature review"] <- 'LiteratureReview-Wuletawu-ETH'
carob$caveat[carob$dataset_id == "Lobell data is not open source"] <- 'Lobell'
carob$caveat[carob$dataset_id == "NGA PT TAMASA data is not open source"] <- 'TAMASA-NGA'
carob$caveat[carob$dataset_id == "NGA UAV TAMASA data is not open source"] <- 'TAMASA-NGA'
carob$caveat[carob$dataset_id == "NGA15 TAMASA data is not open source"] <- 'TAMASA-NGA'
carob$caveat[carob$dataset_id == "NGA16 TAMASA data is not open source"] <- 'TAMASA-NGA'
carob$caveat[carob$dataset_id == "Rwanda data MFR is not open source"] <- 'RWA-MFR'
carob$caveat[carob$dataset_id == "Rwanda data MLR is not open source"] <- 'RWA-MLR'
carob$caveat[carob$dataset_id == "TZA TAMASA data is not open source"] <- 'TAMASA-TZA'

# keep only for africa
sort(unique(carob$country))
ctry <- c("ANGOLA","Benin","Botswana","BOTSWANA","Burkina Faso",
          "Burundi","Cameroon","Chad","Côte d'Ivoire","Democratic Republic of the Congo",
          "Ethiopia","ETHIOPIA","Gabon","Gambia","Ghana","Guinea",
          "Guinea-Bissau","Kenya","KENYA","Lesotho","LESOTHO","Malawi",                          
          "MALAWI","Mali","Mozambique","MOZAMBIQUE","Nigeria","Rwanda","Senegal",
          "Sierra Leone","South Africa","SOUTH AFRICA","SOUTH AFRICA REP.",
          "South Sudan","Sudan","Tanzania","TANZANIA","Togo","Uganda","UGANDA",
          "ZAIRE","Zambia","ZAMBIA","Zimbabwe","ZIMBABWE")

carob <- carob[carob$country %in% ctry,]
carob <- carob[carob$latitude > -15000,]
carob$yield <- as.numeric(carob$yield)
carob <- carob[complete.cases(carob$yield),]

###################
# Maize ITTA trial
###################
mze_trial <- read.csv('Landing/fieldData/carob_maize_trials.csv')
mze_trial <- mze_trial[mze_trial$striga_trial==F & mze_trial$borer_trial==F &
                         mze_trial$striga_infected==F, ]
ctry <- c('Myanmar','Mexico','Indonesia','Thailand')
mze_trial <- mze_trial[!(mze_trial$country %in% ctry),]

sort(unique(mze_trial$description))
dd <- c("","NORMAL (75x20)","NORMAL (75x25)","NORMAL","NORMAL FERT.",
        "not provided","PREMIER SEEDS","A SEASON","Angaradebou, Bagou, Ina",
        "B SEASON","Ejura, Kpeve, Pokuase","HIGH DENSITY","LOW DENSITY","NN (75x25)")
mze_trial <- mze_trial[mze_trial$description %in% dd,]

# NA lat and long and fix
i <- which(is.na(mze_trial$latitude))
ii <- which(is.na(mze_trial$longitude))
dff <- mze_trial[ii,]

mze_trial$latitude[mze_trial$location=="M'vuazi"] <-  -5.439388
mze_trial$longitude[mze_trial$location=="M'vuazi"] <-  14.887269
mze_trial <- mze_trial[complete.cases(mze_trial$longitude),]

# I could remove High nitrogen, low nitrogen etc.
# Only keep "" and not provided description
mze_trial <- mze_trial[,c(1:15,21)]
mze_trial$caveat <- 'ITTA-Genetic-Camila'
mze_trial$N_fertilizer <- 120
mze_trial$P_fertilizer <- 60
mze_trial$K_fertilizer <- 60
mze_trial$N_splits <- 2

# Find difference in columns to create 
d <- setdiff(names(carob),names(mze_trial))
df <- data.frame(matrix(ncol = length(d), nrow = nrow(mze_trial)))
colnames(df) <- d
mze_trial <- cbind(mze_trial,df)

mze_trial <- mze_trial[names(carob)]

# Merge datasets
df <- rbind(carob,mze_trial)
# create a column were treatment= control, N, NP, NPK, etc
df$N_fertilizer <- as.numeric(df$N_fertilizer)
df$P_fertilizer <- as.numeric(df$P_fertilizer)
df$K_fertilizer <- as.numeric(df$K_fertilizer)

df$nut_response_eval <- NA

df$nut_response_eval[df$N_fertilizer == 0 & df$P_fertilizer == 0 
                      & df$K_fertilizer == 0] <- 'Control'
df$nut_response_eval[df$N_fertilizer != 0 & df$P_fertilizer != 0 
                      & df$K_fertilizer != 0] <- 'NPK'

df$nut_response_eval[df$N_fertilizer != 0 & df$P_fertilizer == 0 
                      & df$K_fertilizer == 0] <- 'N'
df$nut_response_eval[df$N_fertilizer != 0 & df$P_fertilizer != 0 
                      & df$K_fertilizer == 0] <- 'NP'
df$nut_response_eval[df$N_fertilizer != 0 & df$P_fertilizer == 0 
                      & df$K_fertilizer != 0] <- 'NK'

df$nut_response_eval[df$N_fertilizer == 0 & df$P_fertilizer != 0 
                      & df$K_fertilizer == 0] <- 'P'
df$nut_response_eval[df$N_fertilizer == 0 & df$P_fertilizer != 0 
                      & df$K_fertilizer != 0] <- 'PK'

df$nut_response_eval[df$N_fertilizer == 0 & df$P_fertilizer == 0 
                      & df$K_fertilizer != 0] <- 'K'
df$N_fertilizer <- round(df$N_fertilizer)
df$P_fertilizer <- round(df$P_fertilizer)
df$K_fertilizer <- round(as.numeric(df$K_fertilizer))

df <- df[!c(df$trial_id=='RS-MFR-1' & df$longitude >31),]

df$country[df$country == "ANGOLA"] <- 'Angola'
df$country[df$country == "BOTSWANA"] <- 'Botswana'
df$country[df$country == "ETHIOPIA"] <- 'Ethiopia'
df$country[df$country == "Guinea-Bissau"] <- 'Guinea'
df$country[df$country == "KENYA"] <- 'Kenya'
df$country[df$country == "LESOTHO"] <- 'Lesotho'
df$country[df$country == "MALAWI"] <- 'Malawi'
df$country[df$country == "MOZAMBIQUE"] <- 'Mozambique'
df$country[df$country == "SOUTH AFRICA" | df$country == "SOUTH AFRICA REP."] <- 'South Africa'
df$country[df$country == "TANZANIA"] <- 'Tanzania'
df$country[df$country == "UGANDA"] <- 'Uganda'
df$country[df$country == "ZAMBIA"] <- 'Zambia'
df$country[df$country == "ZIMBABWE"] <- 'Zimbabwe'

# Remove low yield
df <- df[!c(df$yield < 1000 & df$nut_response_eval != 'Control'),]
df <- df[complete.cases(df$yield),]
df$yield[df$yield <10] <- df$yield[df$yield <10] * 1000

saveRDS(df,'result/fieldData/0_merge_dataset.rds')
# write.csv(df,'result/fieldData/0_merge_dataset.csv')

# #######
# # Map
# #######
# library(terra)
# df_sp <- vect(df, geom=c("longitude", "latitude"),
#               crs="+proj=longlat +datum=WGS84",keepgeom=FALSE)
# 
# plot(vect('../shapefile/africa.shp'))
# plot(df_sp,pch=20,col='blue',add=T)
# 
# writeVector(df_sp,'../maize/data/0_merge_dataset.shp',overwrite=T)

