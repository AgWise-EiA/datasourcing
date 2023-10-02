##############################################
# Data quality inspection by data source and 
# final data source for analysis
# Camila Bonilla-Cedrez 2023
###############################################
library(terra)
library(geodata)

setwd('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Africa_Maize/')

df <- readRDS('result/fieldData/3_selected_country.rds')
df$ID <- paste0(df$longitude,'-',df$latitude)

sort(unique(df$country))
par(mar=c(5,5,5,5))
###############
# Burkina Faso
###############
bfa <- df[df$country=='Burkina Faso',]
bfa_sp <- gadm(country="BFA", level=1, path=tempdir())
plot(bfa_sp)
points(bfa$longitude,bfa$latitude,pch=20,col='blue')

length(unique(bfa$ID)) # unique locations
table(bfa$nut_response_eval)
# majority may come from genetic trial
table(bfa$source)

# Burkina Faso should be out
df <- df[df$country != 'Burkina Faso',]
# Most of the data come from Genetics trial which has only one NPK treatment

#############
# Burundi
#############
bdi <- df[df$country=='Burundi',]
df <- df[df$country !='Burundi',]

bdi_sp <- gadm(country="BDI", level=1, path=tempdir())
plot(bdi_sp)
points(bdi$longitude,bdi$latitude,pch=20,col='blue')

length(unique(bdi$ID)) # unique locations
table(bdi$nut_response_eval)
table(bdi$source)

# b <- boxplot(bdi$yield~bdi$nut_response_eval)
# 
# bdi <- bdi[!c(bdi$nut_response_eval=='Control' & bdi$yield > b$stats[5,1]),]
# bdi <- bdi[!c(bdi$nut_response_eval=='P' & bdi$yield > b$stats[5,5]),]
# bdi <- bdi[!c(bdi$nut_response_eval=='PK' & bdi$yield < b$stats[1,6]),]

df <- rbind(bdi,df)

#############
# Ethiopia
#############
eth <- df[df$country=='Ethiopia',]
df <- df[df$country !='Ethiopia',]

eth_sp <- gadm(country="ETH", level=1, path=tempdir())
plot(eth_sp)
points(eth$longitude,eth$latitude,pch=20,col='blue')

length(unique(eth$ID)) # unique locations
table(eth$nut_response_eval)
table(eth$source)

# Keep only local varieties from the genetic trials
eth_ge <- eth[eth$source=='ITTA-Genetic-Camila',]
eth <- eth[eth$source !='ITTA-Genetic-Camila',]

length(unique(eth_ge$ID))
length(unique(eth_ge$variety))
vr <- c("Local check","Local Check","LOCAL CHECK 1","LOCAL CHECK 2","Check")
eth_ge <- eth_ge[eth_ge$variety %in% vr,]

eth <- rbind(eth_ge,eth)

# b <- boxplot(eth$yield~eth$nut_response_eval)
# 
# eth <- eth[!c(eth$nut_response_eval=='Control' & eth$yield > b$stats[5,1]),]
# eth <- eth[!c(eth$nut_response_eval=='N' & eth$yield > b$stats[5,2]),]
# eth <- eth[!c(eth$nut_response_eval=='NK' & eth$yield > b$stats[5,3]),]
# eth <- eth[!c(eth$nut_response_eval=='NP' & eth$yield > b$stats[5,4]),]
# eth <- eth[!c(eth$nut_response_eval=='NPK' & eth$yield > b$stats[5,5]),]
# eth <- eth[!c(eth$nut_response_eval=='P' & eth$yield > b$stats[5,6]),]
# eth <- eth[!c(eth$nut_response_eval=='PK' & eth$yield > b$stats[5,7]),]

df <- rbind(eth,df)

#############
# Ghana
#############
gha <- df[df$country=='Ghana',]
df <- df[df$country !='Ghana',]

gha_sp <- gadm(country="GHA", level=1, path=tempdir())
plot(gha_sp)
points(gha$longitude,gha$latitude,pch=20,col='blue')

length(unique(gha$ID)) # unique locations
table(gha$nut_response_eval)
table(gha$source)

# Keep only local varieties from the genetic trials
gha_ge <- gha[gha$source=='ITTA-Genetic-Camila',]
gha <- gha[gha$source !='ITTA-Genetic-Camila',]

length(unique(gha_ge$ID))
sort(unique(gha_ge$variety))
vr <- c("Local check","Local Check","Local Check - Dodzi","Local Check - DORKE",                           
        "Local Check - DORKE SR","LOCAL CHECK 1","LOCAL CHECK 2",
        "Check","CHECK","Check-GH120 DYF/OP","CHECK 1","CHECK 2",
        "Check1-Obatanpa","Check2-Obatanpa")
gha_ge <- gha_ge[gha_ge$variety %in% vr,]

gha <- rbind(gha_ge,gha)

# b <- boxplot(gha$yield~gha$nut_response_eval)
# 
# gha <- gha[!c(gha$nut_response_eval=='Control' & gha$yield > b$stats[5,1]),]
# gha <- gha[!c(gha$nut_response_eval=='NPK' & gha$yield > b$stats[5,5]),]

df <- rbind(gha,df)

#############
# Kenya
#############
ken <- df[df$country=='Kenya',]
df <- df[df$country !='Kenya',]

ken_sp <- gadm(country="KEN", level=1, path=tempdir())
plot(ken_sp)
points(ken$longitude,ken$latitude,pch=20,col='blue')

length(unique(ken$ID)) # unique locations
table(ken$nut_response_eval)
table(ken$source)

# Keep only local varieties from the genetic trials
ken_ge <- ken[ken$source=='ITTA-Genetic-Camila',]
ken <- ken[ken$source !='ITTA-Genetic-Camila',]

length(unique(ken_ge$ID))
sort(unique(ken_ge$variety))
vr <- c('Check',"Local Check","LOCAL CHECK 1","LOCAL CHECK 2")
ken_ge <- ken_ge[ken_ge$variety %in% vr,]

ken <- rbind(ken_ge,ken)
# Remove outliers by nutrient response treat
# b <- boxplot(ken$yield~ken$nut_response_eval)
# 
# ken <- ken[!c(ken$nut_response_eval=='Control' & ken$yield > b$stats[5,1]),]
# ken <- ken[!c(ken$nut_response_eval=='N' & ken$yield > b$stats[5,2]),]
# ken <- ken[!c(ken$nut_response_eval=='NK' & ken$yield > b$stats[5,3]),]
# ken <- ken[!c(ken$nut_response_eval=='NP' & ken$yield > b$stats[5,4]),]
# ken <- ken[!c(ken$nut_response_eval=='NPK' & ken$yield > b$stats[5,5]),]
# ken <- ken[!c(ken$nut_response_eval=='P' & ken$yield > b$stats[5,6]),]
# ken <- ken[!c(ken$nut_response_eval=='PK' & ken$yield > b$stats[5,7]),]

df <- rbind(ken,df)

#############
# Malawi
#############
mwi <- df[df$country=='Malawi',]
df <- df[df$country !='Malawi',]

mwi_sp <- gadm(country="MWI", level=1, path=tempdir())
plot(mwi_sp)
points(mwi$longitude,mwi$latitude,pch=20,col='blue')

length(unique(mwi$ID)) # unique locations
table(mwi$nut_response_eval)
table(mwi$source)

# Keep only local varieties from the genetic trials
mwi_ge <- mwi[mwi$source=='ITTA-Genetic-Camila',]
mwi <- mwi[mwi$source !='ITTA-Genetic-Camila',]

length(unique(mwi_ge$ID))
sort(unique(mwi_ge$variety))
vr <- c("CHECK 1","CHECK 2")
mwi_ge <- mwi_ge[mwi_ge$variety %in% vr,]

mwi <- rbind(mwi_ge,mwi)
# Remove outliers by nutrient response treat
# b <- boxplot(mwi$yield~mwi$nut_response_eval)
# 
# mwi <- mwi[!c(mwi$nut_response_eval=='Control' & mwi$yield > b$stats[5,1]),]
# mwi <- mwi[!c(mwi$nut_response_eval=='N' & mwi$yield > b$stats[5,2]),]
# mwi <- mwi[!c(mwi$nut_response_eval=='NK' & mwi$yield > b$stats[5,3]),]
# mwi <- mwi[!c(mwi$nut_response_eval=='NP' & mwi$yield > b$stats[5,4]),]
# mwi <- mwi[!c(mwi$nut_response_eval=='NPK' & mwi$yield > b$stats[5,5]),]
# mwi <- mwi[!c(mwi$nut_response_eval=='P' & mwi$yield > b$stats[5,6]),]
# mwi <- mwi[!c(mwi$nut_response_eval=='PK' & mwi$yield > b$stats[5,7]),]

df <- rbind(mwi,df)

#############
# Mali
#############
mli <- df[df$country=='Mali',]
df <- df[df$country !='Mali',]

mli_sp <- gadm(country="MLI", level=1, path=tempdir())
plot(mli_sp)
points(mli$longitude,mli$latitude,pch=20,col='blue')

length(unique(mli$ID)) # unique locations
table(mli$nut_response_eval)
table(mli$source)

# Keep only local varieties from the genetic trials
mli_ge <- mli[mli$source=='ITTA-Genetic-Camila',]
mli <- mli[mli$source !='ITTA-Genetic-Camila',]

length(unique(mli_ge$ID))
sort(unique(mli_ge$variety))
vr <- c("Local check","Local Check","LOCAL CHECK 1","LOCAL CHECK 2",
        "Check","CHECK","Check 1","CHECK 1","Check 2","CHECK 2")
mli_ge <- mli_ge[mli_ge$variety %in% vr,]

mli <- rbind(mli_ge,mli)
# Remove outliers by nutrient response treat
# b <- boxplot(mli$yield~mli$nut_response_eval)

# mli <- mli[!c(mli$nut_response_eval=='Control' & mli$yield > b$stats[5,1]),]
# mli <- mli[!c(mli$nut_response_eval=='N' & mli$yield > b$stats[5,2]),]
# mli <- mli[!c(mli$nut_response_eval=='NK' & mli$yield > b$stats[5,3]),]
# mli <- mli[!c(mli$nut_response_eval=='NP' & mli$yield > b$stats[5,4]),]
# mli <- mli[!c(mli$nut_response_eval=='NPK' & mli$yield > b$stats[5,5]),]
# mli <- mli[!c(mli$nut_response_eval=='P' & mli$yield > b$stats[5,6]),]
# mli <- mli[!c(mli$nut_response_eval=='PK' & mli$yield > b$stats[5,7]),]

df <- rbind(mli,df)
#############
# Mozambique
#############
moz <- df[df$country=='Mozambique',]
df <- df[df$country !='Mozambique',]

moz_sp <- gadm(country="MOZ", level=1, path=tempdir())
plot(moz_sp)
points(moz$longitude,moz$latitude,pch=20,col='blue')

length(unique(moz$ID)) # unique locations
table(moz$nut_response_eval)
table(moz$source)

# Keep only local varieties from the genetic trials
moz_ge <- moz[moz$source=='ITTA-Genetic-Camila',]
moz <- moz[moz$source !='ITTA-Genetic-Camila',]

length(unique(moz_ge$ID))
sort(unique(moz_ge$variety))
vr <- c("Check")
moz_ge <- moz_ge[moz_ge$variety %in% vr,]

moz <- rbind(moz_ge,moz)
# Remove outliers by nutrient response treat
# b <- boxplot(moz$yield~moz$nut_response_eval)
# 
# moz <- moz[!c(moz$nut_response_eval=='NP' & moz$yield > b$stats[5,2]),]

# df <- rbind(moz,df)
# out for same reason as BFA
#############
# Nigeria
#############
nga <- df[df$country=='Nigeria',]
df <- df[df$country !='Nigeria',]

nga_sp <- gadm(country="NGA", level=1, path=tempdir())
plot(nga_sp)
points(nga$longitude,nga$latitude,pch=20,col='blue')

length(unique(nga$ID)) # unique locations
table(nga$nut_response_eval)
table(nga$source)

# Keep only local varieties from the genetic trials
nga_ge <- nga[nga$source=='ITTA-Genetic-Camila',]
nga <- nga[nga$source !='ITTA-Genetic-Camila',]

length(unique(nga_ge$ID))
v <- sort(unique(nga_ge$variety))
vr <- c("Check","CHECK","Check - K9901","Check 1","CHECK 1","Check 1 - TZEI 16 x TZEI 8",                    
        "Check 1 - TZEI 5 x TZEI 60","Check 2","CHECK 2","Local Check","Local Check-",                                  
        "Local Check - 2009 TZEE-OR2","Local Check - 2009 TZEE-OR2 STR",               
        "Local Check - TZE-Y DT STR C4 x TZEI 11","Local Check - TZE-Y DT STR C4 x TZEI 17",       
        "Local Check - TZE-Y Pop DT STR C4 x TZEI 17","Local Check - TZE-Y POP DT STR C4 x TZEI 17",   
        "Local Check - TZEE-W POP STR C5 x TZEEI 29","Local Check - TZEI 11 x TZE-Y DT STR C4",       
        "Local Check -2008 TZEE-W Pop STR F1",           
        "Local Check  - TZE-Y Pop DT STR C4 x TZEI 17","LOCAL CHECK 1","LOCAL CHECK 2")
nga_ge <- nga_ge[nga_ge$variety %in% vr,]

nga <- rbind(nga_ge,nga)
# Remove outliers by nutrient response treat
# b <- boxplot(nga$yield~nga$nut_response_eval)
# 
# nga <- nga[!c(nga$nut_response_eval=='Control' & nga$yield > b$stats[5,1]),]
# # nga <- nga[!c(nga$nut_response_eval=='N' & nga$yield > b$stats[5,2]),]
# nga <- nga[!c(nga$nut_response_eval=='NK' & nga$yield > b$stats[5,3]),]
# nga <- nga[!c(nga$nut_response_eval=='NP' & nga$yield > b$stats[5,4]),]
# nga <- nga[!c(nga$nut_response_eval=='NPK' & nga$yield > b$stats[5,5]),]
# nga <- nga[!c(nga$nut_response_eval=='P' & nga$yield > b$stats[5,6]),]
# nga <- nga[!c(nga$nut_response_eval=='PK' & nga$yield > b$stats[5,7]),]

df <- rbind(nga,df)

#############
# Rwanda
#############
rwa <- df[df$country=='Rwanda',]
df <- df[df$country !='Rwanda',]

rwa_sp <- gadm(country="RWA", level=1, path=tempdir())
plot(rwa_sp)
points(rwa$longitude,rwa$latitude,pch=20,col='blue')

length(unique(rwa$ID)) # unique locations
table(rwa$nut_response_eval)
table(rwa$source)

# Remove outliers by nutrient response treat
# b <- boxplot(rwa$yield~rwa$nut_response_eval)
# 
# rwa <- rwa[!c(rwa$nut_response_eval=='Control' & rwa$yield > b$stats[5,1]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='N' & rwa$yield > b$stats[5,2]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='N' & rwa$yield < b$stats[1,2]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='NK' & rwa$yield > b$stats[5,3]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='NP' & rwa$yield > b$stats[5,4]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='NP' & rwa$yield < b$stats[1,4]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='NPK' & rwa$yield > b$stats[5,5]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='NPK' & rwa$yield < b$stats[1,5]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='P' & rwa$yield > b$stats[5,6]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='PK' & rwa$yield > b$stats[5,7]),]
# rwa <- rwa[!c(rwa$nut_response_eval=='PK' & rwa$yield < b$stats[1,7]),]

df <- rbind(rwa,df)

#############
# Senegal
#############
sen <- df[df$country=='Senegal',]
df <- df[df$country !='Senegal',]

sen_sp <- gadm(country="SEN", level=1, path=tempdir())
plot(sen_sp)
points(sen$longitude,sen$latitude,pch=20,col='blue')

length(unique(sen$ID)) # unique locations
table(sen$nut_response_eval)
table(sen$source)

# Out same reason as BFA and MOZ
#############
# Tanzania
#############
tza <- df[df$country=='Tanzania',]
df <- df[df$country !='Tanzania',]

tza_sp <- gadm(country="TZA", level=1, path=tempdir())
plot(tza_sp)
points(tza$longitude,tza$latitude,pch=20,col='blue')

length(unique(tza$ID)) # unique locations
table(tza$nut_response_eval)
table(tza$source)

# Keep only local varieties from the genetic trials
tza_ge <- tza[tza$source=='ITTA-Genetic-Camila',]
tza <- tza[tza$source !='ITTA-Genetic-Camila',]

length(unique(tza_ge$ID))
sort(unique(tza_ge$variety))
vr <- c("CHECK 1","CHECK 2","LOCAL CHECK 1","LOCAL CHECK 2")
tza_ge <- tza_ge[tza_ge$variety %in% vr,]

tza <- rbind(tza_ge,tza)
# Remove outliers by nutrient response treat
# b <- boxplot(tza$yield~tza$nut_response_eval)
# 
# tza <- tza[!c(tza$nut_response_eval=='Control' & tza$yield > b$stats[5,1]),]
# tza <- tza[!c(tza$nut_response_eval=='N' & tza$yield > b$stats[5,3]),]
# tza <- tza[!c(tza$nut_response_eval=='NK' & tza$yield > b$stats[5,4]),]
# tza <- tza[!c(tza$nut_response_eval=='NP' & tza$yield > b$stats[5,5]),]
# tza <- tza[!c(tza$nut_response_eval=='NPK' & tza$yield > b$stats[5,6]),]
# tza <- tza[!c(tza$nut_response_eval=='P' & tza$yield > b$stats[5,7]),]
# tza <- tza[!c(tza$nut_response_eval=='PK' & tza$yield > b$stats[5,8]),]

df <- rbind(tza,df)
#############
# Zambia
#############
zmb <- df[df$country=='Zambia',]
df <- df[df$country !='Zambia',]

zmb_sp <- gadm(country="ZMB", level=1, path=tempdir())
plot(zmb_sp)
points(zmb$longitude,zmb$latitude,pch=20,col='blue')

length(unique(zmb$ID)) # unique locations
table(zmb$nut_response_eval)
table(zmb$source)

# Keep only local varieties from the genetic trials
zmb_ge <- zmb[zmb$source=='ITTA-Genetic-Camila',]
zmb <- zmb[zmb$source !='ITTA-Genetic-Camila',]

length(unique(zmb_ge$ID))
sort(unique(zmb_ge$variety))
vr <- c("Check","CHECK 1","CHECK 2","Local Check - Pool 16","LOCAL CHECK 1","LOCAL CHECK 2")
zmb_ge <- zmb_ge[zmb_ge$variety %in% vr,]

zmb <- rbind(zmb_ge,zmb)
# Remove outliers by nutrient response treat
# b <- boxplot(zmb$yield~zmb$nut_response_eval)
# 
# zmb <- zmb[!c(zmb$nut_response_eval=='Control' & zmb$yield > b$stats[5,1]),]
# zmb <- zmb[!c(zmb$nut_response_eval=='N' & zmb$yield > b$stats[5,2]),]
# zmb <- zmb[!c(zmb$nut_response_eval=='NP' & zmb$yield > b$stats[5,3]),]
# zmb <- zmb[!c(zmb$nut_response_eval=='NPK' & zmb$yield > b$stats[5,4]),]
# zmb <- zmb[!c(zmb$nut_response_eval=='P' & zmb$yield > b$stats[5,5]),]

df <- rbind(zmb,df)
#############
# Zimbabwe
#############
zwe <- df[df$country=='Zimbabwe',]
df <- df[df$country !='Zimbabwe',]

zwe_sp <- gadm(country="ZWE", level=1, path=tempdir())
plot(zwe_sp)
points(zwe$longitude,zwe$latitude,pch=20,col='blue')

length(unique(zwe$ID)) # unique locations
table(zwe$nut_response_eval)
table(zwe$source)

# Keep only local varieties from the genetic trials
zwe_ge <- zwe[zwe$source=='ITTA-Genetic-Camila',]
zwe <- zwe[zwe$source !='ITTA-Genetic-Camila',]

length(unique(zwe_ge$ID))
sort(unique(zwe_ge$variety))
vr <- c("Check","CHECK 1","CHECK 2","LOCAL CHECK 1","LOCAL CHECK 2")
zwe_ge <- zwe_ge[zwe_ge$variety %in% vr,]

zwe <- rbind(zwe_ge,zwe)
# Remove outliers by nutrient response treat
# b <- boxplot(zwe$yield~zwe$nut_response_eval)
# 
# zwe <- zwe[!c(zwe$nut_response_eval=='NP' & zwe$yield > b$stats[5,3]),]
# zwe <- zwe[!c(zwe$nut_response_eval=='NPK' & zwe$yield > b$stats[5,4]),]

df <- rbind(zwe,df)

df <- df[complete.cases(df$P_fertilizer),]
df <- df[complete.cases(df$K_fertilizer),]

# write.csv(df,'result/fieldData/4_selected_countries_genetic.csv')
saveRDS(df,'result/fieldData/4_aggregated_fieldData.rds')

