##############################################
# Exploratory analysis and country selection
# Camila Bonilla-Cedrez 2023
###############################################
library(terra)
library(ggplot2)

setwd('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Africa_Maize/')

df <- readRDS('result/fieldData/2_crop_calendar.rds')
ctry <- sort(unique(df$country))
df$ID <- paste0(df$latitude,'-',df$longitude)
df_sp <- vect(df, geom=c("longitude", "latitude"),
              crs="+proj=longlat +datum=WGS84",keepgeom=FALSE)
afr <- vect('raw/shp/africa.shp')

# Number of observation, unique locations, and distribution
nrow(df) # 63528 observations
lc <- unique(df$ID) # 3210 unique location
length(lc)

plot(afr)
plot(df_sp,pch=20,col=as.factor(df_sp$country),add=T)

# Based on distribution we may want to start with: Ethiopia, Nigeria, Ghana, Burkina(not sure)
# Rwanda, Tanzania and Malawi
# But besides distribution we need observation per country
# and different treatments by country

# for(c in 1 :length(ctry)){
#   dff <- df_sp[df_sp$country == ctry[c],]
#   par(ask=T)
#   plot(afr,main=ctry[c])
#   plot(dff,add=T,pch=20,col='blue')
# }

# Number of observation and unique location by country
ob_ct <- as.data.frame(table(df$country))
names(ob_ct) <- c('country','num_obs')

l <- list()
for(c in 1:length(ctry)){
  ct <- df[df$country==ctry[c],]
  l[[c]] <- data.frame(country=ctry[c],Unique_loc=length(unique(ct$ID)))
}
loc_by_country <- do.call(rbind,l)
loc_by_country <- loc_by_country[order(loc_by_country$Unique_loc,decreasing=T),]

ff <- merge(ob_ct,loc_by_country,by='country')
ff <- ff[order(ff$num_obs,decreasing=T),]

# Number of observation by NPK treatment
npk <- as.data.frame(table(df$nut_response_eval))
names(npk) <- c('Nutrient_response','Numb_obs')

# Number of observation by NPK treatment by country
l <- list()
for(c in 1:length(ctry)){
  ct <- df[df$country==ctry[c],]
  l[[c]] <- data.frame(country=ctry[c],obs=table(ct$nut_response_eval))
}
npk_by_country <- do.call(rbind,l)

ff2 <- merge(ff,npk_by_country,by='country')
write.csv(ff2,'../../../../../agwise-datacuration/dataops/datacuration/Data/useCase_Africa_Maize/transform/descriptive_byCountry.csv')


# Yield by NPK
par(mar=c(4,4,4,4))
boxplot(df$yield~df$nut_response_eval,ylab='Yield (kg/ha)',xlab='Nutrient response treatment')

# Yield by NPK by country
# for(c in 1:length(ctry)){
#   ct <- df[df$country==ctry[c],]
#   par(ask=T)
#   boxplot(ct$yield~ct$nut_response_eval,main=ctry[c],ylab='Yield (kg/ha)',
#           xlab='Nutrient response treatment')
# }

p <- ggplot(data = df, aes(x=nut_response_eval, y=yield)) + 
  geom_boxplot(aes(fill=nut_response_eval))
p + facet_wrap( ~ country, scales="free")

# How many season/years
yr <- as.data.frame(table(df$year))
names(yr) <- c('year','numb_obs')

# Check yield by year 
boxplot(df$yield~df$year,ylab='Yield (kg/ha)',xlab='Year')
# Yield by year within NPK treat
ctrl <- df[df$nut_response_eval=='Control',]
boxplot(ctrl$yield~ctrl$year,main='Yield by year for Control',
        ylab='Yield (kg/ha)',xlab='Year')

npk <- df[df$nut_response_eval=='NPK',]
boxplot(npk$yield~npk$year,main='Yield by year for NPK',
        ylab='Yield (kg/ha)',xlab='Year')

# Keep the countries with more obs, location and NPK treatment
ctry <- c('Nigeria','Ethiopia','Ghana','Tanzania','Malawi','Mali',
          'Kenya','Rwanda','Zambia','Burkina Faso','Burundi','Zimbabwe',
          'Mozambique','Senegal')
df2 <- df[df$country %in% ctry,]

# write.csv(df2,'result/fieldData/3_selected_country.csv')
saveRDS(df2,'result/fieldData/3_selected_country.rds')


