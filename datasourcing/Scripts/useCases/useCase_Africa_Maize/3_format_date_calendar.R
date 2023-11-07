######################################
# Format planting and harvesting date
# Camila Bonilla-Cedrez 2023
#####################################

library(terra)
library(stringr)

setwd('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Africa_Maize/')

df <- readRDS('result/fieldData/1_dataset.rds')
mze_cal <- rast('raw/Maize.crop.calendar.tif')

###############
# Planting date
###############
df$start_year <- str_split_fixed(df$planting_date, "/", 3)[,1]
df$start_month <- str_split_fixed(df$planting_date, "/", 3)[,2]
df$start_date <- str_split_fixed(df$planting_date, "/", 3)[,3]

df$start_year <- as.numeric(df$start_year)
df$start_month <- as.numeric(df$start_month)
df$start_date <- as.numeric(df$start_date)
df$case <- NA

i <- rowSums(is.na(df[,c('start_year','start_month','start_date')]))

# Date for year/month/day complete
i1 <- !is.na(rowSums(df[,c('start_year','start_month','start_date')]))
sum(i1)
df$case[i1] <- "all"

# Date for year/month
i2 <- !is.na(df$start_year) & !is.na(df$start_month) & i  == 1 
sum(i2)
df$case[i2] <- "start yr_mn"
df$start_date[i2] <- 15

# Date for year
i4 <- (!is.na(df$start_year)) & i == 2
sum(i4)
df$case[i4] <- "start yr"

# I need to extract crop calendar
st_yr <- df[df$case == "start yr",]
st_yr_sp <- vect(st_yr, geom=c("longitude", "latitude"),crs="+proj=longlat +datum=WGS84", 
                    keepgeom=FALSE)
pl <- extract(mze_cal$plant.start,st_yr_sp)
pl$pl_st <- as.Date(pl$plant.start, origin=as.Date("1960-01-01"))

st_yr$start_month <- str_split_fixed(pl$pl_st, "-", 3)[,2]
st_yr$start_date <- str_split_fixed(pl$pl_st, "-", 3)[,3]

st2 <- df[df$case !='start yr',]
df2 <- rbind(st2,st_yr)

df3 <- df2
df3$new_planting_date <- paste0(df3$start_year,'/',df3$start_month,'/',df3$start_date)
df3$new_planting_date <- as.Date(df3$new_planting_date, format='%Y/%m/%d')

##################
# Harvesting date
##################
df <- df3
df$end_year <- str_split_fixed(df$harvest_date, "/", 3)[,1]
df$end_month <- str_split_fixed(df$harvest_date, "/", 3)[,2]
df$end_date <- str_split_fixed(df$harvest_date, "/", 3)[,3]

df$end_year <- as.numeric(df$end_year)
df$end_month <- as.numeric(df$end_month)
df$end_date <- as.numeric(df$end_date)
df$case_end <- NA

i <- rowSums(is.na(df[,c('end_year','end_month','end_date')]))
# Date for year/month/day complete
i1 <- !is.na(rowSums(df[,c('end_year','end_month','end_date')]))
sum(i1)
df$case_end[i1] <- "all"

# Date for year/month
i2 <- !is.na(df$end_year) & !is.na(df$end_month) & i  == 1 
sum(i2)
df$case_end[i2] <- "end yr_mn"
df$end_date[i2] <- 15

df$case_end[is.na(df$case_end)] <- 'none'

df$new_harvest_date <- NA
dff1 <- df[df$case_end=='none',] 
dff1$new_harvest_date <- dff1$new_planting_date + 120

dff2 <- df[df$case_end=='end yr_mn',]
dff2$new_harvest_date <- paste0(dff2$end_year,'-',dff2$end_month,'-',dff2$end_date)

dff3 <- df[df$case_end=='all',]
dff3$new_harvest_date <- paste0(dff3$end_year,'-',dff3$end_month,'-',dff3$end_date)

df <- rbind(dff1,dff2,dff3)
df <- df[-1,]

df$dif <- df$new_harvest_date - df$new_planting_date
sort(unique(df$dif))

# nn <- df[df$dif<0,]
# unique(nn$new_harvest_date)

df$new_harvest_date[df$dif<0 & df$new_harvest_date == "2010-08-13"] <- "2011-08-13"
df$new_harvest_date[df$dif<0 & df$new_harvest_date == "2010-08-17"] <- "2011-08-17"
df$dif <- df$new_harvest_date - df$new_planting_date

df$new_harvest_date[df$dif < 0] <- NA 
df$new_harvest_date[is.na(df$new_harvest_date)] <-  df$new_planting_date[is.na(df$new_harvest_date)] + 120

df$new_harvest_date[df$dif > 200] <- NA 
df$new_harvest_date[is.na(df$new_harvest_date)] <-  df$new_planting_date[is.na(df$new_harvest_date)] + 120

df$dif <- df$new_harvest_date - df$new_planting_date
sort(unique(df$dif))

df$year_start <- str_split_fixed(df$planting_date, "/", 3)[,1]
df$year_end <- str_split_fixed(df$harvest_date, "/", 3)[,1]

df <- df[,c("dataset_id","country","site","longitude",
            "latitude","on_farm",
            "variety","nut_response_eval","yield","N_fertilizer",
            "N_splits","P_fertilizer","K_fertilizer",
            "year","new_planting_date","new_harvest_date",'dif',"caveat")]
names(df)[c(15:16,18)] <- c('planting_date','harvest_date','source')

df <- df[complete.cases(df[,c(4:5,9)]),]
df <- df[!duplicated(df[,c(1:18)]),]

# write.csv(df,'result/fieldData/2_crop_calendar.csv')
saveRDS(df,'result/fieldData/2_crop_calendar.rds')
