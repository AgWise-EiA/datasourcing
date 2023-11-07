
library(stringr)
library(terra)

setwd('~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Africa_Maize/')

df <- readRDS('result/fieldData/0_merge_dataset.rds')
df <- df[df$country != 'Egypt',]
df$yield <- as.numeric(df$yield)
df <- df[complete.cases(df$yield),]

# Fix lat/long NA
i <- which(is.na(df$latitude))
ii <- which(is.na(df$longitude))

# Keep data with lat/long
df <- df[complete.cases(df$longitude),]

# Remove N application higher than 250
df <- df[df$N_fertilizer < 300,]
df <- df[df$yield < 15000,]
# plot(df$yield~df$N_fertilizer,ylab='Yield (kg/ha)',xlab='N appl (kg/ha)')
# boxplot(df$yield)

# Horrible script but messy formats
# Improve dates
# Planting date
sort(unique(df$planting_date))
df$planting_date[df$planting_date=="NA-NA-NA"|df$planting_date==""] <- NA
df$planting_date[df$planting_date=="1999/1999/NA"] <- 1999
df$planting_date[df$planting_date=="-2000"] <- 2000
df$planting_date[df$planting_date=="2000/2000/NA"] <- 2000
df$planting_date[df$planting_date=="2001/2001/NA"] <- 2001
df$planting_date[df$planting_date=="2003/2003/NA"] <- 2003
df$planting_date[df$planting_date=="2004/2004/NA"] <- 2004
df$planting_date[df$planting_date=="2005/2005/NA"] <- 2005
df$planting_date[df$planting_date=="2006/2006/NA"] <- 2006
df$planting_date[df$planting_date=="2007/2007/NA"] <- 2007
df$planting_date[df$planting_date=="-2008"] <- 2008
df$planting_date[df$planting_date=="2015-2015-NA-NA"] <- 2015 
df$planting_date[df$planting_date=="2015-NA-NA"] <- 2015     
df$planting_date[df$planting_date=="2016-2016-NA-NA"] <- 2016 
df$planting_date[df$planting_date=="2016-NA-NA"] <- 2016
df$planting_date[df$planting_date=="2017-NA-NA"] <- 2017
df$planting_date[df$planting_date=="2016-2016-6"] <- '2016/06'
df$planting_date[df$planting_date=="2016-2016-7"] <- '2016/07'

df$planting_date[df$planting_date=="1992;1993"] <- '1992'
df$planting_date[df$planting_date=="1996;1997"] <- '1996'
df$planting_date[df$planting_date=="1996;1997"] <- '1996'
df$planting_date[df$planting_date=="2003;2004"] <- '2003'
df$planting_date[df$planting_date=="2017/2018"] <- '2017'

df$planting_date <- gsub("-", "/", df$planting_date)
df$planting_date <- gsub(";", "/", df$planting_date)

df$planting_date[df$planting_date=="1998/1999"] <- '1998'

df$planting_date[df$planting_date=="20/Oct/21"] <- '2021/10/20'
df$planting_date[df$planting_date=="20/Sep/21"] <- '2021/09/20'

df$planting_date[df$planting_date=="01/Oct/21"] <- '2021/10/01'
df$planting_date[df$planting_date=="02/Oct/21"] <- '2021/10/02'
df$planting_date[df$planting_date=="06/Oct/21"] <- '2021/10/06'
df$planting_date[df$planting_date=="08/Sep/21"] <- '2021/09/08'
df$planting_date[df$planting_date=="09/Oct/21"] <- '2021/10/09'
df$planting_date[df$planting_date=="09/Sep/21"] <- '2021/09/09'

df$planting_date[df$planting_date=="10/Jan/21"] <- '2021/01/10'
df$planting_date[df$planting_date=="10/Jun/21"] <- '2021/06/10'
df$planting_date[df$planting_date=="10/Mar/21"] <- '2021/03/10'
df$planting_date[df$planting_date=="10/Nov/21"] <- '2021/11/10'
df$planting_date[df$planting_date=="10/Oct/21"] <- '2021/10/10'
df$planting_date[df$planting_date=="10/Sep/21"] <- '2021/09/10'
df$planting_date[df$planting_date=="11/Jun/21"] <- '2021/06/11'

df$planting_date[df$planting_date=="11/Sep/21"] <- '2021/09/11'
df$planting_date[df$planting_date=="12/Oct/21"] <- '2021/10/12'
df$planting_date[df$planting_date=="13/Oct/21"] <- '2021/10/13'
df$planting_date[df$planting_date=="13/Oct/22"] <- '2021/10/13'
df$planting_date[df$planting_date=="13/Sep/21"] <- '2021/09/13'
df$planting_date[df$planting_date=="14/Oct/21"] <- '2021/10/14'
df$planting_date[df$planting_date=="14/Sep/21"] <- '2021/09/14'

df$planting_date[df$planting_date=="14/Sep/21"] <- '2021/09/14'
df$planting_date[df$planting_date=="15/Oct/21"] <- '2021/10/15'
df$planting_date[df$planting_date=="15/Sep/21"] <- '2021/09/15'
df$planting_date[df$planting_date=="16/Oct/21"] <- '2021/10/16'
df$planting_date[df$planting_date=="16/Oct/21"] <- '2021/10/16'
df$planting_date[df$planting_date=="16/Sep/21"] <- '2021/09/16'
df$planting_date[df$planting_date=="17/Sep/21"] <- '2021/09/17'
df$planting_date[df$planting_date=="18/Sep/21"] <- '2021/09/18'

df$planting_date[df$planting_date=="21/Oct/21"] <- '2021/10/21'
df$planting_date[df$planting_date=="21/Sep/21"] <- '2021/09/21'
df$planting_date[df$planting_date=="22/Oct/21"] <- '2021/10/22'
df$planting_date[df$planting_date=="22/Sep/21"] <- '2021/09/22'
df$planting_date[df$planting_date=="23/Sep/21"] <- '2021/09/23'
df$planting_date[df$planting_date=="24/Sep/21"] <- '2021/09/24'
df$planting_date[df$planting_date=="25/Oct/21"] <- '2021/10/25'
df$planting_date[df$planting_date=="25/Sep/21"] <- '2021/09/25'
df$planting_date[df$planting_date=="26/Oct/21"] <- '2021/10/26'

df$planting_date[df$planting_date=="27/Sep/21"] <- '2021/09/27'
df$planting_date[df$planting_date=="28/Oct/21"] <- '2021/10/28'
df$planting_date[df$planting_date=="28/Sep/21"] <- '2021/09/28'
df$planting_date[df$planting_date=="29/Oct/21"] <- '2021/10/29'
df$planting_date[df$planting_date=="29/Sep/21"] <- '2021/09/29'
df$planting_date[df$planting_date=="30/Sep/21"] <- '2021/09/30'
df$planting_date[df$planting_date=="10/Feb/21"] <- '2021/10/02'
df$planting_date[df$planting_date=="14/Sep/22"] <- '2021/09/14'
df$planting_date[df$planting_date=="10/Dec/21"] <- '2021/12/10'
df$planting_date[df$planting_date=="10/Apr/21"] <- '2021/04/10'

df$planting_date[df$planting_date=="10/10/2021"] <- '2021/10/10'
df$planting_date[df$planting_date=="10/11/2021"] <- '2021/10/11'
df$planting_date[df$planting_date=="10/12/2021"] <- '2021/10/12'
df$planting_date[df$planting_date=="10/13/2021"] <- '2021/10/13'
df$planting_date[df$planting_date=="10/14/2021"] <- '2021/10/14'
df$planting_date[df$planting_date=="10/15/2021"] <- '2021/10/15'
df$planting_date[df$planting_date=="10/1/2021"] <- '2021/10/01'

df$planting_date[df$planting_date=="10/16/2021"] <- '2021/10/16'
df$planting_date[df$planting_date=="10/2/2021"] <- '2021/10/02'
df$planting_date[df$planting_date=="10/21/2021"] <- '2021/10/21'
df$planting_date[df$planting_date=="10/23/2021"] <- '2021/10/23'
df$planting_date[df$planting_date=="10/25/2021"] <- '2021/10/25'
df$planting_date[df$planting_date=="10/29/2021"] <- '2021/10/29'
df$planting_date[df$planting_date=="10/3/2021"] <- '2021/10/03'

df$planting_date[df$planting_date=="10/4/2021"] <- '2021/10/04'
df$planting_date[df$planting_date=="10/5/2021"] <- '2021/10/05'
df$planting_date[df$planting_date=="10/6/2021"] <- '2021/10/06'
df$planting_date[df$planting_date=="10/7/2021"] <- '2021/10/07'
df$planting_date[df$planting_date=="10/8/2021"] <- '2021/10/08'

df$planting_date[df$planting_date=="9/18/2021"] <- '2021/09/18'
df$planting_date[df$planting_date=="9/21/2021"] <- '2021/09/21'
df$planting_date[df$planting_date=="9/22/2021"] <- '2021/09/22'
df$planting_date[df$planting_date=="9/23/2021"] <- '2021/09/23'

df$planting_date[df$planting_date=="9/24/2021"] <- '2021/09/24'
df$planting_date[df$planting_date=="9/25/2021"] <- '2021/09/25'
df$planting_date[df$planting_date=="9/27/2021"] <- '2021/09/27'
df$planting_date[df$planting_date=="9/28/2021"] <- '2021/09/28'
df$planting_date[df$planting_date=="9/29/2021"] <- '2021/09/29'
df$planting_date[df$planting_date=="9/30/2021"] <- '2021/09/30'

df$planting_date[df$dataset_id=="NGA15 TAMASA data is not open source"] <- 2015 

# Harvest date
sort(unique(df$harvest_date))
df$harvest_date[df$harvest_date=="NA/NA/NA"] <- NA
df$harvest_date[df$harvest_date==""] <- NA
df$harvest_date[df$harvest_date=="1999/NA/NA"] <- 1999
df$harvest_date[df$harvest_date=="2000/NA/NA"] <- 2000
df$harvest_date[df$harvest_date=="2001/NA/NA"] <- 2001
df$harvest_date[df$harvest_date=="2003/NA/NA"] <- 2003
df$harvest_date[df$harvest_date=="2004/NA/NA"] <- 2004
df$harvest_date[df$harvest_date=="2005/NA/NA"] <- 2005
df$harvest_date[df$harvest_date=="2006/NA/NA"] <- 2006
df$harvest_date[df$harvest_date=="2007/NA/NA"] <- 2007
df$harvest_date[df$harvest_date=="2015-2015-7"] <- '2015/07' 
df$harvest_date[df$harvest_date=="2015-2016-10"] <- '2016/10'
df$harvest_date[df$harvest_date=="2016-2016-5"] <- '2016/05'
df$harvest_date[df$harvest_date=="2016-2016-6"] <- '2016/06'
df$harvest_date[df$harvest_date=="2016-2016-7"] <- '2016/07'
df$harvest_date[df$harvest_date=="2016-2016-8"] <- '2016/08'
df$harvest_date[df$harvest_date=="2016-2016-9"] <- '2016/09'
df$harvest_date[df$harvest_date=="2016-2016-NA-NA"] <- 2016
df$harvest_date[df$harvest_date=="2016-NA-NA"] <- 2016
df$harvest_date[df$harvest_date=="2017-2017-10"] <- '2017/10'
df$harvest_date[df$harvest_date=="2017-2017-5"] <- '2017/05'
df$harvest_date[df$harvest_date=="2017-2017-7"] <- '2017/07'
df$harvest_date[df$harvest_date=="2015-2015-NA-NA"] <- 2015
df$harvest_date[df$harvest_date=="2016-2016-10"] <- '2016-10'
df$harvest_date[df$harvest_date=="NA-2016-10"] <- '2016-10'

df$harvest_date <- gsub("-", "/", df$harvest_date)

df$harvest_date[df$harvest_date=="02/Aug/22"] <- '2022/08/02'
df$harvest_date[df$harvest_date=="02/Dec/22"] <- '2022/12/02'
df$harvest_date[df$harvest_date=="02/Feb/22"] <- '2022/02/02'
df$harvest_date[df$harvest_date=="02/Mar/22"] <- '2022/03/02'
df$harvest_date[df$harvest_date=="02/Nov/22"] <- '2022/11/02'
df$harvest_date[df$harvest_date=="03/Apr/22"] <- '2022/04/03'

df$harvest_date[df$harvest_date=="03/Aug/22"] <- '2022/08/03'
df$harvest_date[df$harvest_date=="03/Dec/22"] <- '2022/12/02'
df$harvest_date[df$harvest_date=="03/Feb/22"] <- '2022/02/02'
df$harvest_date[df$harvest_date=="03/Jan/22"] <- '2022/01/02'
df$harvest_date[df$harvest_date=="03/Jul/22"] <- '2022/07/02'
df$harvest_date[df$harvest_date=="03/Jun/22"] <- '2022/06/02'

df$harvest_date[df$harvest_date=="03/Mar/22"] <- '2022/03/03'
df$harvest_date[df$harvest_date=="03/Nov/22"] <- '2022/11/03'
df$harvest_date[df$harvest_date=="03/Oct/22"] <- '2022/10/03'
df$harvest_date[df$harvest_date=="03/Sep/22"] <- '2022/09/03'
df$harvest_date[df$harvest_date=="04/Apr/22"] <- '2022/04/04'
df$harvest_date[df$harvest_date=="04/Aug/22"] <- '2022/08/04'     

df$harvest_date[df$harvest_date=="04/Feb/22"] <- '2022/02/04'
df$harvest_date[df$harvest_date=="04/Jun/22"] <- '2022/06/04'
df$harvest_date[df$harvest_date=="04/Mar/22"] <- '2022/03/04'
df$harvest_date[df$harvest_date=="04/May/22"] <- '2022/05/04'
df$harvest_date[df$harvest_date=="04/Sep/22"] <- '2022/09/04'
df$harvest_date[df$harvest_date=="05/Feb/22"] <- '2022/02/05'

df$harvest_date[df$harvest_date=="08/Feb/22"] <- '2022/02/08'
df$harvest_date[df$harvest_date=="10/Feb/22"] <- '2022/02/10'
df$harvest_date[df$harvest_date=="13/Feb/22"] <- '2022/02/13'
df$harvest_date[df$harvest_date=="14/Apr/22"] <- '2022/04/14'
df$harvest_date[df$harvest_date=="14/Feb/22"] <- '2022/02/14'
df$harvest_date[df$harvest_date=="14/Mar/22"] <- '2022/03/14'

df$harvest_date[df$harvest_date=="15/Mar/22"] <- '2022/03/15'
df$harvest_date[df$harvest_date=="16/Mar/22"] <- '2022/03/16'
df$harvest_date[df$harvest_date=="17/Feb/22"] <- '2022/02/17'
df$harvest_date[df$harvest_date=="17/Mar/22"] <- '2022/03/17'
df$harvest_date[df$harvest_date=="17/Feb/22"] <- '2022/02/17'
df$harvest_date[df$harvest_date=="17/Mar/22"] <- '2022/03/17'

df$harvest_date[df$harvest_date=="19/Apr/22"] <- '2022/04/19'
df$harvest_date[df$harvest_date=="19/Mar/22"] <- '2022/03/19'

df$harvest_date[df$harvest_date=="2/10/2022"] <- '2022/02/10'
df$harvest_date[df$harvest_date=="2/12/2022"] <- '2022/02/12'

df$harvest_date[df$harvest_date=="20/Mar/22"] <- '2022/03/120'

df$harvest_date[df$harvest_date=="2/13/2022"] <- '2022/02/13'
df$harvest_date[df$harvest_date=="2/15/2022"] <- '2022/02/15'
df$harvest_date[df$harvest_date=="2/2/2022"] <- '2022/02/02'
df$harvest_date[df$harvest_date=="2/21/2022"] <- '2022/02/21'
df$harvest_date[df$harvest_date=="2/22/2022"] <- '2022/02/22'
df$harvest_date[df$harvest_date=="2/23/2022"] <- '2022/02/23'
       
df$harvest_date[df$harvest_date=="2/26/2022"] <- '2022/02/26'
df$harvest_date[df$harvest_date=="2/27/2022"] <- '2022/02/27'
df$harvest_date[df$harvest_date=="2/28/2022"] <- '2022/02/28'
df$harvest_date[df$harvest_date=="2/8/2022"] <- '2022/02/08'
df$harvest_date[df$harvest_date=="2/9/2022"] <- '2022/02/09'    

df$harvest_date[df$harvest_date=="21/Feb/22"] <- '2022/02/21'
df$harvest_date[df$harvest_date=="21/Mar/22"] <- '2022/03/21'
df$harvest_date[df$harvest_date=="22/Mar/22"] <- '2022/03/22'
df$harvest_date[df$harvest_date=="23/Feb/22"] <- '2022/02/23'
df$harvest_date[df$harvest_date=="23/Mar/22"] <- '2022/03/23'

df$harvest_date[df$harvest_date=="24/Feb/22"] <- '2022/01/24'
df$harvest_date[df$harvest_date=="24/Mar/22"] <- '2022/03/24'
df$harvest_date[df$harvest_date=="25/Feb/22"] <- '2022/02/25'
df$harvest_date[df$harvest_date=="25/Mar/22"] <- '2022/03/25'
df$harvest_date[df$harvest_date=="26/Feb/22"] <- '2022/02/26'
df$harvest_date[df$harvest_date=="26/Mar/22"] <- '2022/03/26'

df$harvest_date[df$harvest_date=="27/Mar/22"] <- '2022/03/27'
df$harvest_date[df$harvest_date=="28/Feb/22"] <- '2022/02/28'
df$harvest_date[df$harvest_date=="28/Mar/22"] <- '2022/03/28'
df$harvest_date[df$harvest_date=="29/Apr/22"] <- '2022/04/29'
df$harvest_date[df$harvest_date=="29/Mar/22"] <- '2022/03/29'
df$harvest_date[df$harvest_date=="3/10/22"] <- '2022/03/10'

df$harvest_date[df$harvest_date=="3/12/2022"] <- '2022/03/12'
df$harvest_date[df$harvest_date=="3/15/2022"] <- '2022/03/15'
df$harvest_date[df$harvest_date=="3/16/2022"] <- '2022/03/16'
df$harvest_date[df$harvest_date=="3/17/2022"] <- '2022/03/17'
df$harvest_date[df$harvest_date=="3/18/2022"] <- '2022/03/18'
df$harvest_date[df$harvest_date=="3/19/2022"] <- '2022/03/19'

df$harvest_date[df$harvest_date=="3/20/2022"] <- '2022/03/20'
df$harvest_date[df$harvest_date=="3/21/2022"] <- '2022/03/21'
df$harvest_date[df$harvest_date=="3/23/2022"] <- '2022/03/23'
df$harvest_date[df$harvest_date=="3/24/2022"] <- '2022/03/24'
df$harvest_date[df$harvest_date=="3/25/2022"] <- '2022/03/25'
df$harvest_date[df$harvest_date=="3/5/2022"] <- '2022/03/05'

df$harvest_date[df$harvest_date=="3/6/2022"] <- '2022/03/06'
df$harvest_date[df$harvest_date=="3/7/2022"] <- '2022/03/07'
df$harvest_date[df$harvest_date=="3/9/2022"] <- '2022/03/09'
df$harvest_date[df$harvest_date=="30/Apr/22"] <- '2022/04/30'
df$harvest_date[df$harvest_date=="30/Mar/22"] <- '2022/03/22'
df$harvest_date[df$harvest_date=="3/11/2022"] <- '2022/04/11'

df$harvest_date[df$harvest_date=="4/14/2022"] <- '2022/04/04'
df$harvest_date[df$harvest_date=="4/15/2022"] <- '2022/04/15'
df$harvest_date[df$harvest_date=="4/2/2022"] <- '2022/04/02'
df$harvest_date[df$harvest_date=="4/25/2022"] <- '2022/04/25'
df$harvest_date[df$harvest_date=="4/26/2022"] <- '2022/04/26'
df$harvest_date[df$harvest_date=="4/27/2022"] <- '2022/04/27'

df$harvest_date[df$harvest_date=="4/29/2022"] <- '2022/04/29'
df$harvest_date[df$harvest_date=="4/6/2022"] <- '2022/04/06'
df$harvest_date[df$harvest_date=="4/8/2022"] <- '2022/04/08'
df$harvest_date[df$harvest_date=="4/9/2022"] <- '2022/04/09'
df$harvest_date[df$harvest_date=="5/2/2022"] <- '2022/05/02'
df$harvest_date[df$harvest_date=="NA/NA/NA"] <- NA

df$harvest_date[df$harvest_date=="3/10/2022"] <- '2022/03/10'
df$harvest_date[df$harvest_date=="4/11/2022"] <- '2022/04/11'
df$harvest_date[df$harvest_date=="18/Feb/22"] <- '2022/02/18'
df$harvest_date[df$harvest_date=="18/Mar/22"] <- '2022/03/18'

df$harvest_date[df$harvest_date=="2015/2015/10"] <- '2015/10'
df$harvest_date[df$harvest_date=="2015/2015/11"] <- '2015/11'
df$harvest_date[df$harvest_date=="2015/2015/2"] <- '2015/02'
df$harvest_date[df$harvest_date=="2015/2015/3"] <- '2015/03'
df$harvest_date[df$harvest_date=="2015/2015/8"] <- '2015/08'
df$harvest_date[df$harvest_date=="2015/2015/9"] <- '2015/09'

# Need to decide what to do with data with year=NA
df$year <- as.numeric(format(as.Date(df$planting_date,format="%Y"),'%Y'))
i <- which(is.na(df$year))
dd <- df[i,]
unique(dd$dataset_id)

# Remove extra columns
df2 <- df[,c("dataset_id","country","site","longitude","latitude",
             "planting_date","harvest_date","on_farm","variety","nut_response_eval",
             "yield","N_fertilizer","N_splits","P_fertilizer","K_fertilizer",
             "caveat","year")]

saveRDS(df2,'result/fieldData/1_dataset.rds')
# write.csv(df2,'result/fieldData/1_dataset.csv')


