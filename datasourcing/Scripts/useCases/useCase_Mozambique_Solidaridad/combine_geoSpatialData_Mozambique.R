dat_dir <- '~/agwise-datasourcing/dataops/datasourcing/Data/useCase_Mozambique_Solidaridad/Maize/result/geo_4cropModel'

variables <- c('_windSpeed', '_solarRadiation', '_relativeHumidity', '_temperatureMin', '_temperatureMax', '_Rainfall', '_SoilDEM')

for (v in 1:length(variables)){
vars <- list.files(dat_dir, pattern = variables[v])
vard_a <- NULL
for (f in 1:length(vars)){
  #iotools::fdrbind(readRDS(paste0(dat_dir, '/', vars[f])))
  #vard <- iotools::fdrbind(readRDS(paste0(dat_dir, '/', vars[f])))
  vard_a <- rbind(vard_a,vard)
}
saveRDS(vard_a, paste0(dat_dir, '/', gsub("_","",variables[v]), "_PointData_AOI.RDS"))
print(paste0(gsub("_","",variables[v]), "_PointData_AOI has been created..."))
}
