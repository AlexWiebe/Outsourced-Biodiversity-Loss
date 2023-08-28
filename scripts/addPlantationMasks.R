
### addPlantationMasks.R

### Alex Wiebe
### Princeton University

### DESCRIPTION:
### A script to use plantation masks from Hoang and Kanemoto 2021 to remove
### oil palm and rubber tree plantations from Hansen et al. forest cover data.


setwd("")

library(terra)

terraOptions(tempdir = paste0(getwd(), "/raster_temp"))
terraOptions(memfrac = 0.9)

nigeria = terra::rast(paste0(getwd(), "/Nigeria_plantationmask.tif"))
se_asia = terra::rast(paste0(getwd(), "/SoutheastAsia_plantationmask.tif"))

path_hansen = paste0(getwd(), "/Hansen_30m_forestcover_2000")
dir_hansen = list.files (path = path_hansen, pattern = "*.tif", full.names = T)


setwd(paste0(getwd(), "/Hansen_30m_2000_final"))
# Can change start and end indices to block runs manually
start = 1
end = length(dir_hansen)

for(i in start:end){
  hansen = terra::rast(dir_hansen[i])
  
  intersection_n = terra::intersect(ext(hansen), ext(nigeria))
  intersection_s = terra::intersect(ext(hansen), ext(se_asia))
  intersection_n
  intersection_s
  gc()
  
  if (!is.null(intersection_n) && length(intersection_n) > 0){
    plant = crop(nigeria, ext(hansen))
    gc()
    plant = extend(plant, ext(hansen))
    plant = 100 * plant # rescale to scale of hansen data
    gc()
    hansen = sum(hansen, -plant, na.rm = T)
    gc()
    hansen = clamp(hansen, lower = 0, upper = 100, values = F)
    rm(plant)
    gc()
  }
  
  if (!is.null(intersection_s) && length(intersection_s) > 0){
    plant = crop(se_asia, ext(hansen))
    gc()
    plant = extend(plant, ext(hansen))
    plant = 100 * plant # rescale to scale of hansen data
    gc()
    hansen = sum(hansen, -plant, na.rm = T)
    gc()
    hansen = clamp(hansen, lower = 0, upper = 100, values = F)
    rm(plant)
    gc()
  }
  
  gc()
  writeRaster(hansen, filename = paste0("hansen",i,".tif"), overwrite = T)
  gc()
  print(i)
  
}
















