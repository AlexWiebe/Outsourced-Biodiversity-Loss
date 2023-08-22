
### OBL_makeHansenBinary.R

### Alex Wiebe
### Princeton University

### DESCRIPTION:
### A script to turn Hansen et al. forest cover data from a continuous variable
### (percent tree cover) to binary (forest or non-forest) using their definition
### of a forest, >50% tree cover.

setwd("")

library(terra)

terraOptions(tempdir = paste0(getwd(), "/raster_temp"))
terraOptions(memfrac = 0.9)

### driver

path_hansen = paste0(getwd(), "/Hansen_30m_2000")
dir_hansen = list.files (path = path_hansen, pattern = "*.tif", full.names = T)

setwd(paste0(getwd(), "/Hansen_30m_forestcover_2000"))
start = 1 # Can change these indices to manually block for runs.
end = length(dir_hansen)
for(i in start:end){
  hansen = terra::rast(dir_hansen[i])
  hansen[hansen > 50] <- 100
  gc()
  hansen[hansen < 100] <- 0
  gc()

  writeRaster(hansen, filename = paste0("hansen",i,".tif"), overwrite = T)
  gc()
  print(i)
}






























