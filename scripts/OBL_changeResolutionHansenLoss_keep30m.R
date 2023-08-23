
### OBL_changeResolutionHansenLoss_keep30m.R

### Alex Wiebe
### Princeton University

### DESCRIPTION:
### A script to subset Hansen et al. forest loss data to the study period,
### keeping it at 30 m resolution for some analyses.

## This is like the other script with a similar name, but saves processed
## forest loss data at 30 m resolution to be used for other analyses instead
## of aggregating it and saving at 600 m resolution.

setwd("")

library(terra)

terraOptions(tempdir = paste0(getwd(), "/raster_temp"))
terraOptions(memfrac = 0.9)

changeResolution = function(index){
  og_raster = hansenloss[[index]]
  # change values between 1 and 15 to 1, higher than 15 to 0
  
  og_raster = terra::classify(og_raster, c(1, 16, 1), right = T, others = 0)
  # right = T to not include 16 in the range, and others = 0 sets anything
  # outside 1-15 to 0
  gc()
  for (h in 1:length(hansen)){
    if(ext(hansen[[h]]) == ext(hansenloss[[index]])){
      overlap_i = h
    }
  }
  og_raster = og_raster * hansen[[overlap_i]] / 100
  og_raster[is.na(og_raster)] <- 0 # change NA's to zeros in the raster
  gc()
  print("Zero-filled.")
  print(Sys.time())
  
  gc()
  print(Sys.time())
  
  og_raster  
}


### driver

path_hansen = paste0(getwd(), "/Hansen_lossyr")
dir_hansen = list.files (path = path_hansen, pattern = "*.tif", full.names = T)
hansenloss = lapply(dir_hansen, terra::rast)
path_hansen = paste0(getwd(), "/Hansen_30m_2000_final")
dir_hansen = list.files (path = path_hansen, pattern = "*.tif", full.names = T)
hansen = lapply(dir_hansen, terra::rast)
# hansen tiles have dims 40,000 x 40,000 pixels
# resolution = 0.00025 x 0.00025

setwd(paste0(getwd(), "/scratch/gpfs/rwiebe/30m_hansenloss"))
start = 1 # Modify start and end indices as needed.
end = length(hansenloss)
for(i in start:end){
  coarse_tile = changeResolution(i)
  print(i)
  writeRaster(coarse_tile, filename = paste0("hansenloss",i,".tif"), overwrite = T)
  gc()
}



