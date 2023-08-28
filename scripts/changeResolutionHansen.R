
### changeResolutionHansen.R

### Alex Wiebe
### Princeton University

### DESCRIPTION:
### A script to aggregate Hansen et al. forest cover data from 2000
### from 30 m to 600 m for computational efficiency.


setwd("")

library(fasterize)
library(ggplot2)
library(raster)
library(rasterVis)
library(remotes)
library(rgdal)
library(terra)

terraOptions(tempdir = paste0(getwd(), "/raster_temp"))
terraOptions(memfrac = 0.9)

changeResolution = function(index){
  og_raster = hansen[[index]]
  print(Sys.time())
  og_raster[is.na(og_raster)] <- 0 # change NA's to zeros in the raster
  print("Zero-filled.")
  print(Sys.time())
  new_raster = terra::aggregate(og_raster, fact = 20, fun = "mean") # fact = 20
  # changes resolution from 30m to 600m
  print(Sys.time())
  
  new_raster  
}


### driver

path_hansen = paste0(getwd(), "/Hansen_30m_2000_final")
dir_hansen = list.files (path = path_hansen, pattern = "*.tif", full.names = T)
hansen = lapply(dir_hansen, terra::rast)
# hansen tiles have dims 40,000 x 40,000 pixels
# resolution = 0.00025 x 0.00025

setwd(paste0(getwd(), "/600m_coarse_hansen_2000"))
start = 1 # modify start and end indices as needed.
end = length(hansen)
for(i in start:end){
  coarse_tile = changeResolution(i)
  gc()
  print(i)
  writeRaster(coarse_tile, filename = paste0("hansen",i,".tif"), overwrite = T)
  gc()
}



