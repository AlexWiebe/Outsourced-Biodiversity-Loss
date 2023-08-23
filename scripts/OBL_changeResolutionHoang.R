
### OBL_changeResolutionHoang.R

### Alex Wiebe
### Princeton University

### DESCRIPTION:
### A script to change the resolution of the land attribution rasters from
### Hoang and Kanemoto 2021 for computational efficiency.

setwd("")

library(terra)

terraOptions(tempdir = paste0(getwd(), "/raster_temp"))
terraOptions(memfrac = 0.9)

changeResolution = function(index){
  og_raster = hoang[[index]]
  
  gc()
  for (h in 1:length(hansen)){
    if(ext(hansen[[h]]) == ext(og_raster)){
      overlap_i = h
    }
  }
  og_raster = og_raster * hansen[[overlap_i]]
  og_raster[is.na(og_raster)] <- 0 # change NA's to zeros in the raster
  gc()
  print("Zero-filled.")
  print(Sys.time())
  # aggregate from 30 m resolution to 600 m resolution
  og_raster = terra::aggregate(og_raster, fact = 20, fun = "mean")
  gc()
  print(Sys.time())
  
  og_raster  
}


### driver

COUNTRY = "IRN" # Select country code
path_hoang = paste0(getwd(), "/Hoang_30m/", COUNTRY)
dir_hoang = list.files (path = path_hoang, pattern = "*.tif", full.names = T)
hoang = lapply(dir_hoang, terra::rast)
path_hansen = paste0(getwd(), "/30m_hansenloss")
dir_hansen = list.files (path = path_hansen, pattern = "*.tif", full.names = T)
hansen = lapply(dir_hansen, terra::rast)
# hansen tiles have dims 40,000 x 40,000 pixels
# resolution = 0.00025 x 0.00025

setwd(paste0(getwd(), "/600m_coarse_hoang/", COUNTRY))
start = 1 # modify start and end indices as needed.
end = length(hoang)
for(i in start:end){
  coarse_tile = changeResolution(i)
  print(i)
  writeRaster(coarse_tile, filename = paste0("hoang_", COUNTRY, "_", i, ".tif"),
              overwrite = T)
  gc()
}



