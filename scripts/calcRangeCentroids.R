
### calcRangeCentroids.R

### Alex Wiebe
### Princeton University
### 2/9/23

### DESCRIPTION:
### A script to calculate the range centroids of species and get the IUCN
### statuses of those species.

### Libraries
setwd("")

library(dplyr)
library(ggplot2)
library(rredlist)
library(geosphere)
library(rgeos)
library(rworldmap)
library(terra)
library(CoordinateCleaner)
my.iucn.key = "" # Insert your IUCN key here
source('OBL_aohcalculations.R')
source('OBL_functions.R')

terraOptions(tempdir = paste0(getwd(), "/raster_temp"))
terraOptions(memfrac = 0.9)

## Helper functions

# avgCentroid: Calculates weighted average centroid of multiple range polygons,
# correcting for ranges that have individual centroids that are on
# opposite sides of the 180th longitudinal meridian.
avgCentroid = function(ranges){
  centroids = data.frame(area = rep(NA, length(ranges)),
                         lat = rep(NA, length(ranges)),
                         lon = rep(NA, length(ranges)))
  for (i in 1:length(ranges)){
    centroids$area[i] = expanse(ranges[i,])
    centroids$lat[i] = ext(terra::centroids(ranges[i,]))[3]
    centroids$lon[i] = ext(terra::centroids(ranges[i,]))[1]
    if(centroids$lon[i] > 180)
      centroids$lon[i] = centroids$lon[i] - 360
  }
  for(j in 1:length(centroids$area)){
    if(j == 1){
      avg.centroid = data.frame(area = centroids$area[j],
                                lat = centroids$lat[j],
                                lon = centroids$lon[j])
    } else{
      if(abs(avg.centroid$lon - centroids$lon[j]) > 180){
        lon.diff = 360 - abs(avg.centroid$lon - centroids$lon[j])
        correction = lon.diff * centroids$area[j] /
          (avg.centroid$area + centroids$area[j])
        if(centroids$lon[j] < avg.centroid$lon){
          new.lon = avg.centroid$lon + correction
        } else{
          new.lon = avg.centroid$lon - correction
        }
        if(new.lon > 180)
          new.lon = new.lon - 360
        if(new.lon < -180)
          new.lon = new.lon + 360
        avg.centroid$lon = new.lon
      } else{
        avg.centroid$lon = (avg.centroid$area * avg.centroid$lon +
                              centroids$area[j] * centroids$lon[j])/
          (avg.centroid$area + centroids$area[j])
      }
      avg.centroid$lat = (avg.centroid$area * avg.centroid$lat +
                            centroids$area[j] * centroids$lat[j])/
        (avg.centroid$area + centroids$area[j])
      avg.centroid$area = avg.centroid$area + centroids$area[j]
    }
  }
  gc()
  
  avg.centroid
}


getSpeciesInfo = function(taxon, filename){
  if(taxon == 1){
    loadBirdRanges()
  }
  if(taxon == 2){
    loadMammalRanges()
  }
  if(taxon == 3){
    loadReptileRanges()
  }
  
  traits = calcRangeCentroid(taxon_ranges)
  # traits = getIUCNStatus(traits)
  
  write.csv(traits, filename, row.names = F)
}

calcRangeCentroid = function(rangeList){
  unique.sp = data.frame(id_no = unique(rangeList$id_no))
  unique.sp$centroid_lat = rep(NA, length(unique.sp$id_no))
  unique.sp$centroid_lon = rep(NA, length(unique.sp$id_no))
  
  for (id in 1:length(unique.sp$id_no)){
    if(length(rangeList[rangeList$id_no == unique.sp$id_no[id],]) == 1){
      centroid = centroids(rangeList[rangeList$id_no == unique.sp$id_no[id],])
      unique.sp$centroid_lat[id] = ext(centroid)[3]
      unique.sp$centroid_lon[id] = ext(centroid)[1]
      if(unique.sp$centroid_lon[id] > 180)
        unique.sp$centroid_lon[id] = unique.sp$centroid_lon[id] - 360
    } else{
      range_polygons = rangeList[rangeList$id_no == unique.sp$id_no[id],]
      avg.centroid = avgCentroid(range_polygons)
      unique.sp$centroid_lat[id] = avg.centroid$lat
      unique.sp$centroid_lon[id] = avg.centroid$lon
    }
    print(id)
    gc()
    print(free_RAM())
  }
  
  unique.sp
  
}

getIUCNStatus = function(sp.data){
  dd = rl_sp_category("DD", key = my.iucn.key)
  lc = rl_sp_category("LC", key = my.iucn.key)
  nt = rl_sp_category("NT", key = my.iucn.key)
  vu = rl_sp_category("VU", key = my.iucn.key)
  en = rl_sp_category("EN", key = my.iucn.key)
  cr = rl_sp_category("CR", key = my.iucn.key)
  ew = rl_sp_category("EW", key = my.iucn.key)
  ex = rl_sp_category("EX", key = my.iucn.key)
  
  sp.data$IUCN = rep(NA, length(sp.data[,1]))
  for (i in 1:length(sp.data$IUCN)){
    if (sp.data$id_no[i] %in% dd$result$taxonid){
      sp.data$IUCN[i] = "dd"
    } 
    if (sp.data$id_no[i] %in% lc$result$taxonid){
      sp.data$IUCN[i] = "lc"
    } 
    if (sp.data$id_no[i] %in% nt$result$taxonid){
      sp.data$IUCN[i] = "nt"
    } 
    if (sp.data$id_no[i] %in% vu$result$taxonid){
      sp.data$IUCN[i] = "vu"
    } 
    if (sp.data$id_no[i] %in% en$result$taxonid){
      sp.data$IUCN[i] = "en"
    } 
    if (sp.data$id_no[i] %in% cr$result$taxonid){
      sp.data$IUCN[i] = "cr"
    } 
    if (sp.data$id_no[i] %in% ew$result$taxonid){
      sp.data$IUCN[i] = "ew"
    } 
    if (sp.data$id_no[i] %in% ex$result$taxonid){
      sp.data$IUCN[i] = "ex" 
    }
  }
  
  sp.data = sp.data[!is.na(sp.data$IUCN),]
  
  sp.data
  
}

### Driver
filename = "bird_covariates_final.csv"
traits = getSpeciesInfo(taxon = 1, filename) # 1: birds, 2: mammals, 3: reptiles
