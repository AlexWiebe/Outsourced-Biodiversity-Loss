
### functions.R

### Alex Wiebe
### Princeton University
### 11/7/22

### DESCRIPTION:
### A script of all helper functions for the driver script,
### 'OBL_driver.R'
### Contains all of the functions necessary for the telecoupling analyses for
### a basic analysis on the patterns of telecoupled habitat loss on global
### biodiversity loss, using Hoang & Kanemoto 2021 raster data on telecoupled
### land use change.

### Functions that load data into the global environment:
### loadBirdRanges, loadCoarseHansen, loadCoarseHansenLoss,
### loadCoarseHoang, loadMammalRanges

wd = ""
setwd(wd)
cc.path = ""

{
  
  # Description: loadBirdRanges loads IUCN bird range polygons, drawing from
  # Chris Crawford's cleaned and validated (with a few exceptions) processed
  # dataset.
  # Value: none
  loadBirdRanges = function(){
    # Load range maps
    cc.path = cc.path
    load(paste0(cc.path, "bird_sf_validated.RData"))
    
    path_sp_ids = "forestobligate_bird_ids"
    dir_sp_ids = list.files(path = path_sp_ids, pattern = "*.csv",
                            full.names = T)
    sp_ids = lapply(dir_sp_ids, tryCSV)
    
    sp.ids = do.call(rbind, sp_ids)
    sp.ids = unique(sp.ids$forest.ids)
    
    bird_ranges = bird_sf_validated[bird_sf_validated$id_no %in% sp.ids,]
    bird_ranges$binomial = bird_ranges$sci_name # to keep same variable names as with mammals
    # Black-browed Babbler has unknown range, causes issues, so I remove it here:
    # bird_ranges = bird_ranges[bird_ranges$binomial != "Malacocincla perspicillata",]
    
    bird_ranges = vect(bird_ranges)
    
    taxon_ranges <<- bird_ranges
  }
  
  # Description: loadCoarseHansen loads Hansen et al. forest
  # cover data at 600m resolution, saving it to global environment
  # as hansen600m
  # Value: none
  loadCoarseHansen = function(){
    
    setwd(wd) 
    
    path_hansen = "600m_coarse_hansen_2000"
    dir_hansen = list.files (path = path_hansen, pattern = "*.tif", full.names = T)
    hansen600m = lapply(dir_hansen, terra::rast)
    
    hansen600m <<- hansen600m
    
  }
  
  # Description: loadCoarseHansenLoss loads Hansen et al. forest
  # loss data (as opposed to forest cover) at 600m resolution,
  # saving it to global environment as hansenloss
  # Value: none
  loadCoarseHansenLoss = function(){
    
    setwd(wd) 

    path_hansenloss = "600m_coarse_hansenloss"

    dir_hansenloss = list.files (path = path_hansenloss, pattern = "*.tif",
                                 full.names = T)
    hansenloss = lapply(dir_hansenloss, terra::rast)
    
    hansenloss <<- hansenloss
    
  }
  
  # Description: loadCoarseHoang loads raster data of telecoupled land use
  # change from Hoang & Kanemoto 2021, at 600 m resolution.
  # Value: none (saves data to global environment)
  loadCoarseHoang = function(country){
    
    setwd("wd") 
    
    # Only using USA map for now, at 600 m resolution
    path_hoang = paste0("600m_coarse_hoang/", country)
    dir_hoang = list.files (path = path_hoang, pattern = "*.tif", full.names = T)
    hoang = lapply(dir_hoang, terra::rast)
    
    hoang <<- hoang
  }
  
  # Description: loadMammalRanges loads IUCN mammal range polygons,
  # saving them to the global environment as mammal_ranges
  # Value: none
  loadMammalRanges = function(){
    
    # Load range maps
    mammal_ranges = vect("Mammal_rangemaps/IUCN_maps.shp")
    
    path_sp_ids = "forestobligate_mammal_ids"
    dir_sp_ids = list.files(path = path_sp_ids, pattern = "*.csv",
                            full.names = T)
    sp_ids = lapply(dir_sp_ids, tryCSV)
    # sp_ids = lapply(dir_sp_ids, read.csv, header = T)
    sp.ids = do.call(rbind, sp_ids)
    sp.ids = unique(sp.ids$forest.ids)
    
    mammal_ranges = mammal_ranges[mammal_ranges$id_no %in% sp.ids,]
    
    taxon_ranges <<- mammal_ranges
    
  }
  
  # Description: loadReptileRanges loads IUCN reptile range polygons,
  # saving them to the global environment as reptile_ranges
  # Value: none
  loadReptileRanges = function(){
    
    path_sp_ids = "forestobligate_reptile_ids"
    dir_sp_ids = list.files(path = path_sp_ids, pattern = "*.csv",
                            full.names = T)
    sp_ids = lapply(dir_sp_ids, tryCSV)
    sp.ids = do.call(rbind, sp_ids)
    sp.ids = unique(sp.ids$forest.ids)
    
    # Load range maps
    reptile_ranges = vect("Reptile_rangemaps/REPTILES.shp")
    reptile_ranges = reptile_ranges[reptile_ranges$id_no %in% sp.ids,]
    # ^ Code for if using IUCN range maps (fewer species, lower precision than GRAD maps).
    # Different taxonomic system, making IUCN more complete when classifying forest
    # obligate species using IUCN criteria. (Using GRAD would drop ~153 spp. from
    # name differences.)
    
    # iucn_reptile_ranges = vect("Reptile_rangemaps/REPTILES.shp")
    # iucn_reptile_ranges = iucn_reptile_ranges[iucn_reptile_ranges$id_no %in% sp.ids,]
    # binomials = iucn_reptile_ranges$binomial
    # reptile_ranges = vect("Gard_maps/Gard_1_7_ranges.shp")
    # # ^ Load GRAD reptile range maps, the most commonly used source of global
    # # reptile range maps.
    
    # reptile_ranges = reptile_ranges[reptile_ranges$binomial %in% binomials,]
    # rm(iucn_reptile_ranges)
    gc()
    
    taxon_ranges <<- reptile_ranges
    
  }
  
  
  
}

### Miscellaneous functions
{
  
  # Description: tryCSV is similar to the 'read.csv' function,
  # but puts that function in a tryCatch statement
  # Value: returns data.frame from read.csv
  tryCSV = function(filename){
    return(tryCatch(read.csv(filename, header = T),
                    error = function(e) NULL))
  }
}














