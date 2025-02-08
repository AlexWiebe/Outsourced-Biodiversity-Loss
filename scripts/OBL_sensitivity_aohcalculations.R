
### OBL_sensitivity_aohcalculations.R

### Alex Wiebe
### Princeton University

### DESCRIPTION:
### Just helper functions that are used by the driver script,
###  to calculate Area-of-Habitat (AOH) and related
### metrics.
### Specifically, the function getAOH is called, which converts a range polygon
### to an Area-of-Habitat (AOH) map, using another raster or list of rasters
### that represents a relevant data layer (like Hansen et al. forest cover)

### Description: Crops a list of landcover raster tiles to the range polgyon
### of a selected species; this list of cropped raster tiles represents the
### Area-of-Habitat (AOH) map for the distribution of that species. Returns a
### list of spatRaster tiles that comprise the AOH map for that species.
### Arguments:
### rangepolygon: a polgyon representing a species' range;
### landcover_tiles: a list of landcover tiles (spatRasters) - to make an AOH
### map, this is usually forest cover.
### Value: list of spatRaster tiles.
getAOH <- function(rangepolygon, landcover_tiles){
  
  blocks = length(landcover_tiles)
  full_range = vector(mode = "list", length = length(landcover_tiles))
  # initializes a list that will be populated with raster tiles
  
  if(blocks>0){
    for(k in 1:blocks){
      full_range[[k]] = maskRange(landcover_tiles[[k]], rangepolygon, k)
      full_range[[k]] = full_range[[k]] * cellSize(full_range[[k]], unit = "km")
      print(paste("Done with tile", k, "of", blocks, "."))
      gc() # clears RAM
    }
  } else{
    full_range = landcover_tiles
  }
  
  print(Sys.time())
  
  full_range
  
}

#//////////////////////////////////////

### Description: Takes in a list of lancover tiles, returns the subset that
### overlaps with the range polygon of a species. Done before cropping the
### landcover in the getAOH function to speed up that function by reducing
### the number of landcover tiles to loop through.
### Arguments:
### rangepolygon: a polgyon representing a species' range;
### landcover: a list of spatRaster tiles.
### Values: a list of spatRaster tiles.
subsetLandcover = function(rangepolygon, landcover){
  rm(landcover_inrange) # throws a warning message if it's the first time and
  # landcover_inrange does not yet exist - don't worry about this.
  
  for (j in 1:length(landcover)){
    
    intersection = terra::intersect(ext(rangepolygon), ext(landcover[[j]]))
    
    if (!is.null(intersection) && length(intersection) > 0){
      print(j)
      if(!exists('landcover_inrange')){ # landcover_inrange is list of land
        # cover tiles within the extent of the species' range
        landcover_inrange = list(landcover[[j]])
      } else{
        landcover_inrange = append(landcover_inrange, c(landcover[[j]]))
      }
      
    }
    
  }
  if(exists('landcover_inrange')){
    print(paste0("The number of landcover tiles overlapping the species' range is ",
                 length(landcover_inrange)))
  } else{
    landcover_inrange = vector(mode = "list", length = 0)
    print("No overlap with provided tiles.")
  }
  
  landcover_inrange
}

#//////////////////////////////////////

### Description: Takes in a landcover tile, crops it to the species' range.
### Arguments:
### landcover_tile: a spatRaster.
### range_vector: the spatVector range of a species.
### block: a number.
### Values: a spatRaster.
maskRange = function(landcover_tile, range_vector, block){
  print(paste("Masking tile", block, "."))
  
  range_edge = terra::crop(range_vector, ext(landcover_tile))
  
  # Check for non-empty tile with if statement
  if(max(ext(range_edge))[1] - min(ext(range_edge))[1] !=0){
    sf_edge = sf::st_as_sf(range_edge)
    if(length(sf_edge$geometry) < 2){
      r = raster(sf_edge, resolution = res(landcover_tile))
      # print(Sys.time())
      faster_edge = fasterize(sf_edge, r)
      # print(Sys.time())
      raster_range = terra::rast(faster_edge)
      crs(raster_range) = proj4string(faster_edge) # new piece of code, I guess
      # necessary from some terra update
      # print(Sys.time())
      background_layer = crop(landcover_tile, ext(raster_range))
      background_layer = terra::project(x = background_layer, y = raster_range)
    } else{
      a = rast(range_edge, resolution = c(0.005, 0.005))
      laster_edge = terra::rasterize(range_edge, a)
      raster_range = laster_edge
      background_layer = crop(landcover_tile, ext(raster_range))
      background_layer = terra::project(x = background_layer, y = raster_range)
    }
    
    # print(Sys.time())
    range_tile = sum(raster_range, background_layer)
    range_tile = range_tile - 1
    # tile = overlay(raster_range, background_layer, fun=sum)
    print(Sys.time())
    
  } else{
    r = rast(x = ext(landcover_tile), resolution = c(0.005))
    values(r) = 0
    range_tile = r
    print("Created empty tile.")
  }
  
  range_tile
  
}

###############################################################

sumDuplicateSpecies = function(sp.df){
  
  sp.sum <- sp.df %>%
    group_by(id_no) %>%
    dplyr::summarise(aoh_nasa2000 = sum(aoh_nasa2000, na.rm = T)) %>%
    as.data.frame()
  
  sp.sum
  
}

sumDuplicateDomesticSpecies = function(sp.df){
  
  sp.sum <- sp.df %>%
    group_by(id_no, binomial) %>%
    dplyr::summarise(aoh2000 = sum(aoh2000, na.rm = T),
                     rangeloss = sum(rangeloss, na.rm = T),
                     drivenloss = sum(drivenloss, na.rm = T),
                     present = max(present, na.rm = T)) %>%
    as.data.frame()
  
  sp.sum
  
}





