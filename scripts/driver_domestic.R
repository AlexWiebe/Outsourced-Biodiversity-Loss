
### driver_domestic.R

### Alex Wiebe
### Princeton University
### 2/2/23

### DESCRIPTION:
### A version of teh driver script, "Outsourced Biodiversity Loss" (OBL).
### Calculates domestic habitat loss within each driver country.

setwd("")

### Libraries and source scripts
{
  library(dplyr)
  library(fasterize)
  library(ggplot2)
  library(raster)
  library(rasterVis)
  library(remotes)
  library(rgdal)
  library(rgeos)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rnaturalearthhires)
  library(sf)
  library(terra)
  
  source('OBL_aohcalculations.R')
  source('OBL_functions.R')
}

### Global variables
### Mostly names (country names, taxon selected, etc.)
{
  TAXON = "Mammal" # Set the taxon.
  country.index = 24 # Set the country to analyze.
  country.names = c("ARG", "AUS", "BRA", "CAN", "CHN", "DEU", "FRA",
                    "GBR", "IDN", "IND", "IRN", "ITA", "JPN",
                    "KOR", "MEX", "NOR", "RUS", "SAU", "SGP",
                    "SWE", "TUR", "UKR", "USA", "ZAF")
  full.country.names = c("Argentina", "Australia", "Brazil", "Canada", "China", "Germany",
                         "France", "United Kingdom", "Indonesia", "India", "Iran",
                         "Italy", "Japan", "South Korea", "Mexico", "Norway",
                         "Russia", "Saudi Arabia", "Singapore", "Sweden",
                         "Turkey", "Ukraine", "United States of America",
                         "South Africa")
  
  # Select a country by index
  COUNTRY = country.names[country.index]
  FULL.COUNTRY = full.country.names[country.index]
  
  processed.file = paste0(getwd(), "/IDBL_outputs/species_data/",
                          TAXON, "_Domestic/", COUNTRY, ".csv")
}

### Load global environment
### Load raster datasets
{
  terraOptions(tempdir = paste0(getwd, "/raster_temp"))
  terraOptions(memfrac = 0.9)
  
  loadCoarseHoang(COUNTRY)
  loadCoarseHansen()
  loadCoarseHansenLoss()
  
  if(TAXON == "Bird")
    loadBirdRanges()
  if(TAXON == "Mammal")
    loadMammalRanges()
  if(TAXON == "Reptile")
    loadReptileRanges()
  
  # grab country outlines / ocean areas as sf object
  country.boundary <- ne_countries(returnclass = "sf", country = FULL.COUNTRY,
                                   scale = 10)
  c.mask = vect(country.boundary)
  if(COUNTRY == "USA")
    c.mask = crop(c.mask, ext(c(-180, -50, 18, 72))) # exclude territories besides DC
  if(COUNTRY == "FRA")
    c.mask = crop(c.mask, ext(c(-20, 30, 30, 60))) # restrict to European France
  if(COUNTRY == "NOR")
    c.mask = crop(c.mask, ext(c(-20, 50, 40, 90))) # exclude Norwegian owned island
  # territories in the Southern Ocean
}

### Loop through species, run the main analysis for each one
### See OBL_driver.R for more comments.
{
  First_sp = c(1)
  Last_sp = length(taxon_ranges)
  # Last_sp = c(1000)
  
  # if(First_sp != 1 | Last_sp != length(taxon_ranges)){
  #   if(file.exists(processed.file)){
  #     sp.data = read.csv(processed.file, header = T)
  #   }
  # }
  
  ## LOOP THROUGH SPECIES
  for (sp in First_sp:Last_sp){
    selected_sp = taxon_ranges[sp,]
    print(paste("Processing species", selected_sp$id_no))

    intersection = terra::intersect(ext(selected_sp), ext(c.mask))
    
    if (!is.null(intersection) & length(intersection) > 0){
      
      # if(selected_sp$sci_name == "Setophaga pinus"){ # this code is Bird-specific
      #   domestic_range = selected_sp
      # } else{
      
    new.mask = sf::st_as_sf(c.mask)
    if(!st_is_valid(new.mask)){
      new.mask = sf::st_make_valid(new.mask)
    }
    new.sp = sf::st_as_sf(selected_sp)
    if(!st_is_valid(new.sp)){
      new.sp = sf::st_make_valid(new.sp)
    }
    if(st_is_valid(new.sp)){
      domestic_range = sf::st_intersection(new.sp, new.mask)
    } else{
      domestic_range = terra::intersect(selected_sp, c.mask)
    }
      # }
    if(dim(domestic_range)[1] > 0){
      if(all(class(domestic_range) != "SpatVector"))
        domestic_range = terra::vect(domestic_range)
      
      hansen_subset = subsetLandcover(domestic_range, hansen600m)
      hoang_subset = subsetLandcover(domestic_range, hoang)
      hansenloss_subset = subsetLandcover(domestic_range, hansenloss)
      
      aoh.range = getAOH(rangepolygon = domestic_range,
                         landcover_tiles = hansen_subset)
      
      range.loss = getAOH(rangepolygon = domestic_range,
                          landcover_tiles = hansenloss_subset)
      
      driven.loss = getAOH(rangepolygon = domestic_range,
                           landcover_tiles = hoang_subset)
      
      # Calculate metrics
      # Sum area of habitat in the species' original range (sum of aoh.range
      # divided by 100 because the Hansen data reports percent forest cover 0-100.)
      original.area = sum(unlist(lapply(aoh.range, FUN = terra::global,
                                        fun = sum, na.rm = T)), na.rm = T)/100 # in km2
      loss.area = sum(unlist(lapply(range.loss, FUN = terra::global,
                                    fun = sum, na.rm = T)), na.rm = T) # in km2
      # Sum area of habitat lost (sum of range.loss)
      drivenloss.area = sum(unlist(lapply(driven.loss, FUN = terra::global,
                                          fun = sum, na.rm = T)), na.rm = T) # in km2
    } else{
      original.area = 0
      loss.area = 0
      drivenloss.area = 0
    }

    } else{
      original.area = 0
      loss.area = 0
      drivenloss.area = 0
    }
    # Store it with reference to the species ID
    new.data = data.frame(id_no = selected_sp$id_no,
                          binomial = selected_sp$binomial,
                          aoh2000 = original.area,
                          rangeloss = loss.area,
                          drivenloss = drivenloss.area)
    if(exists('sp.data')){
      sp.data = rbind(sp.data, new.data)
    } else{
      sp.data = new.data
    }
    
    # names(sp.data) = c("id_no", "binomial", "aoh2000", "USAloss")
    write.csv(sp.data, processed.file, row.names = F)
    
    # if(length(driven.loss) > 0){ # filters out extinct species with no forest
    #   #remaining in range to begin with
    #   for (f in 1:length(driven.loss)){
    #     sp.filename = paste0("IDBL_outputs/species_maps/", TAXON, "/", 
    #                          COUNTRY, "_", new.data$id_no,
    #                          "_tile_", f, ".tif")
    #     writeRaster(driven.loss[[f]], sp.filename, overwrite = T)
    #   } 
    # }
    
  }
  
  sp.data = sumDuplicateSpecies(sp.data) # have to be careful with multiple runs
  # through, if you duplicate species data with multiple runs that overlap in
  # species index this will sum those rows and make the species' range and other
  # measurements seem larger than they are.
  
  write.csv(sp.data, processed.file, row.names = F)
  
  print(Sys.time())
}



































