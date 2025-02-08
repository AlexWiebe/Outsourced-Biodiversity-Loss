
### OBLdriver.R

### Alex Wiebe
### Princeton University
### 11/7/22

### DESCRIPTION:
### A driver script, "Outsourced Biodiversity Loss" (OBL).

wd = "" # Working directory
setwd(wd) # Set working directory.

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
  TAXON = "Bird"
  country.index = 1
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
  
  processed.file = paste0(wd, "/IDBL_outputs/species_data/",
                          TAXON, "/", COUNTRY, ".csv")
}

### Load global environment
### Load raster datasets
{
  terraOptions(tempdir = paste0(wd, "/raster_temp"))
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
  
}

### Loop through species, run the main analysis for each one
### This is time-consuming, and can be split up into batches to increase
### efficiency. This is done manually below, by setting First_sp and
### Last_sp indices, but can be parallelized, or run as a whole, depending
### on computational capacity.
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
  
  hansen_subset = subsetLandcover(selected_sp, hansen600m) # subsets land tiles
  # to just those that overlap in coordinates with the species' range.
  hoang_subset = subsetLandcover(selected_sp, hoang)
  hansenloss_subset = subsetLandcover(selected_sp, hansenloss)
  
  aoh.range = getAOH(rangepolygon = selected_sp,
                     landcover_tiles = hansen_subset)
  
  range.loss = getAOH(rangepolygon = selected_sp,
                      landcover_tiles = hansenloss_subset)
  
  driven.loss = getAOH(rangepolygon = selected_sp,
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



































