
### fig3.R

### Alex Wiebe
### Princeton University
### 2/6/23

### DESCRIPTION:
### Creates Figure 3 (hotspot maps for USA, Japan, China, and Germany). Also
### used to create hotspot maps for other countries. Hotspot maps show the 
### distribution of affected species by a country, within which the exact
### distribution of deforestation varies year to year and will continue to be
### different in the future.

### Libraries
setwd("") # Set working directory
library(ggplot2)
library(sf)
library(terra)
library(tidyterra)
source('OBL_aohcalculations.R')
source('OBL_functions.R')

terraOptions(tempdir = paste0(getwd(), "/scratch/gpfs/rwiebe/raster_temp"))
terraOptions(memfrac = 0.9)

# Splits manually by country, using three letter country code below (e.g., ARG, AUS, ZAF)
country = "ZAF"

### Helper function (addSp):
### sp.i  is an index of the species' id_no
### tax.i is an index of taxonomic group (bird = 1, mammal = 2, reptile = 3)
### layer is an index of layer to deal with species that have more than
### one range file
addSp = function(sp.i, tax.i, layer){
  
  if(tax.i == 1){
    tax_ranges = bird_ranges
    sp.data = bird.data
  }
  if(tax.i == 2){
    tax_ranges = mammal_ranges
    sp.data = mammal.data
  }
  if(tax.i == 3){
    tax_ranges = reptile_ranges
    sp.data = reptile.data
  }
  
  sp.map = tax_ranges[tax_ranges$id_no == sp.data$id_no[sp.i],]
  sp.map = sp.map[layer,]
  
  sp.val = sp.data$drivenloss[sp.i] / sp.data$aoh2000[sp.i]

  r = rast(ext(c(-180, 180, -90, 90)), resolution = c(0.01, 0.01))
  # if(dim(r)[1]>0){
  print("Got here.")
    sp.map.rast = terra::rasterize(sp.map, r)
    sp.map.rast[sp.map.rast == 1] <- sp.val
    print("Finished long stuff.")
    gc()
    print(free_RAM())
    if(exists('biodiversity')){
      biodiversity = c(biodiversity, sp.map.rast)
    } else{
      biodiversity = sp.map.rast
    }
  # }
  
  biodiversity
}

### Driver

# Load data
loadBirdRanges()
bird_ranges <- taxon_ranges
loadMammalRanges()
mammal_ranges <- taxon_ranges
loadReptileRanges()
reptile_ranges <- taxon_ranges
rm(taxon_ranges)
gc()

  
  ### Load data
  
  bird.data = read.csv(paste0(getwd(), "/IDBL_outputs/species_data/Bird/",
                              country, ".csv"), header = T)
  mammal.data = read.csv(paste0(getwd(), "/IDBL_outputs/species_data/Mammal/",
                                country, ".csv"), header = T)
  reptile.data = read.csv(paste0(getwd(), "/IDBL_outputs/species_data/Reptile/",
                                 country, ".csv"), header = T)
  
  ### Make hotspot map

  ### Manually splits into chunks for running using a start_index and end_index
  ### This can be changed as needed based on computational capacity.
  start_index = 601
  end_index = 800
  
  ### Runs through one taxonomic group at a time, others commented out.
  # Loop through birds
  # for (b in 1:length(bird.data$id_no)){
  for (b in start_index:end_index){
    target_range = bird_ranges[bird_ranges$id_no == bird.data$id_no[b],]
    if(ext(target_range)[2]-ext(target_range)[1] > 0.006){
      for(i in 1:length(target_range)){
        biodiversity = addSp(sp.i = b, tax.i = 1, layer = i)
        gc()
        if(dim(biodiversity)[3] > 1){
          biodiversity = sum(biodiversity, na.rm = T)
        }
      }
    }
    gc()
    print(free_RAM())
    print(paste0("Done with bird species number: ", b))
  }
  
  # # Loop through mammals
  # for (b in start_index:end_index){
  #   target_range = mammal_ranges[mammal_ranges$id_no == mammal.data$id_no[b],]
  #   if(ext(target_range)[2]-ext(target_range)[1] > 0.006){
  #     for(i in 1:length(target_range)){
  #       biodiversity = addSp(sp.i = b, tax.i = 2, layer = i)
  #       gc()
  #       if(dim(biodiversity)[3] > 1){
  #         biodiversity = sum(biodiversity, na.rm = T)
  #       }
  #     }
  #   }
  #   gc()
  #   print(free_RAM())
  #   print(paste0("Done with mammal species number: ", b))
  # }

  # # Loop through reptiles
  # for (b in start_index:end_index){
  #   target_range = reptile_ranges[reptile_ranges$id_no == reptile.data$id_no[b],]
  #   if(ext(target_range)[2]-ext(target_range)[1] > 0.006){
  #     for(i in 1:length(target_range)){
  #       biodiversity = addSp(sp.i = b, tax.i = 3, layer = i)
  #       gc()
  #       if(dim(biodiversity)[3] > 1){
  #         biodiversity = sum(biodiversity, na.rm = T)
  #       }
  #     }
  #   }
  #   gc()
  #   print(free_RAM())
  #   print(paste0("Done with reptile species number: ", b))
  # }
  
filename = paste0(getwd(), "/IDBL_outputs/trial_biodiversitymaps/", 
                  country, "/biodiversity_bird_",
                    start_index,".tif")
print(filename)
writeRaster(biodiversity, filename, overwrite = T)
print("Saved raster file.")

  






