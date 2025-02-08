
### Alex Wiebe
### Princeton University

wd = ""
setwd(wd)

library(terra)

terraOptions(tempdir = paste0(wd, "/raster_temp"))
terraOptions(memfrac = 0.9)


# # 
# # # Main
# # matching_files = list.files(pattern = "2000\\.tif$", full.names = T)
# # hansen_ref = rast("Hansen_30m_2000_final/hansen1.tif") # reference for CRS
# # 
# # start_index = 1
# # end_index = length(matching_files)
# # 
# # for(i in start_index:end_index){
# #   tryCatch({
# #     sexton = terra::rast(matching_files[i])
# #     gc()
# #     coarse_tile = changeResolution()
# #     gc()
# #     print(i)
# #     writeRaster(coarse_tile,
# #                 filename = paste0("sexton600m/",i,".tif"),
# #                 overwrite = T)
# #     gc()
# #   }, error = function(e){
# #     print("Error occurred: trying to read empty .tif file.")
# #   })
# # 
# # }
# 
# 
# ### driver
# 
library(dplyr)
library(fasterize)
library(raster)
library(remotes)
library(rgdal)
library(rgeos)
library(sf)
library(terra)

source('OBL_sensitivity_aohcalculations.R')
source('OBL_functions.R')

terraOptions(tempdir = paste0(wd, "/raster_temp"))
terraOptions(memfrac = 0.9)

directory_path = "" # path to Sexton et al. data
nasa_names = list.files(directory_path, full.names = T)
nasa = lapply(nasa_names, terra::rast)

args = commandArgs(T) # Access the slurm array variable
run_block = as.numeric(args[1])

TAXON = "Bird" # Change to alternate between taxa

if(TAXON == "Bird")
  loadBirdRanges()
if(TAXON == "Mammal")
  loadMammalRanges()
if(TAXON == "Reptile")
  loadReptileRanges()


processed.file = ""
First_sp = c(run_block*300 - 299)
# # Last_sp = length(taxon_ranges)
Last_sp = c(run_block*300)
# First_sp = c(3000 + run_block*150 - 149)
# Last_sp = c(3000 + run_block*150)

print(Sys.time())
for (sp in First_sp:Last_sp){
  selected_sp = taxon_ranges[sp,]
  print(paste("Processing species", selected_sp$id_no))

  nasa_subset = subsetLandcover(selected_sp, nasa)

  aoh.range = getAOH(rangepolygon = selected_sp,
                     landcover_tiles = nasa_subset) # function includes correction
  # for cell sizes with the cellSize function


  # Calculate metrics
  # Sum area of habitat in the species' original range (sum of aoh.range
  # divided by 100 because the Hansen data reports percent forest cover 0-100.)
  nasa.area = sum(unlist(lapply(aoh.range, FUN = terra::global,
                                    fun = sum, na.rm = T)), na.rm = T) # in km2

  # Store it with reference to the species ID
  new.data = data.frame(id_no = selected_sp$id_no,
                        aoh_nasa2000 = nasa.area)
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
  #     sp.filename = paste0("/scratch/gpfs/rwiebe/IDBL_outputs/species_maps/", TAXON, "/",
  #                          COUNTRY, "_", new.data$id_no,
  #                          "_tile_", f, ".tif")
  #     writeRaster(driven.loss[[f]], sp.filename, overwrite = T)
  #   }
  # }

}
print(Sys.time())


### Secondary Analysis:
nasa_files = list.files(pattern = "^nasa_.*\\.csv$")
full.nasa = data.frame(id_no = NA, aoh_nasa2000 = NA)
for(file in nasa_files){
  nasa = read.csv(file, header = T)
  full.nasa = rbind(full.nasa, nasa)
}

full.nasa = sumDuplicateSpecies(full.nasa)

bird.file = ""
obl.birds = read.csv(bird.file, header = T)
mammal.file = ""
obl.mammals = read.csv(mammal.file, header = T)
reptile.file = ""
obl.reptiles = read.csv(reptile.file, header = T)
full.obl = rbind(obl.birds, obl.mammals, obl.reptiles)

merged_df = full.nasa %>%
  left_join(full.obl, by = "id_no") %>%
  dplyr::select(id_no, binomial, aoh2000, aoh_nasa2000, rangeloss) %>%
  na.omit() #7681 species

# Remove species without data coverage (like species on small islands)
# Removing species without data from either dataset, because it would be misleading
# to compare the results for a species missing coverage from either dataset, even
# if it has coverage from another dataset
filtered_df = merged_df %>%
  filter(aoh2000 != 0, aoh_nasa2000 != 0) #7497 species, removing 184 species

filtered_df$diff = filtered_df$aoh_nasa2000 - filtered_df$aoh2000
filtered_df$err = filtered_df$diff / filtered_df$aoh2000
filtered_df$prop_rangeloss = filtered_df$rangeloss / filtered_df$aoh2000

plot(filtered_df$err, filtered_df$prop_rangeloss)
summary(lm(prop_rangeloss ~ err, data = filtered_df)) #beta = -0.56 p = 0.24
nooutliers_df = filtered_df[filtered_df$err < 1,]
plot(nooutliers_df$err, nooutliers_df$prop_rangeloss)
summary(lm(prop_rangeloss ~ err, data = nooutliers_df)) #beta = -0.11 p = 0.04*

large.errors = filtered_df[abs(filtered_df$err) > 1,]












