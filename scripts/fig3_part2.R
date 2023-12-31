
### fig3_part2.R

### Alex Wiebe
### Princeton University
### 2/6/23

### DESCRIPTION:
### Plotting of hotspot maps (Fig. 3 and otherwise) generated by OBL_fig3.R
### script.

### Libraries
setwd("")
library(ggplot2)
library(maps)
library(sf)
library(terra)
library(tidyterra)
source('OBL_aohcalculations.R')
source('OBL_functions.R')

terraOptions(tempdir = paste0(getwd(), "/raster_temp"))
terraOptions(memfrac = 0.9)

### Country-by-country plotting with country selected using this variable,
### change as needed.
country = "ZAF"

###----Put the pieces together (if running in batches):
  dir_hotspots = list.files(path = paste0(getwd(), "/IDBL_outputs/trial_biodiversitymaps/",
                                          country, ""),
                             pattern = "*", full.names = T)
  h_files = lapply(dir_hotspots, terra::rast)
  hotspots = h_files[[1]]
for(h in 2:length(h_files)){
  hotspots = sum(hotspots, h_files[[h]], na.rm = T)
  gc()
  print(h)
}

  filename = paste0(getwd(), "/IDBL_outputs/", country, "_hotspots.tif")
  writeRaster(hotspots, filename, overwrite = T)

### Plotting
world = map_data("world")
world = world[world$lat > -57,] # remove Antarctica
hotspots = crop(hotspots, ext(c(-180, 180, -57, 90))) # remove Antarctica

plt = ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "black", size = 0.1
  ) +
  geom_spatraster(data = hotspots) +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    color = "black", fill = "transparent", size = 0.05
  ) +
  scale_fill_gradientn(colors = c('#000040', '#02d1fa',  '#b81a1f', '#fa5902',
           '#fad102', '#f7ff03', '#ffffff'),
           name = "Cumulative Range Loss \n of Occurring Species",
na.value = "transparent") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.title=element_text(size=7),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.text=element_text(size=5))

plt

# To save:
  png(paste0("fig3_", country, ".png"), units = "px", height=2000,
      width=5000, res=600)
  plt
  dev.off()




