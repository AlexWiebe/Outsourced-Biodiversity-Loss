
### OBL_fig4.R

### Alex Wiebe
### Princeton University
### 2/9/23

### DESCRIPTION:
### Creates Figure 4 (distance plot), including the main plot and Madagascar inset,
### and calculates relevant statistics for the distance relationship.

### Libraries
setwd("")

library(dplyr)
library(ggplot2)
library(rredlist)
library(geosphere)
library(rgeos)
library(rworldmap)
library(terra)
library(tidyterra)
library(CoordinateCleaner)
my.iucn.key = "" # Enter your IUCN data access key here.
source('OBL_aohcalculations.R')
source('OBL_functions.R')

terraOptions(tempdir = paste0(getwd(), "/raster_temp"))
terraOptions(memfrac = 0.9)


### Driver

b.traits = read.csv("bird_covariates_final.csv", header = T)
m.traits = read.csv("mammal_covariates_final.csv", header = T)
r.traits = read.csv("reptile_covariates_final.csv", header = T)

full.country.names = c("Argentina", "Australia", "Brazil", "Canada", "China", "Germany",
                       "France", "United Kingdom", "Indonesia", "India", "Iran",
                       "Italy", "Japan", "South Korea", "Mexico", "Norway",
                       "Russia", "Saudi Arabia", "Singapore", "Sweden",
                       "Turkey", "Ukraine", "United States of America",
                       "South Africa")

dir_bird = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Bird"),
                       pattern = "*", full.names = T)
bird = lapply(dir_bird, read.csv, header = T)
dir_mammal = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Mammal"),
                       pattern = "*", full.names = T)
mammal = lapply(dir_mammal, read.csv, header = T)
dir_reptile = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Reptile"),
                       pattern = "*", full.names = T)
reptile = lapply(dir_reptile, read.csv, header = T)

# Remove extinct species
ex = read.csv("IUCN_extinctspecies.csv", header = T)
for(i in 1:length(bird)){
  bird[[i]] = bird[[i]][!(bird[[i]]$id_no %in% ex$taxonid),]
  mammal[[i]] = mammal[[i]][!(mammal[[i]]$id_no %in% ex$taxonid),]
  reptile[[i]] = reptile[[i]][!(reptile[[i]]$id_no %in% ex$taxonid),]
}

### Code to calculate country centroids the first time
# wmap = getMap(resolution = "high")
# centroids = gCentroid(wmap, byid = T)
# centroids = as.data.frame(centroids)
# c.df = data.frame(country = full.country.names,
#                   driverlat = rep(NA, length(full.country.names)),
#                   driverlon = rep(NA, length(full.country.names)))
# for(k in 1:length(full.country.names)){
#   c.df$driverlat[k] = centroids[rownames(centroids) == full.country.names[k],]$y
#   c.df$driverlon[k] = centroids[rownames(centroids) == full.country.names[k],]$x
# }
# write.csv(c.df, file = "/scratch/gpfs/rwiebe/country_centroids.csv")

## Load country centroids from file after creating
country.centroids = read.csv("country_centroids.csv", header = T)

for (s in 1:length(bird)){
  bird[[s]]$driver.country = full.country.names[s]
  bird[[s]] = merge(bird[[s]], b.traits, by = "id_no")
  bird[[s]]$driverlat = country.centroids$driverlat[s]
  bird[[s]]$driverlon = country.centroids$driverlon[s]
}
for (s in 1:length(mammal)){
  mammal[[s]]$driver.country = full.country.names[s]
  mammal[[s]] = merge(mammal[[s]], m.traits, by = "id_no")
  mammal[[s]]$driverlat = country.centroids$driverlat[s]
  mammal[[s]]$driverlon = country.centroids$driverlon[s]
}
for (s in 1:length(reptile)){
  reptile[[s]]$driver.country = full.country.names[s]
  reptile[[s]] = merge(reptile[[s]], r.traits, by = "id_no")
  reptile[[s]]$driverlat = country.centroids$driverlat[s]
  reptile[[s]]$driverlon = country.centroids$driverlon[s]
}

bird = bind_rows(bird)
mammal = bind_rows(mammal)
reptile = bind_rows(reptile)
full = bind_rows(bird, mammal, reptile)
full$dist = rep(NA, length(full$id_no))
for (i in 1:length(full$id_no)){
  # Have to use Haversine great circle distance for distance between two lat-lon points
  full$dist[i] = distHaversine(c(full$driverlon[i], full$driverlat[i]),
                                       c(full$centroid_lon[i], full$centroid_lat[i]))/1000 # units of km
}

full$drivenprop = full$drivenloss/full$aoh2000
full$drivenlog = log(full$drivenprop +
                       quantile(full$drivenprop, probs = c(0.25), na.rm = T)^2/
                       quantile(full$drivenprop, probs = c(0.75), na.rm = T))
c = quantile(full$drivenprop, probs = c(0.25), na.rm = T)^2/
  quantile(full$drivenprop, probs = c(0.75), na.rm = T)
full$drivensqrt = sqrt(full$drivenprop)
full$drivenlog2 = log(full$drivenprop + 0.1)
full$drivenlog3 = log(full$drivenprop + 0.01)

plt <- ggplot(full, aes(x = dist, y = drivenprop)) +
  geom_point(alpha = 1/3, size = 0.3, pch = 19) +
  geom_smooth(method = "loess", se = F) + 
  xlab("Distance between Species Range and Driver Country (km)") +
  ylab("Proportion of Range Lost")

m1 = lm(drivenprop ~ dist, data = full)
m2 = lm(drivenlog ~ dist, data = full)
m3 = lm(drivensqrt ~ dist, data = full)
m4 = lm(drivenlog2 ~ dist, data = full)
m5 = lm(drivenlog3 ~ dist, data = full)
hist(resid(m1)) # Not a good model
hist(resid(m2)) # Good residuals
hist(resid(m3)) # Not good residuals
hist(resid(m4)) # Not good residuals
hist(resid(m5)) # Not good residuals
# AIC(m1, m2, m3, m4, m5)
# outliers = full[full$dist < 15000,]
# threshold = quantile(outliers$drivenprop, 0.99, na.rm = T) # 2.76%
# outliers = outliers[full$drivenprop > threshold,]
# outliers = outliers[complete.cases(outliers),]
cooksD <- cooks.distance(m2)
# influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential <- cooksD[cooksD > 4/length(full$dist)]
outliers <- full
outliers = outliers[as.numeric(names(influential)),]
outliers = outliers[outliers$dist > 5000,]
outliers$pred = predict(m2, outliers)
outliers = outliers[outliers$drivenlog > outliers$pred,]
plot(full$dist, full$drivenprop, pch = 19, cex=0.01)
points(outliers$dist, outliers$drivenprop, pch = 19, cex = 0.01, col = "red")
length(outliers$dist) #2047 species-country pairs
mad.spp = outliers[outliers$centroid_lon < 55 & outliers$centroid_lon > 42 &
                     outliers$centroid_lat < -11.5,]
mad.spp = mad.spp[complete.cases(mad.spp),]
length(mad.spp$dist) # 941 species-country pairs

# To save:
{
  png(paste0("fig4_plot.png"), units = "px", height=3500,
      width=5000, res=600)
  plt
  dev.off()
}

# To plot madagascar inset
usa = rast(paste0(getwd(), "/IDBL_outputs/USA_hotspots.tif"))
usa.mad = crop(usa, ext(c(43, 51.06, -26, -11.7)))
world = map_data("world")
madagascar = world[world$lat > -26 & world $lat < -11.7 &
              world$long > 43 & world$long < 51.06,]
plt = ggplot() +
  geom_map(
    data = madagascar, map = madagascar,
    aes(long, lat, map_id = region),
    color = "black", fill = "darkgray", size = 0.1
  ) +
  geom_spatraster(data = usa.mad) +
  geom_map(
    data = madagascar, map = madagascar,
    aes(long, lat, map_id = region),
    color = "black", fill = "transparent", size = 0.05
  ) +
scale_fill_gradientn(colors = c('#000040', '#02d1fa',  '#b81a1f', '#fa5902',
                                         '#fad102', '#f7ff03', '#ffffff'), 
                                         na.value = "white")

{
  png(paste0("fig4_madagascar.png"), units = "px", height=2860,
      width=1612, res=600)
  plt
  dev.off()
  }

### Exception statistics
not.mad = outliers[!outliers$id_no %in% mad.spp$id_no,]
sum(mad.spp[mad.spp$driver.country == "United States of America",]$drivenprop,
    na.rm =T) / sum(mad.spp$drivenprop, na.rm = T) #71.0%
length(mad.spp[mad.spp$driver.country == "United States of America",]$drivenprop) #378 spp.
sum(mad.spp[mad.spp$driver.country == "Mexico",]$drivenprop,
    na.rm =T) / sum(mad.spp$drivenprop, na.rm = T) #2.8%
sum(mad.spp[mad.spp$driver.country == "Canada",]$drivenprop,
    na.rm =T) / sum(mad.spp$drivenprop, na.rm = T) #7.7%
sum(mad.spp[mad.spp$driver.country == "Japan",]$drivenprop,
    na.rm =T) / sum(mad.spp$drivenprop, na.rm = T) #4.8%
sum(mad.spp[mad.spp$driver.country == "Germany",]$drivenprop,
    na.rm =T) / sum(mad.spp$drivenprop, na.rm = T) #3.9%
sum(mad.spp[mad.spp$driver.country == "France",]$drivenprop,
    na.rm =T) / sum(mad.spp$drivenprop, na.rm = T) #9.8%
# sum(mad.spp[mad.spp$driver.country != "United States of America" &
#               mad.spp$driver.country != "France" &
#               mad.spp$driver.country != "Canada" &
#               mad.spp$driver.country != "Japan",]$drivenprop,
#     na.rm =T) / sum(mad.spp$drivenprop, na.rm = T) #
atl.spp = outliers[outliers$centroid_lon < -30 & outliers$centroid_lon > -60 &
                     outliers$centroid_lat < 0 & outliers$centroid_lat > -32.5,] # Atlantic forest spp.
atl.spp = atl.spp[complete.cases(atl.spp),]
length(atl.spp$id_no)/length(not.mad$id_no) #50.6%
eaf.spp = outliers[outliers$centroid_lon < 42 & outliers$centroid_lon > 30 &
                     outliers$centroid_lat > -12,] # East African spp.
eaf.spp = eaf.spp[complete.cases(eaf.spp),]
length(eaf.spp$id_no)/length(not.mad$id_no) #10.0%
idn.spp = outliers[outliers$centroid_lon < 180 & outliers$centroid_lon > 130 &
                     outliers$centroid_lat > -12,] # Insular southeast asia spp.
idn.spp = idn.spp[complete.cases(idn.spp),]
length(idn.spp$id_no)/length(not.mad$id_no) #6.2%


library(tidyverse)
world_coordinates <- map_data("world")
ggplot() +
  geom_map(
    data = world_coordinates, map = world_coordinates,
    aes(long, lat, map_id = region)
  ) +
  geom_point(data = not.mad, aes(x = centroid_lon, y = centroid_lat))

