
### OBLcriticallyendangered.R

### Alex Wiebe
### Princeton University

### DESCRIPTION:
### Calculates statistics for critically endangered species, to assess
### contributions to extinction risk for the highest risk species.

setwd("")

bm = read.csv("birdsandmammals_cr.csv", header = T)
bm = data.frame(id_no = bm$internalTaxonId,
                IUCN = rep("cr", length(bm$internalTaxonId)))
r = read.csv("CR_reptiles.csv", header = T)
r = data.frame(id_no = r$internalTaxonId,
               IUCN = rep("cr", length(r$internalTaxonId)))
cr = rbind(bm, r)

### Read global data
{
  dir_rept = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Reptile"),
                         pattern = "*", full.names = T)
  rept = lapply(dir_rept, read.csv, header = T)
  dir_bird = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Bird"),
                         pattern = "*", full.names = T)
  bird = lapply(dir_bird, read.csv, header = T)
  dir_mammal = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Mammal"),
                           pattern = "*", full.names = T)
  mammal = lapply(dir_mammal, read.csv, header = T)
}
### Read domestic data
{
  dir_rept = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Reptile_Domestic"),
                         pattern = "*", full.names = T)
  rept.dom = lapply(dir_rept, read.csv, header = T)
  dir_bird = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Bird_Domestic"),
                         pattern = "*", full.names = T)
  bird.dom = lapply(dir_bird, read.csv, header = T)
  dir_mammal = list.files (path = paste0(getwd(), "/IDBL_outputs/species_data/Mammal_Domestic"),
                           pattern = "*", full.names = T)
  mammal.dom = lapply(dir_mammal, read.csv, header = T)
}

country.names = c("ARG", "AUS", "BRA", "CAN", "CHN", "DEU", "FRA",
                  "GBR", "IDN", "IND", "IRN", "ITA", "JPN",
                  "KOR", "MEX", "NOR", "RUS", "SAU", "SGP",
                  "SWE", "TUR", "UKR", "USA", "ZAF")
full.country.names = c("Argentina", "Australia", "Brazil", "Canada", "China", "Germany",
                       "France", "United Kingdom", "Indonesia", "India", "Iran",
                       "Italy", "Japan", "South Korea", "Mexico", "Norway",
                       "Russia", "Saudi Arabia", "Singapore", "Sweden",
                       "Turkey", "Ukraine", "USA",
                       "South Africa")

our.cr = rbind(rept[[1]][rept[[1]]$id_no %in% cr$id_no,],
               bird[[1]][bird[[1]]$id_no %in% cr$id_no,],
               mammal[[1]][mammal[[1]]$id_no %in% cr$id_no,])











