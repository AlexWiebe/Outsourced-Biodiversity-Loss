
### OBL_additional_analysis.R

### Alex Wiebe
### Princeton University

### DESCRIPTION:
### Assorted analyses to generate statistics.

setwd("")

###---- To calculate average proportion of species that have nonzero (or >1 km^2)
### habitat loss attributable to each driver country.
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


###--------------Data manipulation

### Create one dataframe with each country's global data
{
  global = data.frame(country.abbrev = country.names,
                      country.name = full.country.names,
                      nonzero = rep(NA, length(country.names)))
}

### Populate global.rangeeqs (global range equivalents of habitat loss)
{
  for (c in 1:length(global$country.name)){
    r = rept[[c]]
    b = bird[[c]]
    m = mammal[[c]]
    
    # Remove all species without coverage
    r = r[r$aoh2000 > 0,]; b = b[b$aoh2000 > 0,]; m = m[m$aoh2000 > 0,]
    
    # Remove extinct species
    ex = read.csv("IUCN_extinctspecies.csv", header = T)
    r = r[!(r$id_no %in% ex$taxonid),]
    b = b[!(b$id_no %in% ex$taxonid),]
    m = m[!(m$id_no %in% ex$taxonid),]
    
    r.nonzero = length(r[r$drivenloss > 1,]$id_no)
    b.nonzero = length(b[b$drivenloss > 1,]$id_no)
    m.nonzero = length(m[m$drivenloss > 1,]$id_no)
    
    nonzero = sum(r.nonzero, b.nonzero, m.nonzero, na.rm = T)
    
    global$nonzero[c] = nonzero
  }
}

median(global$nonzero)/sum(length(rept[[1]]$id_no), length(bird[[1]]$id_no),
                                  length(mammal[[1]]$id_no))



###---- To calculate median contributions of driver countries as a whole

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
{
  for (c in 1:length(country.names)){
    r = rept[[c]]
    b = bird[[c]]
    m = mammal[[c]]
    
    r.dom = rept.dom[[c]]
    b.dom = bird.dom[[c]]
    m.dom = mammal.dom[[c]]
    
    # Remove extinct species
    ex = read.csv("IUCN_extinctspecies.csv", header = T)
    r = r[!(r$id_no %in% ex$taxonid),]
    b = b[!(b$id_no %in% ex$taxonid),]
    m = m[!(m$id_no %in% ex$taxonid),]
    ex = read.csv("IUCN_extinctspecies.csv", header = T)
    r.dom = r.dom[!(r.dom$id_no %in% ex$taxonid),]
    b.dom = b.dom[!(b.dom$id_no %in% ex$taxonid),]
    m.dom = m.dom[!(m.dom$id_no %in% ex$taxonid),]
    
    r$drivenloss = r$drivenloss - r.dom$drivenloss
    b$drivenloss = b$drivenloss - b.dom$drivenloss
    m$drivenloss = m$drivenloss - m.dom$drivenloss
    
    full = rbind(r, b, m)
    for(i in 1:length(full$id_no)){
      if(full$drivenloss[i] < 0){
        full$drivenloss[i] = 0
      }
    }
    
    
    if(c == 1){
      global = full
    } else{
      global$drivenloss = global$drivenloss + full$drivenloss
    }
  }
}

View(global) # all values under 'drivenloss' only include international losses
# attributable to driver countries

# global = global[global$aoh2000 > 0,]
global$drivenprop = global$drivenloss / global$rangeloss
median(global$drivenprop, na.rm = T) # 13.3%



###---- Calculating contributions to Critically Endangered species, using the
### data products from the above analysis.

bm = read.csv("birdsandmammals_cr.csv", header = T)
bm = data.frame(id_no = bm$internalTaxonId,
                IUCN = rep("cr", length(bm$internalTaxonId)))
r = read.csv("CR_reptiles.csv", header = T)
r = data.frame(id_no = r$internalTaxonId,
               IUCN = rep("cr", length(r$internalTaxonId)))
cr = rbind(bm, r)

global.cr = global[global$id_no %in% cr$id_no,]
global.cr$losses = global.cr$drivenloss / global.cr$aoh2000
sum(global.cr$drivenloss, na.rm = T) / sum(global.cr$rangeloss, na.rm = T) # 16.2%
# of all habitat loss to these species is attributable to the driver countries
median(global.cr$drivenprop, na.rm = T) # 14.0% average international contribution
# to these species' ranges

### A Supplementary Figure
plt <- ggplot(global.cr, aes(x = drivenprop)) + 
  geom_histogram(colour="black", fill="white")+
  geom_density(aes(y = 0.036 * ..count..), alpha=.2, fill="#FF6666") +
  ggtitle("Proportion of Habitat Loss Attributable to \nInternationally Driven Deforestion") +
  xlab("Proportion of Total Range Loss Attributable to Driver Countries") +
  ylab("Number of Species") +
  xlim(0,1)+
  theme(plot.title = element_text(hjust = 0.5))

plt

# To save:
{
  png("supp_fig_criticallyendangered.png", units = "px", height=3000, width=6000, res=600)
  plt
  dev.off()
  }





