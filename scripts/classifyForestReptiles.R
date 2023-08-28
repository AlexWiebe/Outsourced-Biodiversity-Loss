
### classifyForestReptiles.R

### Alex Wiebe
### Princeton University
### 8/4/22

setwd("")

library(rredlist)
library(terra)

my.iucn.key = "" # Insert your IUCN key here
reptile_ranges = vect(paste0("/Reptile_rangemaps/REPTILES.shp"))

forest.ids = c()
blocksize= 100
blocks = ceiling(length(reptile_ranges)/blocksize)


getHabs = function(hab, start.index){
  for(i in 1:length(hab)){
    hab[[i]] = rl_habitats(id = reptile_ranges$id_no[c(i + start.index - 1)],
                           key = my.iucn.key)
    print(i)
  }
  hab
}



setwd(paste0("/forestobligate_reptile_ids"))

# See OBL_classifyForestMammals.R for more comments on the following code.
for(b in 1:blocks){
  if(b < blocks){
    indices = c(c(b*blocksize-99) : c(b*blocksize))
    hab = vector(mode = "list", length = 100)
    
    hab = getHabs(hab, start.index = indices[1])
    
  }else{
    indices = c(c(b*blocksize-99) : c(length(reptile_ranges)))
    hab = vector(mode = "list", length = length(indices))
    hab = getHabs(hab, start.index = indices[1])
  }
  
  for (s in 1:length(hab)){
    # Subset to only habitats with classification "Suitable" (not "Marginal").
    # For example, for Harpy Eagle tropical forest is "Suitable," and
    # tropical grassland is "Marginal," and we will only use tropical forest
    # as the habitat for that species.
    hab.result = hab[[s]]$result
    if(length(hab.result) > 0){
      if(dim(hab.result[hab.result$suitability == "Suitable",])[1] > 0 &&
         !is.na(hab.result[hab.result$suitability == "Suitable",])[1]){
        
        suitable.hab = hab[[s]]$result[hab[[s]]$result$suitability == "Suitable",]
        suitable.hab = suitable.hab[!is.na(suitable.hab$suitability),]
        # Forest codes all start with 1 (e.g., "1.2" is tropical forest), so we just
        # need to know if all of the codes are less than 2
        if (all(with(suitable.hab, as.numeric(suitable.hab$code) < 2))){
          forest.ids = c(forest.ids, as.numeric(hab[[s]]$id))
        }
      }
    }
    
    if(s%%100 == 0 | b == blocks){
      id.output = as.data.frame(forest.ids)
      filepath = paste0("forestobligate_reptile_ids_", b, ".csv")
      write.csv(id.output, file = filepath)
      print(paste0("Saved an output of species IDs: ", b, "."))
      forest.ids = c()
    }
    
  }
  
}







































