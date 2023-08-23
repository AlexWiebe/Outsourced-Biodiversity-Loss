
### OBL_classifyForestBirds.R

### Alex Wiebe
### Princeton University
### 8/4/22

setwd("")

library(rredlist)
library(sf)
library(terra)

my.iucn.key = "" # Change to your IUCN key

cc.path = paste0("") # Insert path for bird data
load(paste0(cc.path, "bird_sf_validated_2022_03_23.RData"))


forest.ids = c()
blocksize= 100
blocks = ceiling(length(bird_sf_validated$vert_id)/blocksize)


getHabs = function(hab, start.index){
  for(i in 1:length(hab)){
    hab[[i]] = rl_habitats(id = bird_sf_validated$id_no[c(i + start.index - 1)],
                           key = my.iucn.key)
    print(i)
  }
  hab
}



setwd(paste0(getwd(), "/forestobligate_bird_ids"))

# see OBL_classifyForestMammals.R for more comments about following code.
for(b in 1:blocks){
  if(b < blocks){
    indices = c(c(b*blocksize-99) : c(b*blocksize))
    hab = vector(mode = "list", length = 100)
    
    hab = getHabs(hab, start.index = indices[1])
    
  }else{
    indices = c(c(b*blocksize-99) : c(length(bird_sf_validated$id_no)))
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
    # if(s == length(hab)){
      id.output = as.data.frame(forest.ids)
      filepath = paste0("forestobligate_bird_ids_", b, ".csv")
      write.csv(id.output, file = filepath)
      print(paste0("Saved an output of species IDs: ", b, "."))
      forest.ids = c()
    }
    
  }
  
}







































