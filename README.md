# Global biodiversity loss from outsourced deforestation

Code and data archives tba

This repository holds code for the following manuscript under review:
> Wiebe, R.A.* and Wilcove, D.S. _2022_. Global Biodiversity Loss from Outsourced Deforestation.

*rwiebe@princeton.edu, Guyot Hall, Princeton University, Princeton, NJ

In this analysis, we quantified the range loss to forest-dwelling vertebrates that is attributable to demand for agricultural and forestry products by developed countries in 2001-2015.

We used several datasets in this analysis. Data on forest cover and forest loss were sourced from _Hansen et al._ and publicly available to download online: https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.7.html. Data on range maps for species were sourced from the IUCN and downloaded from the IUCN’s API (https://www.iucnredlist.org/resources/spatial-data-download) and its partner BirdLife International’s API (http://datazone.birdlife.org/species/requestdis). These datasets are made available for academic research on request, and an API key is necessary for their downloads. We sourced data on land use attribution from Nguyen Tien Hoang and Keiichiro Kanemoto, from their 2021 publication “Mapping the deforestation footprint of nations reveals growing threat to tropical forests.” These data are not publicly available, but the authors kindly provided the data on request.


Here, we provide information to assess and replicate our analysis.

# Overview

Four main components to this analysis. All scripts noted below can be found in the /scripts/ folder of this repo.

## 1/ Data cleaning. We use a number of scripts to modify datasets before using them in analyses. These scripts pertain to aggregating data for computational efficiency, managing and cleaning range maps, and doing basic alterations of datasets.

[OBL_addPlantationMasks.R]








