# Global biodiversity loss from outsourced deforestation

Code and data archives tba

This repository holds code for the following manuscript under review:
> Wiebe, R.A.* and Wilcove, D.S. _2022_. Global Biodiversity Loss from Outsourced Deforestation.

*rwiebe@princeton.edu, Guyot Hall, Princeton University, Princeton, NJ

In this analysis, we quantified the range loss to forest-dwelling vertebrates that is attributable to demand for agricultural and forestry products by developed countries in 2001-2015.

We used several datasets in this analysis. Data on forest cover and forest loss were sourced from _Hansen et al._ and publicly available to download online: https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.7.html. Data on range maps for species were sourced from the IUCN and downloaded from the IUCN’s API (https://www.iucnredlist.org/resources/spatial-data-download) and its partner BirdLife International’s API (http://datazone.birdlife.org/species/requestdis). These datasets are made available for academic research on request, and an API key is necessary for their downloads. We sourced data on land use attribution from Nguyen Tien Hoang and Keiichiro Kanemoto, from their 2021 publication “Mapping the deforestation footprint of nations reveals growing threat to tropical forests.” These data are not publicly available, but the authors kindly provided the data on request.


Here, we provide information to assess and replicate our analysis.

## Overview

Four main components to this analysis. All scripts noted below can be found in the /scripts/ folder of this repo.

### 1. Data cleaning

We use a number of scripts to modify datasets before using them in analyses. These scripts pertain to aggregating data for computational efficiency, managing and cleaning range maps, and doing basic alterations of datasets.

addPlantationMasks.R <br/>
calcRangeCentroids.R<br/>
changeResolutionHansenLoss.R<br/>
changeResolutionHansenLoss_keep30m.R<br/>
changeResolutionHoang.R
classifyForestBirds.R
classifyForestMammals.R
classifyForestReptiles.R
makeHansenBinary.R

### 2. Primary analysis of range loss to species

Second, the core of our analysis is the calculation of range loss to species attributable to individual countries or across all countries. These scripts also include some code for secondary analysis (e.g., some summary statistics).

driver.R
driver_domestic.R

aohcalculations.R
functions.R

### 3. Supplementary analyses

We perform additional analyses on these outputs, for example to calculate summary statistics.

additional_analysis.R
criticallyendangered.R

### 4. Figure creation

fig2.R
fig3.R
fig3_part2.R
fig4.R
supp_fig_ratio.R

## Additional information

Some scripts have considerable computational requirements (>100 mb RAM, >100 hrs continuous runtime across all species). We ran all code on Princeton University’s High Performance Computing cluster system, with scripts run using ‘slurm’ scripts to set computational parameters for cluster usage. An example slurm script is attached (‘ex_slurm’) in this repository.

Alex Wiebe









