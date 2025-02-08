# Global biodiversity loss from outsourced deforestation



This repository holds code for the following manuscript:
> Wiebe, R.A.* and Wilcove, D.S. _2025_. Global Biodiversity Loss from Outsourced Deforestation.

*rwiebe@princeton.edu, Guyot Hall, Princeton University, Princeton, NJ

In this analysis, we quantified the range loss to forest-dwelling vertebrates that is attributable to demand for agricultural and forestry products by developed countries in 2001-2015.

We used several datasets in this analysis. Data on forest cover and forest loss were sourced from _Hansen et al._ and publicly available to download online: https://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.7.html. We sourced data on land use attribution from Nguyen Tien Hoang and Keiichiro Kanemoto, from their 2021 publication “Mapping the deforestation footprint of nations reveals growing threat to tropical forests.” These data are not publicly available, but the authors kindly provided the data on request. Data on range maps for species were sourced from the IUCN and downloaded from the IUCN’s API (https://www.iucnredlist.org/resources/spatial-data-download) and its partner BirdLife International’s API (http://datazone.birdlife.org/species/requestdis). These datasets are made available for academic research on request, and an API key is necessary for their downloads.

## Overview

There are three main components to this analysis. All scripts noted below can be found in the /scripts/ folder of this repository.

### 1. Primary analysis of range loss to species

Second, the core of our analysis is the calculation of range loss to species attributable to individual countries or across all countries. These scripts also include some code for secondary analysis (e.g., some summary statistics).

driver.R<br/>
driver_domestic.R

aohcalculations.R<br/>
functions.R

### 2. Supplementary analyses

We perform additional analyses on these outputs, for example to calculate summary statistics.

additional_analysis.R<br/>
criticallyendangered.R<br/>
OBL_sensitivity_analysis_driver.R<br/>
OBL_sensitivity_aohcalculations.R

### 3. Figure creation

fig2.R<br/>
fig3.R<br/>
fig3_part2.R<br/>
fig4.R<br/>
supp_fig1.R<br/>
supp_fig2_totallosses.R<br/>
supp_fig4.R<br/>
supp_fig_logratio.R<br/>

## Computational requirements

We ran all code using R version 4.3 on Princeton University’s High Performance Computing cluster. The code may be run on a computer with sufficient RAM to accommodate the in-memory operations. Some scripts have considerable computational requirements (>100 gb RAM, >100 hrs total continuous runtime across all species). An example slurm script (‘OBL_ex_slurm’) is housed in this repository.
