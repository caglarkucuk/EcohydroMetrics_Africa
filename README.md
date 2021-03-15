# Introduction

This repository provides the scripts to apply the algorithms explained in the manuscript:

Küçük Ç., Koirala S., Carvalhais N., Miralles D.G., Reichstein M., and Jung M. (2020) Characterising the response of vegetation cover to water limitation in Africa using geostationary satellites.

# Developer
The scripts were written by [Çağlar Küçük](https://www.bgc-jena.mpg.de/bgi/index.php/People/CaglarKucuk) 
at the [Department of Biogeochemical Integration, Max Planck Institute for Biogeochemistry](https://www.bgc-jena.mpg.de/bgi/index.php/Main/HomePage) in Jena, Germany. 
All questions and information shall be directed to [ckucuk[at]bgc-jena.mpg.de](ckucuk@bgc-jena.mpg.de).

# Description

## Functions

smoothDetector.R -> Implementation of Steps 1-7 of Algorithm-1 in the corresponding manuscript

exitCorrector.R -> Step 8 of Algorithm-1

expFit_intFree_asyMin.R -> Curve fitting function for the exponential decay function (see Eq. 4 in the publication)

expFit_process.R -> Implementation of Steps 1-8 of Algorithm-2

## Example

main.R -> Main script to run the functions provided above

sampleDataPoints.rds -> Time series of FVC used in this study for 100 randomly selected grid cells 

sampleMinValues.rds -> FVC_min values of those randomly selected grid cells, to be used in Eq. 4

# Requires

dplyr

data.table

zoo

minpack.lm

hydroGOF

splus2R

# Disclaimer:

This repository is created reproduce the algorithms of the afformentioned manuscript. 
Any usage beyond the intended purpose are the responsibility of the users.

# License: 

CC BY-NC 4.0
