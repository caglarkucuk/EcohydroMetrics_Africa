#### Main script for detecting the events and fitting the exponential decay function!

require(dplyr)  # This is vital!
require(data.table) # Will be useful for memory-efficient indexing of the changes in the time series
require(zoo)  # Needed for rollapply functions!
require(minpack.lm)  # Needed for the nlsLM function for curve fitting
require(hydroGOF)   # Needed for calculating Nash-Sutcliffe modelling efficiency
require(splus2R) # Needed for calculating the local minima

setwd("YOUR/LOCAL/FOLDER/")  ## Give your local folder with the scripts and data

#### Load the functions necessary
source("attrCleaner.R")  # Simple function to clear unnecessary attributes in dataframes and clear up space in the memory
source("smoothDetector.R")  # Function for detecting the decay events
source("exitCorrector.R")  # Function for extending the decay events until time series would have a certain increase, step 8 of Algoritm 1
source("expFit_intFree_asyMin.R")  # Curve fitting algorithm for Eq. 1
source("expFit_process.R")  # Script for running the Algorithm 2

#### Load the data!
myPoints <- readRDS("sampleDataPoints.rds")  # Full time series of 100 grid cells randomly sampled from the study domain
myAsy <- readRDS("sampleMinValues.rds")  # FVC_min values for those grid cells

## Detect the events!
myPoints <- smoothDetector(data_points = myPoints)
## Make sure the events don't finish before a substantial increase in the observations
myPoints <- exitCorrector(data_points = myPoints)

## Get the characteristics of the events!
myDecays <- expFit_process(data_points = myPoints, asy_df = myAsy)