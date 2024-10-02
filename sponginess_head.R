## ---------------------------
##
## Script name: sponginess_head.R
##
## Purpose of script: This is the first script, that should set up all the remaining scripts
##                    with a consistent date and structure. 
##                    
## ------------ Notes --------------  ##
## This is the full run through of the sponginess model
## Below, the user should choose which aspect of the model they want to run
## ------------ ----- --------------  ##
rm(list = ls())

#### load settings ####
## automatic install of packages if they are not installed already
list.of.packages <- c(
  "terra", "raster", "dplyr", "tidyr"
  , "ggplot2", "tidyterra", "gridExtra"
  , "patchwork" # for multi-displays
  , "sf", "stars", "fasterize", "exactextractr"
  , "tictoc", "data.table", "pbapply"
  , "foreach", "parallel", "doParallel" # for parallel procesing
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}
# loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(library(package.i, character.only = TRUE)
  )
}
# tidy
rm(list.of.packages, new.packages, package.i)
terraOptions(progress=10)
options(dplyr.summarise.inform = F)
rasterOptions(progress = 'text', timer=TRUE)
terraOptions(memfrac=0.5, tempdir = "N:/temp")
tempFilePat <- dirname(rasterTmpFile())
removeTmpFiles(h=.1)
gc()

#### set the structure for the whole project ####
files.toInclude <- c("data/raw", "data/intermediate_outputs"
                     # non-data architecture
                     , "code/r_scripts"
                     , "results/tables"
                     , "images"
                     , "docs/references", "docs/spreadsheets"
                     
                     # continent shapefiles
                     , "data/intermediate_outputs/cn"
                     
                     # saving figures
                     , "results/figures"
                     , "results/figures/maps"
)
pblapply(files.toInclude, function(x) {dir.create(x, showWarnings = F, recursive = T)})
rm(files.toInclude)

# path the r scripts
rscriptsPath <- file.path("code", "r_scripts")

#### User input ####
## set the name of the folder you want to store all of the output to
## This can be the current date, for ease of determining the most recent run
## Note: comment out below
saveName <- Sys.Date()
# saveName <- "2024-03-15"

## set the results directory to a local directory on your computer
resultsPath <- file.path("results", saveName)
intermediatePath <- file.path("data", "intermediate_outputs")
dataPath <- file.path("data", "raw")

## set the resolution you want the base model to run at (this assumes a square pixel)
resolution.base <- 25

## set the resolution, in km, you want as the final results (this assumes a square pixel)
resolution.final <- 1000

## set common an extent to crop all datasets
### the current extent is for Great Britain
commonExtent <- c(xmin = 60625
                  , xmax = 655000
                  , ymin = 12000
                  , ymax = 1056000)
### create terra version
e <- terra::ext(commonExtent)

## crs required for the project
project.crs <- 27700

## set the datasets you want to include in the analysis
### hydrologic soil group (HSG)
data.hsg <- file.path("C:/Users/paueva/OneDrive - UKCEH/flood", "data_in", "hsg_class", "HYSOGs250m.tif")
### land cover map (LCM)
data.lcm <- file.path("C:/Users/paueva/OneDrive - UKCEH/flood", "data_in", "land_cover", "LCM2015PlusCrops.tif")
### land cover map (LCM)
data.slope <- file.path("C:/Users/paueva/OneDrive - UKCEH/Data/dtm_50m/IHDTM_50m_toUse.tif")

## set final paths for different data at the correct resolution and extent
finalHsg <- file.path("data", "raw", "hsg_data.tif")
finalLcm <- file.path("data", "raw", "lcm_data.tif")
finalSlope <- file.path("data", "raw", "slope_data.tif")

#### which elements should be run? ####
## ------------ Notes --------------  ##
## The below lines should be assigned 'TRUE' (or 'T') if you want them to run
## ------------ ----- --------------  ##

# Step 1: ensure all the relevant datasets [hydrologic soil group (HSG), land cover, slope] are at the correct resolution and extent
part1prepareData <- T
# Step 2: create the non-slope CN maps based on land cover and HSG 
part2cnCreate <- F
# Step 3: create the slope-adjusted CN maps based on land cover and HSG 
part3cnCreateSlope <- F
# Step 4: create the initial abstraction maps
part4Ia <- F
# Step 4 chooses. If 'part4Ia' is TRUE, select which Ia to create
Ia20 <- F # 0.2 * s
Ia05 <- F # 0.05 * S
IaVary <- T # IaLand * S (where different land covers have different Ias)
# Step 5: create storm precipitation
part5Precip <- F
# Step 6: calculate the final run-off (Q, in mm) based on sponginess
part6Qcalculation <- F
# Step 6 chooses. If 'part6Qcalculation' is TRUE, select which Q to create based on different Ia
Ia20.Q <- F # 0.2 * s
Ia05.Q <- F # 0.05 * S
IaVary.Q <- F # IaLand * S (where different land covers have different Ias)
# Step 7: convert the final maps from the base resolution to the final resolution
part7finalResolutionQ <- F
# Step 8: create the final visual output for the results
part8qVisualimages <- F

#### load settings ####
## automatic install of packages if they are not installed already
list.of.packages <- c(
  "terra", "raster", "dplyr"
  , "sf", "stars", "fasterize"
  , "readxl"
  , "tictoc", "data.table", "parallel", "pbapply"
  , "beepr" # for sounds when things are finished
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}
# loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(library(package.i, character.only = TRUE)
  )
}

# tidy
rm(list.of.packages, new.packages, package.i)
terraOptions(progress=10)
rasterOptions(progress = 'text', timer=TRUE)
tempFilePat <- dirname(rasterTmpFile())
removeTmpFiles(h=.1)
gc()

#### run elements ####
if(part1prepareData){
  source(file.path(rscriptsPath, "checkResolutionExtent.R"))
}
if(part2cnCreate){
  source(file.path(rscriptsPath, "cnMapsNonSlopeCreate.R"))
}
if(part3cnCreateSlope){
  source(file.path(rscriptsPath, "cnMapsWithSlope.R"))
}
if(part4Ia){
  source(file.path(rscriptsPath, "IaCreation.R"))
}
if(part5Precip){
  source(file.path(rscriptsPath, "storm_precipitation.R"))
}
if(part6Qcalculation){
  source(file.path(rscriptsPath, "calculate_Q.R"))
}
if(part7finalResolutionQ){
  source(file.path(rscriptsPath, "final_resolution_Q.R"))
}
if(part8qVisualimages){
  source(file.path(rscriptsPath, "Q_final_images.R"))
}

########## R script info ##########
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13
###################################