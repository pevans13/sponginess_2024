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
                     , "images"
                     , "docs/references", "docs/spreadsheets"
                     
                     # continent shapefiles
                     , "data/intermediate_outputs/cn"
                     
                     # saving figures
                     , "results/images"
)
pblapply(files.toInclude, function(x) {dir.create(x, showWarnings = F, recursive = T)})
rm(files.toInclude)

# path the r scripts
rscriptsPath <- file.path("code", "r_scripts")

#### User input ####
## set the results directory to a local directory on your computer
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
data.hsg <- file.path("hsg_class", "HYSOGs250m.tif")
### land cover map (LCM)
data.lcm <- file.path("land_cover", "LCM2015PlusCrops.tif")
### land cover map (LCM)
data.slope <- file.path("IHDTM_50m_toUse.tif")

## set final paths for different data at the correct resolution and extent
finalHsg <- file.path("data", "raw", "hsg_data.tif")
finalLcm <- file.path("data", "raw", "lcm_data.tif")
finalSlope <- file.path("data", "raw", "slope_data.tif")

#### which elements should be run? ####
## ------------ Notes --------------  ##
## The below lines should be assigned 'TRUE' (or 'T') if you want them to run
## ------------ ----- --------------  ##

# Step 1: ensure all the relevant datasets [hydrologic soil group (HSG), land cover, slope] are at the correct resolution and extent
part1prepareData <- F
# Step 2: create the non-slope CN maps based on land cover and HSG 
part2cnCreate <- F
# Step 3: create the slope-adjusted CN maps based on land cover and HSG 
part3cnCreateSlope <- F
# Step 4: create the initial abstraction maps
part4Ia <- F
IaVary <- T # IaLand * S (where different land covers have different Ias)
# Step 5: create storm precipitation
part5Precip <- F
# Step 6: calculate the final run-off (Q, in mm) based on sponginess
part6Qcalculation <- F
# Step 6 chooses. If 'part6Qcalculation' is TRUE, select which Q to create based on different Ia
IaVary.Q <- F # IaLand * S (where different land covers have different Ias)
# Step 7: convert the final maps from the base resolution to the final resolution
part7finalResolutionQ <- F
# Step 8: create the final visual output for the results
part8qVisualimages <- T

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

demIn.res <- 50
readmePath <- file.path(getwd(), "readme_files.md")
# Create and open the file connection
fileConn <- file(readmePath)
writeLines(c(
  "Creator: Dr. Paul M. Evans (paueva@ceh.ac.uk) | Git: https://github.com/pevans13",
  "Date created: 2023-04-13",
  paste0("\n-----------------------------"),
  paste0("All files in the below section were created using the 'checkResolutionExtent.R' script"
         , "\nSection last updated: ", format(Sys.Date())),
  paste0(
    "\nLand cover map (LCM) files:", "\n",
    "\tfilename: 'data/raw/lcm_data.tif'", "\n",
    "\tdetails: The data in this file were all derived from Rowland et al., 2017 [https://doi.org/10.5285/bb15e200-9349-403c-bda9-b430093807c7]", "\n",
    "\tUnits: land cover classes", "\n",
    "\t\t", "1 = Broadleaved woodland", "\n",
    "\t\t", "2 = Coniferous woodland", "\n",
    "\t\t", "3 = Arable and horticulture", "\n",
    "\t\t", "401 = Improved grassland", "\n",
    "\t\t", "5 = Neutral grassland", "\n",
    "\t\t", "6 = Calcareous grassland", "\n",
    "\t\t", "7 = Acid grassland", "\n",
    "\t\t", "8 = Fen, marsh and swamp", "\n",
    "\t\t", "9 = Heather", "\n",
    "\t\t", "10 = Heather grassland", "\n",
    "\t\t", "11 = Bog", "\n",
    "\t\t", "12 = Inland rock", "\n",
    "\t\t", "13 = Saltwater", "\n",
    "\t\t", "14 = Freshwater", "\n",
    "\t\t", "15 = Supra-littoral rock", "\n",
    "\t\t", "16 = Supra-littoral sediment", "\n",
    "\t\t", "17 = Littoral rock", "\n",
    "\t\t", "18 = Littoral sediment", "\n",
    "\t\t", "19 = Saltmarsh", "\n",
    "\t\t", "20 = Urban", "\n",
    "\t\t", "21 = Suburban", "\n",
    "\t\t", "301 = Arable grass", "\n",
    "\t\t", "302 = Winter wheat", "\n",
    "\t\t", "303 = Winter barley", "\n",
    "\t\t", "304 = Spring wheat", "\n",
    "\t\t", "305 = Oats", "\n",
    "\t\t", "306 = Maize", "\n",
    "\t\t", "307 = Oilseed rape", "\n",
    "\t\t", "308 = Spring barley", "\n",
    "\t\t", "309 = Potatoes", "\n",
    "\t\t", "310 = Beans", "\n",
    "\t\t", "311 = Sugarbeet", "\n",
    "\tSpatial resolution: 25 m2\n",
    "\tSpatial extent: GB", "\n",
    "\tTemporal coverage: 2015", "\n",
    "\tNative projection: 27700"),
  
  paste0(
    "\nSlope files:", "\n",
    "\tNote: all data relating to slope was derived from CEH's Integrated Hydrological Digital Terrain Model (IHDTM), which had a native resolution of 50 m2, and was gridded data.", "\n\n", 
    
    "\tfilename: 'data/intermediate_outputs/demSlopeDegrees.tif'", "\n",
    "\tdetails: slope (in degrees) was calculated from the IHDTM data using the 'terrain' function from the raster R package. It used the 8 neighbouring pixels to calculate slope", "\n",
    "\tUnits: degrees", "\n",
    "\tSpatial resolution: ", demIn.res, "m2\n",
    
    "\n\tfilename: 'data/intermediate_outputs/demSlopePercent.tif'", "\n",
    "\tdetails: The slope degrees data were converted into percetanges using tan(slope[in degrees] * pi/180)*100"
    , "\n",
    "\tUnits: %", "\n",
    "\tSpatial resolution: ", demIn.res, "m2\n"
  ),
  paste0(
    "\tfilename: 'data/raw/slope_data.tif'", "\n",
    "\tdetails: The percent slope data were resampled to 25 m2 using 'resample' from the raster R package. It was calculated using nearest neighbour algorithm", "\n",
    "\tUnits: %", "\n",
    "\tSpatial resolution: ", resolution.base, "m2\n",
    "\tSpatial extent: GB", "\n",
    "\tTemporal coverage: 2004", "\n",
    "\tNative projection: 27700"),
  
  paste0(
    "\nHydrological soil group (HSG) files:", "\n",
    "\tNote: all data relating to HSG were derived from Ross et al. (2018) [10.3334/ORNLDAAC/1566], which had a native projection of epsg: 4326.", "\n\n", 
    
    "\tfilename: 'data/intermediate_outputs/HYSOGs250mUKpoints.gpkg'", "\n",
    "\tdetails: The centroids of each pixel was extracted and classified as a HSG category, based on Table 1 from https://daac.ornl.gov/SOILS/guides/Global_Hydrologic_Soil_Group.html, with A = 1 to D = 4", "\n",
    "\tSpatial resolution: 250-m", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/HYSOGs250mclass.tif'", "\n",
    "\tdetails: rasterised version of 'HYSOGs250mUKpoints.gpkg'", "\n",
    "\tSpatial resolution: 250-m", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/HYSOGs2class.tif'", "\n",
    "\tdetails: resampled version of 'HYSOGs250mclass.tif', calculated using 'disaggregate' from the raster R package, using a factor of 2", "\n",
    "\tSpatial resolution: 25-m", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/HYSOGs2classResample.tif'", "\n",
    "\tdetails: resampled version of 'HYSOGs2class.tif' to get correct extent, calculated using 'resample' from the raster R package, using the 'bilinear' method", "\n",
    "\tUnits: HSG categories", "\n",
    "\tSpatial resolution: 25-m", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/HYSOGs2classNgb.tif'", "\n",
    "\tdetails: resampled version of 'HYSOGs2class.tif' to get correct extent, calculated using 'resample' from the raster R package, using the 'nearest neighbour' method", "\n",
    "\tSpatial resolution: 25-m", "\n",
    
    "\n\tfilename: 'data/raw/hsg_data.tif'", "\n",
    "\tdetails: final version of HSG categories with water or coastal pixels excluded", "\n",
    "\tUnits: HSG categories", "\n",
    "\tSpatial resolution: 25-m", "\n",
    "\tSpatial extent: GB", "\n",
    "\tTemporal coverage: 2008", "\n",
    "\tFinal projection: 27700"),
  
  paste0("\n-----------------------------"),
  paste0("All files in the below section were created using the 'cnMapsNonSlopeCreate.R' script"
         , "\nSection last updated: ", format(Sys.Date())),
  
  paste0(
    "\nCurve Number (CN) files:", "\n",
    "\tfilename: 'data/raw/cntables_nisbet.csv'", "\n",
    "\tdetails: The data in this file contain the S (maximum potential retention) derived from CN values, based on a simple transformation: S <- (25400/CN_value) - 254", "\n",
    
    "\n\tfilename: 'data/raw/cnAMC.csv'", "\n",
    "\tdetails: The data in this file contains the CN results using different equations", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/[x]_reclass.csv' (with x representing a letter)", "\n",
    "\tdetails: The data in these files contain the individual CN number for different HSGs", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/hsg_lcm[x].tif' (with x representing a letter)", "\n",
    "\tdetails: The data in these files contain the individual CN number for different HSGs applied across GB.", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/CNmap_amc[x].tif' (with x representing a number)", "\n",
    "\tdetails: The data contain spatial CN values based on the specific number of antecedent moisture content (AMC) being  used.", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/S_amc[x].tif' (with x representing a number)", "\n",
    "\tdetails: The data contain spatial S (maximum potential retention) values based on the specific number of AMC  used.", "\n",
    "\tUnits: CN (unitless) or mm (for S)", "\n",
    "\tSpatial resolution: 25-m", "\n",
    "\tSpatial extent: GB", "\n",
    "\tTemporal coverage: 2015", "\n",
    "\tFinal projection: 27700"),
  
  paste0("\n-----------------------------"),
  paste0("All files in the below section were created using the 'cnMapsWithSlope.R' script"
         , "\nSection last updated: ", format(Sys.Date())),
  
  paste0(
    "\nCurve Number (CN) files:", "\n",  
    "\tfilename: 'data/intermediate_outputs/CNmap_amc2_slope.tif'", "\n",
    "\tdetails: The data in this file were created by adjusting the CN values by slope, using Sharpley and Williams' (1990) equation [Sharpley, A. & Williams, J. Epic—Erosion/Productivity Impact Calculator: 1. Model Documentation. (1990)]. 
",
    "\n\tfilename: 'data/intermediate_outputs/S_amc2_slope.tif'", "\n",
    "\tdetails: The data contains the S values, in mm, based on slope-adjusted curve numbers (for CN2)", "\n",
    "\tUnits: CN (unitless) or mm (for S)", "\n",
    "\tSpatial resolution: 25-m", "\n",
    "\tSpatial extent: GB", "\n",
    "\tTemporal coverage: 2015", "\n",
    "\tFinal projection: 27700"),
  
  paste0("\n-----------------------------"),
  paste0("All files in the below section were created using the 'IaCreation.R' script"
         , "\nSection last updated: ", format(Sys.Date())),
  
  paste0(
    "\nIa (Initial abstraction) files:", "\n",  
    "\tfilename: 'data/raw/IaTableLAI_20240626.xlsx'", "\n",
    "\tdetails: The data in this file indicate the leaf area index (LAI) per land cover type for the UK. The data were obtained from references in the file, which can be found in the manuscript. The prefix 'Ia' stands for initial abstraction i.e. the amount of water 'soaked-up' by the environment before it could runoff. Ia was calculated following: Ia = λS
    where:
      Ia = initial abstraction (mm)
      λ = proportion of precipitation in a location that will not be involved in direct runoff.
      S = maximum potential retention (mm)", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/IaLC_delta.tif'", "\n",
    "\tdetails: The data were calculated based on the conversion delta rates for specific land covers. See Evans et al. (20xx) for specific values.", "\n",
    
    "\n\tfilename: 'data/intermediate_outputs/IaSlopeLC.tif'", "\n",
    "\tdetails: The data were calculated calculated using Ia = 'IaLC_delta.tif' * S", "\n",
    "\tUnits: mm", "\n",
    "\tSpatial resolution: 25-m", "\n",
    "\tSpatial extent: GB", "\n",
    "\tTemporal coverage: 2015", "\n",
    "\tFinal projection: 27700"),
  
  paste0("\n-----------------------------"),
  paste0("All files in the below section were created using the 'final_resolution_Q.R' script"
         , "\nSection last updated: ", format(Sys.Date())),
  
  paste0(
    "\nQ (run-off) files:", "\n",  
    "\tfilenames: 'results/final_results/[mean/median]/[tif file]'", "\n",
    "\t\twhere:
    \t\t'mean' or 'median' = averaging method used when resampling from 25-m to 1-km resolutions
    \t\t[tif file] = the final result of:
    \t\t\t - the depth of runoff ('Q') from a pixel (i.e. the amount of water that leaves a pixel).
    \t\t\t - The initial abstraction (Ia) amount
    \t\t\t - The maximum potential retention ('S'), in mm", "\n",
    "\tdetails: The resolution of these data are ", resolution.final, " m2, which was aggregated from the initial ", resolution.base, " m2 output.",
    "\tUnits:  Q (mm depth) pixel runoff | Ia (unitless)", "\n",
    "\tSpatial resolution: ", resolution.final, "m2", "\n",
    "\tSpatial extent: GB", "\n",
    "\tTemporal coverage: 2015/2016", "\n",
    "\tFinal projection: 27700"),
  
  paste0(
    "\n\tfilename: 'results/final_results/final_results_1000.csv'", "\n",
    "\tdetails: The data shows the above data in spreadsheet form ", resolution.final, " m2, using median averages calculations.", "\n",
    "\tcolumns: \n",
    "\t\t'x' and 'y' = columns indicating the centroid of each grid cell at ", resolution.final, " m2, showing the X and Y coordinate, respectively (EPSG:", project.crs,")", "\n",
    "\t\t'Ia[type of Ia used]' = the initial abstraction (Ia) amount, in mm, calculated based on the maximum potential retention (or infiltration)", "\n",
    "\t\t\t'Ia5pc' and 'Ia20pc' indicate that the lambda used in the calculation of Ia was 5% and 20%, respectively", "\n",
    "\t\t\t'IaSlopeLC' indicates that the lambda used in the calculation of Ia was varied spatially via an influence of land cover", "\n",
    "\t\t'[precip]_[Ia]_Qmm_[stat]', with [precip] indicating that precipitation amount used used in the calculations, [Ia] 'Ia' indicateing which Ia was used (note if 'IaLC', see Evans et al. (20xx) for specific proportions used), and [stat] = the statistic that was used to upscale the data from ", resolution.base, " to ", resolution.final, "\n"
  )
)
, fileConn)
close(fileConn)

########## R script info ##########
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13
###################################