## ---------------------------
##
## Script name: final_resolution_Q.R
##
## Purpose of script: take the results of 'calculate_Q.R', which gave direct 
##                    runoff results based on the Curve Number method at
##                    the base resolution and convert them to final spatial resolution
##                    In addition, also convert Ia amounts to final resolution
##
## Run after: calculate_Q.R
##
## Run before: Q_final_images.R
##
## Specific numbered tasks:
## 1 - convert all Q (direct runoff) results to final resolution
## 2 - convert all Ia (initial abstraction) results to final resolution
## 3 - Link and flatten Q and Ia results data 
##
## list of final outputs:
##    final_results/final_results_1000.csv
##    for mean and median directories:
##        dir/Ia05pcSlope.tif
##        dir/Ia20pcSlope.tif
##        dir/IaSlopeLC.tif
##        dir/S_amc2_slope.tif
##        dir/storm25_IaSlopeLC_Qmm1000.tif
##        dir/storm50_IaSlopeLC_Qmm1000.tif
##        dir/storm75_IaSlopeLC_Qmm1000.tif
##    
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13

#### 0 - functions ####
## get extent or original precipitation
e <- terra::ext(commonExtent)
# set factor difference
xFactor <- resolution.final / resolution.base # determine factor difference
# stop("stop")

if(file.exists("maskRastR2.tif")){
  maskRast <- rast("maskRastR2.tif")
} else {
  # read in GB outline to mask
  maskRast <- st_read(file.path("C:/Users/paueva/OneDrive - UKCEH/Data/uk/boundary"
                                , "gb.gpkg")) %>% 
    st_cast("POLYGON") 
  xRast <- rast(file.path(intermediatePath, "storm25_IaSlopeLC_Qmm.tif"))
  maskRast <- fasterize::fasterize(maskRast, as(xRast, "Raster"))
  gc()
  writeRaster(maskRast, "maskRastR2.tif", overwrite=TRUE)
  maskRast <- rast(maskRast)
}

# x <- Qnames[[1]]
# create function to upscale - using the mean
m25to1kmFuncMean <- function(x){
  
  # start text
  cat("\ncalculating means for", basename(x), "at", format(Sys.time()), "\n")
  
  # load
  xRast <- terra::rast(x)
  # extent to meet ext
  xRast <- terra::extend(xRast, e)
  xRast <- terra::crop(xRast, e)
  # convert NAs to 0
  cat("converting NAs to 0s...\n")
  xRast[is.na(xRast)] <- 0
  plot(xRast)

  # mask to only gb outline
  cat("masking...\n")
  xRast <- terra::mask(xRast, maskRast)

  # writeRaster(xRast, paste0("xRast", gsub("IaSlopeLC", "", basename(x)))
  #             , overwrite = T)
  
  # aggregate
  xRastAgg <- terra::aggregate(xRast, fact = xFactor, fun = "mean"
                               , cores = 3
                               # output using the basename
                               # and save in the new directory
                               , filename = file.path("results", "final_results"
                                                      , "mean", gsub("mm.tif", paste0("mm", resolution.final, ".tif")
                                                                     , basename(x)))
                               , overwrite = T
                               , extent = e
                               , na.rm=TRUE)
  print(res(xRastAgg))
  print(ext(xRastAgg))

}

# create function to upscale - using the median
m25to1kmFuncMedian <- function(x){
  
  # start text
  cat("\ncalculating medians for", basename(x), "at", format(Sys.time()), "\n")
  
  # load
  xRast <- terra::rast(x)
  # convert NAs to 0
  cat("converting NAs to 0s...\n")
  xRast[is.na(xRast)] <- 0
  plot(xRast)

  # mask to only gb outline
  cat("masking...\n")
  xRast <- mask(xRast, maskRast)

  # writeRaster(xRast, paste0("xRast", gsub("IaSlopeLC", "", basename(x)))
  #             , overwrite = T)
  
  # aggregate
  xRastAgg <- terra::aggregate(xRast, fact = xFactor, fun = "median"
                               , cores = 3
                               # output using the basename
                               # and save in the new directory
                               , filename = file.path("results", "final_results"
                                                      , "median", gsub("mm.tif", paste0("mm", resolution.final, ".tif")
                                                                       , basename(x)))
                               , overwrite = T
                               , extent = e
                               , na.rm=TRUE)
  print(res(xRastAgg))
  print(ext(xRastAgg))

}

#### 1 - convert all Q (direct runoff) results to final resolution ####
# create mean and median directories to store final results at final resolution
dir.create(file.path("results", "final_results", "mean"), showWarnings = F, recursive = T)
dir.create(file.path("results", "final_results","median"), showWarnings = F, recursive = T)

# list all of the base resolution Q results to convert
Qnames <- list.files(intermediatePath
                     , pattern = "_Qmm.tif"
                     , full.names = T
                     , recursive = T)

# list
cat(Qnames, sep = "\n")
# load as rasters
Qnames.rasts <- pblapply(Qnames, terra::rast)

# run the functions to upscale Q results
## use mean to upscale
Qs25rasts <- pblapply(Qnames, m25to1kmFuncMean)
## use median to upscale
Qs25rasts <- pblapply(Qnames, m25to1kmFuncMedian)

#### 2 - convert all Ia (initial abstraction) results to final resolution ####
# read in all Ias
IaNames <- list.files(file.path(intermediatePath)
                      , recursive = T
                      , pattern = "^Ia(.+).tif$"
                      , full.names = T)
# list
cat(IaNames, sep = "\n")
# load as rasters
IaRasts <- pblapply(IaNames, terra::rast)

# run the function to upscale
## use mean to upscale
IaRasts <- pblapply(IaNames, m25to1kmFuncMean)
## use median to upscale
IaRasts <- pblapply(IaNames, m25to1kmFuncMedian)

#### 3 - convert all S (maximum potential retention) results to final resolution ####
## ------------ Notes --------------  ##
## This will be used in the AgLand Explorer and Viewer
## ------------ ----- --------------  ##
# read in all Ias
sNames <- list.files(file.path(intermediatePath)
                      , recursive = T
                      , pattern = "S_amc2_slope.tif$"
                      , full.names = T)
# list
cat(sNames, sep = "\n")
# load as rasters
sRasts <- pblapply(sNames, terra::rast)

# run the function to upscale
## use mean to upscale
sRasts <- pblapply(sNames, m25to1kmFuncMean)
## use median to upscale
sRasts <- pblapply(sNames, m25to1kmFuncMedian)

#### 4 - Link and flatten Q and Ia results data ####
# read in all final upscaled outputs
finalResults <- list.files(file.path("results", "final_results")
                           , recursive = T
                           , full.names = T
                           , pattern = ".tif$")
# list
cat(finalResults, sep = "\n")
mOrm <- ifelse(grepl("median", finalResults), "median", "mean")
# load as rasters
finalResults.rasts <- pblapply(finalResults, terra::rast)

# flatten the rasters and convert to a data.frame 
# do this one by one
## load first one, which will set table dimensions
cat("Combining flattened version of results...\n")
fd <- pblapply(finalResults.rasts, function(x) {
  # convert tif to dataframe
  xydf <- as.data.frame(x, xy=TRUE)
  return(xydf)
})
cat("merging flattened version of results...\n")
# combine the data frames using Reduce and merge
flatData <- Reduce(function(df1, df2) merge(df1, df2, by = c("x", "y"), all = TRUE), fd)
# convert colnames
colnames(flatData)[3:ncol(flatData)] <- paste0(sub("Slope", ""
                                                   , sub(resolution.final, ""
                                                         , sub(".tif", "", basename(finalResults))))
                                               , "_", mOrm)
# save
fwrite(flatData
       , file.path("results", "final_results"
                   , paste0("final_results_", resolution.final, ".csv"))
       , row.names = F)
