## ---------------------------
##
## Script name: IaCreation.R
##
## Purpose of script: To create the possible Initial abstraction, Ia, layers
##                    that may be used for the calcuation. In this script,
##                    Ia changes based on land cover
##                    
## Run after: cnMapsWithSlope.R
##
## Run before: storm_precipitation.R
##
## Specific numbered tasks:
## 1 - creates the Ia raster for land cover adjusted Ia (lcIa)
##
## list of final outputs:
##    IaSlopeLC.tif
##    
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13

#### 1 - load in S data ####
# this is the version where slope had an impact on the Curve Number
# it is at the 25 m2 resolution
sWithSlope <- rast(file.path(intermediatePath, "S_amc2_slope.tif"))
summary(sWithSlope)

#### calculate Ia based on S ####
##### use values based on land cover #####
if(IaVary){
  cat("Calculating IaSlopeLC...\n")
  # first reclassify the land cover map
  ## load it in
  cat("loading crop map...\n")
  lcmIn <- raster(data.lcm) %>%
    ## crop to common extent
    raster::crop(., commonExtent)
  ext(lcmIn)
  
  # stop("IaVary")
  
  ## load in Ia values table
  iaTable <- read_excel(file.path(dataPath, "IaTableLAI_20240626.xlsx")
                        , sheet = 2) %>% 
    as.data.frame() %>%
    dplyr::select(c(2, 3))
  head(iaTable)
  names(iaTable)
  
  ## reclassify lcm based on these values
  ### create the reclass table
  cat("reclassifying Ia based on land cover...\n")
  rclmat <- as.matrix(cbind(as.numeric(iaTable$lcm_code)
                            , as.numeric(iaTable$`Rainfall interception proportion value (λ)`))
                      , ncol = 2
                      , byrow = TRUE)
  rclmat
  # reclassify
  recLcmIa <- raster::reclassify(lcmIn, rclmat)
  ext(recLcmIa)
  xMax <- cellStats(recLcmIa, max)
  stopifnot(xMax <= 1)
  
  # save the delta output
  cat("writing the delta for the land cover-influenced delta...\n")
  writeRaster(recLcmIa, file.path(intermediatePath, "IaLC_delta.tif")
              , overwrite = T)
  cat("saved here:", file.path(intermediatePath, "IaLC_delta.tif"), "\n")
  
  # calculate Ia = λS, with λ being different for unique land covers
  cat("calculating Ia = land cover-influenced λ * S...\n")
  # multiple S by the land cover-influenced Ia
  IaLand <- terra::rast(recLcmIa) * sWithSlope
  
  # save the Ia output output
  cat("writing the Ia for the land cover-influenced data...\n")
  writeRaster(IaLand, file.path(intermediatePath, "IaSlopeLC.tif")
              , overwrite = T)
  cat("saved here:", file.path(intermediatePath, "IaSlopeLC.tif"), "\n")
}