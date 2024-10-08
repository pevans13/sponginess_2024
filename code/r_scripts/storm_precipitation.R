## ---------------------------
##
## Script name: storm_precipitation.R
##
## Purpose of script: To create the 'storm' precipitation layers that will be used
##                    for the CN method.
##                    
## Run after: IaCreation.R
##
## Run before: calculate_Q.R
##
## Specific numbered tasks:
## 1 - creates precipitation 'storm' layers of 25, 50, and 75 mm
##
## list of final outputs:
##    storm25.tif
##    storm50.tif
##    storm75.tif
##    
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13

#### 1 - create storm precipitation ####
cat("Getting stormy...\n")
dir.create(file.path(dataPath, "precipitation_storm"), showWarnings = F)

# load in LCM to get raster
lcmTemplate <- raster(finalLcm)

# make three separate precips, showing 'storm'
stormList <- list()
for(i in c(25, 50, 75)){
  
  if(!file.exists(file.path(dataPath, "precipitation_storm"
                            , paste0("storm", i, ".tif")))){
    
    tic("storming")
    cat("Storming...", i, "...\n")
    currStorm <- lcmTemplate
    cat("Storming...", i, "... [removing 0s]\n")
    currStorm[currStorm > 0] <- i
    stormList[[i]] <- currStorm
    # print(table(as.vector(currStorm)))
    toc()
    cat("Storming...", i, "... [saving]\n")
    writeRaster(stormList[[i]], file.path(dataPath, "precipitation_storm"
                                          , paste0("storm", i, ".tif")))
    
  }
}

