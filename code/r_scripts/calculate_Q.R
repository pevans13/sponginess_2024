## ---------------------------
##
## Script name: calculate_Q.R
##
## Purpose of script: To calculate Q (the depth of run-off, in mm) based on the 
##                    sponginess of the land
##
## Run after: storm_precipitation.R
##
## Run before: final_resolution_Q.R
##
## Specific numbered tasks:
## 0 - load in all the Ias to be included in analysis, and the S map
## 1 - calculate (p - Ia)
## 2 - calculate (P - Ia)^2
## 3 - calculate Q
##
## list of final outputs:
##    storm25_IaSlopeLC_Qmm.tif
##    storm50_IaSlopeLC_Qmm.tif
##    storm75_IaSlopeLC_Qmm.tif
##    intermediate_outputs/storm25_IaSlopeLC_P_Ia.tif
##    intermediate_outputs/storm50_IaSlopeLC_P_Ia.tif
##    intermediate_outputs/storm75_IaSlopeLC_P_Ia.tif
##    intermediate_outputs/storm25_IaSlopeLC_P_Ia2.tif
##    intermediate_outputs/storm50_IaSlopeLC_P_Ia2.tif
##    intermediate_outputs/storm75_IaSlopeLC_P_Ia2.tif
##    
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13

#### 0 - load functions ####
# Define a function to calculate a new raster layer based on the two input layers
# minus Ia from P if P > Ia
ifAboveFunc <- function(piStack) {
  # perform some calculation using the values of x and y
  z <- ifelse(piStack[[1]] > piStack[[2]]
              , piStack[[1]] - piStack[[2]]
              , NA)
  return(z)
}

# square the result
sqFunc <- function(x) {
  z <- x^2
  return(z)
}

# calculate Q using the created rasters
calcQFunc <- function(piStack) {
  # for piStack, use:
  # x = P-Ia^2: piStack[[1]]
  # y = P-Ia: piStack[[2]]
  # s = S: piStack[[3]]
  zSlope <- piStack[[1]] / (piStack[[2]] + piStack[[3]])
  return(zSlope)
}

## 0 - load in all the Ias to be included in analysis, and the S map
IaAnalyse <- c(); IaNames <- c()
if(IaVary.Q){
  cat("loading IaSlopeLC.tif in...\n")
  IaVary <- rast(file.path(intermediatePath, "IaSlopeLC.tif"))
  IaAnalyse <- c(IaAnalyse, IaVary)
  IaNames <- c(IaNames, "IaSlopeLC")
}

## load S map
cat("loading sloped S map in...\n")
sMaxNm <- rast(file.path(intermediatePath, "S_amc2_slope.tif"))
# make 0 slope NA
sMaxNm[sMaxNm == 0] <- NA

## ------------ Notes --------------  ##
## The three steps taken below will be run for each precipitation file in
## the first list ('precip.list')
## ------------ ----- --------------  ##

precip.list <- list.files(file.path(dataPath, "precipitation_storm")
                          , full.names = T
                          , pattern = ".tif$")
precip.list
prec = precip.list[[1]]
for(prec in precip.list){
  
  # load in the raster of precipitation
  # tic("total time for one storm loop")
  
  # get current P
  currP <- rast(prec)
  # get name
  currPname <- gsub(".tif", "", basename(prec))
  
  # do it for all Ias
  i = 1
  for(i in 1:length(IaAnalyse)){
    # get current Ia
    currIa <- IaAnalyse[[i]]
    currIa
    
    cat(format(Sys.time()), "| starting", currPname, "with Ia =", IaNames[[i]], "\n")
    
    # stack the rasters
    cat(format(Sys.time()), "\n")
    cat("Stacking precip and Ia rasters...\n")
    rStack <- stack(c(currP, currIa))
    rStack <- c(currP, currIa)
    class(rStack)
    rStack
    
    ## ------------ Notes --------------  ##
    ## The below code runs the analysis to determine Q. It does it in three parts,
    ## saving the output at each stage, for easy reference later.
    
    ## This analysis used multiple cores on the computer. 
    ## ------------ ----- --------------  ##
    
    # see the row numbers being analysed, on how many cores
    numCores <- detectCores() - 4
    cat(numCores, "\n")
    
    #### 1 - calculate (p - Ia) ####
    PIaSave <- file.path(intermediatePath
                         , paste0(currPname, "_", IaNames[[i]], "_P_Ia.tif"))
    if(file.exists(PIaSave)){
      
      cat(PIaSave, "(PminusIa) already exists\n")
      PminusIa <- raster(PIaSave)
      
    } else {
      tic("PminusIa-ed")
      cat("starting on", numCores, "cores\n")
      cat(format(Sys.time()), "\n")
      cat("P - Ia is being calculated [part 1]...")
      # set up the cluster object for parallel computing
      beginCluster(numCores)
      
      # calculate P-Ia
      if(class(rStack) == "SpatRaster"){
        cat("using 'app'\n")
        system.time(PminusIa <- app(rStack, ifAboveFunc))
      } else {
        cat("using 'clusterR'\n")
        system.time(PminusIa <- clusterR(rStack, calc, args = list(fun = ifAboveFunc), export = "ifAboveFunc"))
      }
      
      # save
      cat("\n", "saving", paste0(currPname, "_", IaNames[[i]], "_P_Ia.tif"), "\n")
      writeRaster(PminusIa
                  , PIaSave
                  , overwrite = T)
      # done with cluster object
      endCluster()
      toc(log = T)
      cat("PminusIa done\n")
      beep(1)
    }
    
    #### 2 - calculate (p - Ia)^2 ####
    PIa2Save <- file.path(intermediatePath
                          , paste0(currPname, "_", IaNames[[i]], "_P_Ia2.tif"))
    if(file.exists(PIa2Save)){
      
      cat(PIa2Save, "(PminusIa2) already exists\n")
      PminusIa2 <- raster(PIa2Save)
      
    } else {
      tic("PminusIa squared-ed")
      cat("starting on", numCores, "cores\n")
      cat(format(Sys.time()), "\n")
      cat("the square of P - Ia is being calculated [part 2]...\n")
      # set up the cluster object for parallel computing
      beginCluster(numCores)
      
      # calculate P-Ia
      if(class(PminusIa) == "SpatRaster"){
        cat("using 'app'\n")
        system.time(PminusIa2 <- app(PminusIa, sqFunc))
      } else {
        cat("using 'clusterR'\n")
        # calculate the square of P - Ia
        system.time(PminusIa2 <- clusterR(PminusIa, calc, args = list(fun = sqFunc), export = "sqFunc"))
      }
      # save
      cat("\n", "saving", paste0(currPname, "_", IaNames[[i]], "_P_Ia2.tif"), "\n")
      writeRaster(PminusIa2
                  , PIa2Save
                  , overwrite = T)
      # done with cluster object
      endCluster()
      toc(log = T)
      cat("PminusIa squared done\n")
      beep(3)
    }
    
    #### 3 - calculate Q ####
    QSave <- file.path(intermediatePath
                       , paste0(currPname, "_", IaNames[[i]], "_Qmm.tif"))
    if(file.exists(QSave)){
      cat(QSave, "(QSave) already exists\n")
    } else {
      tic("Q calculated")
      cat("starting on", numCores, "cores\n")
      cat(format(Sys.time()), "\n")
      # set up the cluster object for parallel computing
      cat("Q, in mm, is being calculated... [part 3]\n")
      Sys.sleep(5)
      # use the 'calcQFunc' function to calculate Q
      ## create stack - to include in function
      stackForQ <- c(rast(PminusIa2) # (P - Ia)^2
                     , rast(PminusIa) # (P - Ia)
                     , sMaxNm) # S (max potential retention)
      # first, inspect
      summary(PminusIa2)
      summary(stackForQ[[1]])  # Check PminusIa2
      summary(PminusIa)
      summary(stackForQ[[2]])  # Check PminusIa
      summary(sMaxNm)
      summary(stackForQ[[3]])  # Check sMaxNm
      
      # Apply the calcQFunc using app with parallel processing
      system.time(
        qOut1 <- terra::app(stackForQ, fun = calcQFunc)
      )
      qOut1

      # save
      cat("\n", "saving", paste0(currPname, "_", IaNames[[i]], "_Qmm.tif"), "\n")
      
      writeRaster(qOut1
                  , QSave
                  , overwrite = T)
      # done with cluster object
      toc(log = T)
      cat("Q, in mm, has been calculated\n")
    }
  } # Ia
} # Precipitation