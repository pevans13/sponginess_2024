## ---------------------------
##
## Script name: cnMapsWithSlope.R
##
## Purpose of script: To create the second CN maps; the topologically-adjusted ones
##                    
## Run after: cnMapsNonSlopeCreate.R
##
## Run before: IaCreation.R
##
## Specific numbered tasks:
## 1 - create slope-adjusted CN map, by applying slope adjustments to the initial CN map
## 2 - create S map based on slope-adjusted CN
##
## list of final outputs:
##    S_amc3_slope.tif
##    CNmap_amc2_slope.tif
##    
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13

## ---------- notes ---------- # 
## S (maximum potential retention (or infiltration)) can be calculated based on
## the CN value. In order to calculate the slope-adjusted version of CN, first 
## CN the conditions for dry and wet antecedent moisture content need to be calculated
## --------------------------- # 

# load CN (all AMCs)
cnMap.amc2 <- rast(file.path(intermediatePath, paste0("CNmap_amc2", ".tif")))
cnMap.amc1 <- rast(file.path(intermediatePath, paste0("CNmap_amc1", ".tif")))
cnMap.amc3 <- rast(file.path(intermediatePath, paste0("CNmap_amc3", ".tif")))

# using the slope equation from Sharpley and Williams (1990):
slopePercent2 <- rast(finalSlope)

## ------------ Notes --------------  ##
## in this section, Sharpley and Williams (1990) slope-adjusted CN equation
## will be calculated
## equation: 1/3 * (CN3 - CN1) (1-2^(-13.86*slope)) + CN1
## where: CN3 = CN for dry conditions, CN1 = CN for wet conditions
## ------------ ----- --------------  ##

cat("starting at", format(Sys.time()), "\n")
# include slope in calculations
## calculate the middle proportion of the Sharpley and Williams (1990)
cat("calculating the middle proportion of the Sharpley and Williams (1990) equation for slope...\n")
e1 <- -13.86 * slopePercent2
cat("calculating the middle proportion of the Sharpley and Williams (1990) equation for slope [part2]...\n")
ePort <- 1 - 2 * exp(e1) 
hist(ePort)
# save
writeRaster(ePort, file.path(intermediatePath, "ePort.tif"), overwrite = T)
# check extents are the same
extent(st_bbox(cnMap.amc2))
extent(st_bbox(cnMap.amc1))
extent(st_bbox(cnMap.amc3))
extent(st_bbox(ePort))

# convert NAs to 0
cat("replacing NAs with 0s for CN_amc1...\n")
cnMap.amc1[is.na(cnMap.amc1)] <- 0
cat("replacing NAs with 0s for CN_amc3...\n")
cnMap.amc3[is.na(cnMap.amc3)] <- 0    
cat("replacing NAs with 0s for mid portion...\n")
ePort[is.na(ePort)] <- 0

# calculate slope - using function
cat("calculating sloped version of CN2 map\n")
CNslope <- (((1/3)*(cnMap.amc3 - cnMap.amc2)) * ePort) + cnMap.amc2
# save
cat("saving sloped version of CN2 map\n")
writeRaster(CNslope, file.path(intermediatePath, "CNmap_amc2_slope.tif")
            , overwrite = T)

cat("calculating S based on the slope-adjusted CN2...\n")
## based on non-slope input
Samc2Slope <- (25400/CNslope) - 254
# save
cat("saving S based on the slope-adjusted CN2...\n")
writeRaster(Samc2Slope, file.path(intermediatePath, "S_amc3_slope.tif"), overwrite = T)