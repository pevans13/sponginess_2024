## ---------------------------
##
## Script name: cnMapsNonSlopeCreate.R
##
## Purpose of script: To create the first CN maps. They are the non-topologically
##                    adjusted ones. They just use the 'flat' CN values.
##                    
## Run after: checkResolutionExtent.R
##
## Run before: cnMapsWithSlope.R
##
## Specific numbered tasks:
## 1 - create CN map by combining LCM and HSG, and condition
## 2 - create S, the maximum potential retention, map from the CN
##
## list of final outputs:
##    S_amc1.tif
##    S_amc2.tif
##    S_amc3.tif
##    CNmap_amc1.tif
##    CNmap_amc2.tif
##    CNmap_amc3.tif
##    Reclassification tables and tifs based on HSG (csvs of A|B|C|D_reclass and 
##                                                   hsg_lcmA|B|C|D)
##    
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13

dir.create(file.path(intermediatePath, "cn_creation"))

#### 1 - calculate CN map ####
tic("cn map")
# load in CN conversion table associated table
cnTable <- fread(file.path(dataPath, "cntables_nisbet.csv"), header = T) %>%
  as.data.frame() %>%
  # select lcm names, and ABCD codes 
  dplyr::select(c(2, 5:8)) %>%
  # remove top 2 rows
  slice(3:n()) 
# correct names
names(cnTable) <- c("lcm", "A", "B", "C", "D")
head(cnTable)
cnTable

# get unique values from HSG (at the project resolution)
HSG25 <- rast(finalHsg)
## get unique values
uniHSG <- unique(HSG25)
uniHSG

# stack lcm and hsg
## read in lcm
lcm <- rast(finalLcm) 
## stack with hsg
lcmHsgStack <- rast(c(lcm, HSG25))

# get the four reclassification tables, one each to correspond to the HSG categories
rcTables <- list()
for(i in 1:4){
  cat("running reclassification tables, one each to correspond to the HSG categories [number"
      , i, "out of 4...\n")
  rcTables[[i]] <- as.matrix(cbind(as.numeric(cnTable$lcm)
                                   , as.numeric(cnTable[, i+1]))
                             , ncol = 2
                             , byrow = TRUE)
  tic("rc")
  # save the table
  hsgLetter <- LETTERS[[i]]
  cnhsg <- paste0("CN_", hsgLetter)
  fwrite(rcTables[[i]] %>% as.data.frame() %>%
           rename(lcm_code = 1
                  , !!cnhsg := 2)
         , file.path(intermediatePath, "cn_creation"
                     , paste0(hsgLetter, "_reclass.csv"))
         , row.names = F)
  
  print(head(rcTables[[i]]), 10)
  
  # reclassify just with HSG column
  lcm25rc <- classify(lcm, rcTables[[i]])
  toc()
  
  tic("oneZero")
  # make 1 and 0s
  xN <- HSG25 == i
  toc()
  
  tic("rastCalc")
  # do raster calcs
  rasterCalc <- xN * lcm25rc
  toc()
  
  # save
  writeRaster(rasterCalc, file.path(intermediatePath, "cn_creation"
                                    , paste0("hsg_lcm", hsgLetter, ".tif"))
              , overwrite = T)
  beep(8)
}

combRast <- list()
for(i in 1:4){
  hsgLetter <- LETTERS[[i]]
  combRast[[i]] <- rast(file.path(intermediatePath, "cn_creation", paste0("hsg_lcm", hsgLetter, ".tif")))
}
# stack the layers using rast
stackcombRast <- rast(combRast)
# get sum, which will show only results for the different HSGs
CNmap <- app(stackcombRast, sum)
# adjust the CN map, so that when a land cover is water-based, or coastal-based
## the CN becomes 100
CNmapCorr <- ifel(lcm %in% c(13:19) & is.na(CNmap), 100, CNmap)
## save
cat("saving original CN map...\n")
writeRaster(CNmapCorr, file.path("data", "intermediate_outputs", paste0("CNmap_amc2", ".tif"))
            , overwrite = T)

#### 2 - calculate S from CN ####
##### convert CN to dry and wet prior conditions ######
## ------------ Notes --------------  ##
## The conversions have been pre-calculated and can be found in a table
## ------------ ----- --------------  ##
cnAMCtable <- fread(file.path(dataPath, "cnAMC.csv")) %>%
  # reduce to just the sharpley calculations
  select(1:3)

cn1Reclass <- as.matrix(cbind(as.numeric(cnAMCtable$CN2)
                              , cnAMCtable[, 2])
                        , ncol = 2
                        , byrow = TRUE)

cn3Reclass <- as.matrix(cbind(as.numeric(cnAMCtable$CN2)
                              , cnAMCtable[, 3])
                        , ncol = 2
                        , byrow = TRUE)

# reclassify
## cn1
cat("reclassifying CN1 from CN2...\n")
cn1Reclassfied <- classify(CNmapCorr, cn1Reclass)
## cn3
cat("reclassifying CN3 from CN2...\n")
cn3Reclassfied <- classify(CNmapCorr, cn3Reclass)

## save
cat("saving reclassified CN3 and CN1...\n")
writeRaster(cn1Reclassfied, file.path("data", "intermediate_outputs", paste0("CNmap_amc1", ".tif"))
            , overwrite = T)
writeRaster(cn3Reclassfied, file.path("data", "intermediate_outputs", paste0("CNmap_amc3", ".tif"))
            , overwrite = T)

##### AMC moderate (CN2) #####
cat("calculating S based on the original (non-slope) CN...\n")
## based on non-slope input
S <- (25400/CNmapCorr) - 254
# save
cat("saving S based on the original (non-slope) CN2...\n")
writeRaster(S, file.path("data", "intermediate_outputs", "S_amc2.tif"), overwrite = T)

##### AMC dry (CN1) #####
cat("calculating S based on the original (non-slope) CN1...\n")
## based on non-slope input
Samc1 <- (25400/cn1Reclassfied) - 254
# save
cat("saving S based on the original (non-slope) CN1...\n")
writeRaster(Samc1, file.path("data", "intermediate_outputs", "S_amc1.tif"), overwrite = T)

##### AMC dry (CN3) #####
cat("calculating S based on the original (non-slope) CN3...\n")
## based on non-slope input
Samc3 <- (25400/cn3Reclassfied) - 254
# save
cat("saving S based on the original (non-slope) CN3...\n")
writeRaster(Samc3, file.path("data", "intermediate_outputs", "S_amc3.tif"), overwrite = T)
