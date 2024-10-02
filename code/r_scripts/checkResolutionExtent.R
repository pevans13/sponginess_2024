## ---------------------------
##
## Script name: checkResolutionExtent.R
##
## Purpose of script: To check that the resolution and extent of the datasets required 
##                    for the sponginess model all match
##                    
## ------------ Notes --------------  ##
## There are three datasets this is required for as part of the sponginess model:
## 1: hydrologic soil group (HSG)
## 2: land cover
## 3: slope / DEM
## ------------ ----- --------------  ##
##
## Run after: sponginess_head.R
##
## Run before: cnMapsNonSlopeCreate.R
##
## Specific numbered tasks:
## 1 - check resolution and extents of datasets
## 2 - resample, if required
##
## list of final outputs:
##    data/raw/slope_data.tif <- final slopw data
##    data/raw/lcm_data.tif <- final land cover map
##    data/raw/hsg_data.tif <- final soil group map
##    intermediate_outputs/demSlopeDegrees.tif
##    intermediate_outputs/demSlopePercent.tif
##    intermediate_outputs/HYSOGs2class.tif
##    intermediate_outputs/HYSOGs2classNgb.tif
##    intermediate_outputs/HYSOGs2classResample.tif
##    intermediate_outputs/HYSOGs250mUKpoints.gpkg
##    
## Author: Dr. Paul M. Evans
##
## Date Created: 2024-03-15
##
## Copyright (c) Paul M. Evans, 2024
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13

## ---------------------------
options(scipen = 6, digits = 4) # for non-scientific notation
## ---------------------------

#### 1 - check resolution and extents of datasets ####
# LCM
if(file.exists(finalLcm)){
  cat("saved version of cropped LCM already exists at", 
      finalLcm
      , "...\n")
} else {
  # load in LCM
  lcmIn <- raster(data.lcm)
  ## check resolution is same as base resolution
  stopifnot(xres(lcmIn) == resolution.base)
  
  # crop to common extent, if not that already
  cat("cropping LCM to common extent...\n")
  lcmIn.crop <- raster::crop(lcmIn, commonExtent)
  stopifnot(identical(extent(lcmIn.crop), extent(commonExtent)))
  
  cat("saving common extent LCM...\n")
  # save the crop LC map
  writeRaster(lcmIn.crop
              , finalLcm
              , overwrite = T)
}

# HSG
if(file.exists(finalHsg)){
  cat("saved version of cropped HSG already exists at", 
      finalHsg
      , "...\n")
} else {
  # load in HSG
  hsgIn <- read_stars(file.path(data.hsg))
  hsgIn.espg <- sf::st_crs(hsgIn)$epsg # get current projection (epsg)
  hsgIn.ext <- extent(st_bbox(hsgIn)) # get current extent (epsg)
  
  ## check resolution is same as base resolution
  if(hsgIn.ext != extent(commonExtent)){
    
    # determine the projection - see if it matches project one
    if(hsgIn.espg != project.crs){
      
      # get UK bbox in 27700
      ukbb27700 <- st_bbox(c(xmin = 60625, xmax = 655350, ymax = 1215850, ymin = 7850)
                           , crs = st_crs(27700))
      # get UK bbox in 4326
      ukbb4326 <- ukbb27700 %>%
        st_as_sfc() %>%
        st_transform(crs = hsgIn.espg) %>%
        st_bbox(ukbb)
      
      # crop hydro soils to uk
      hydroInUK <- hsgIn %>% 
        st_crop(ukbb4326) %>%
        # read in as 4326; convert to 27700
        st_transform(27700)
      rm(hsgIn) # tidy, to reduce memory required
      # convert to points
      hydroInUKpoints <- st_as_sf(hydroInUK, as_points = T) 
      
      # # create bbox from common extent
      # ceBbox.OP <- st_bbox(commonExtent)
      # st_crs(ceBbox.OP) <- project.crs
      # 
      # # convert common extent bbox in to HSG projection
      # ceBbox.converted <- ceBbox.OP %>%
      #   st_as_sfc() %>%
      #   st_transform(crs = as.numeric(hsgIn.espg)) %>%
      #   st_bbox()
      # ceBbox.converted
      # 
      # cat("Cropping HSG to common extent...\n")
      # # crop hydro soils to project extent
      # hsgIn.crop <- hsgIn %>% 
      #   st_crop(ceBbox.converted) %>%
      #   # convert to project crs
      #   st_transform(project.crs)
      # rm(hsgIn) # tidy, to reduce memory required
      # 
      # cat("converting HSG to points...\n")
      # # convert to points
      # hsgIn.points <- st_as_sf(hsgIn.crop, as_points = T) 
      
      cat("converting HSG to classes...\n")
      # convert to HSG classes, with A = 1 to D = 4
      hsgIn.points <- hydroInUKpoints %>%
        # convert to HSG classes, with A = 1 to D = 4
        # from Table 1 (https://daac.ornl.gov/SOILS/guides/Global_Hydrologic_Soil_Group.html)
        mutate(HSGclass = if_else(HYSOGs250m.tif == 12, 1
                                  , if_else(HYSOGs250m.tif %in% c(9,11), 2
                                            , if_else(HYSOGs250m.tif %in% c(1:3), 4
                                                      , 3))))
      
      cat("Saving HSG points...\n")
      # save points
      write_sf(hsgIn.points, file.path(intermediatePath, "HYSOGs250mUKpoints.gpkg"))
      st_bbox(hsgIn.points)
      cat("Rasterising HSG points...\n")
      tic("rastised")
      # rasterise
      hydroRast <- st_rasterize(hsgIn.points %>% dplyr::select(HSGclass, geometry)
                                , dx = 250, dy = 250
                                , crs = project.crs)
      toc()
      cat("Saving HSG raster classes...\n")
      write_stars(hydroRast, file.path(intermediatePath, "HYSOGs250mclass.tif"))
      
      cat("Rasterising HSG points...\n")
      tic("rastised2")
      # disaggregate to project resolution
      hydroRast <- as(hydroRast, "Raster")
      xFactor <- xres(hydroRast) / resolution.base # determine factor difference
      hsgRast2 <- disaggregate(as(hydroRast, "Raster"), fact = xFactor)
      cat("Saving HSG raster classes - 2nd resolution...\n")
      writeRaster(hsgRast2, file.path(intermediatePath, "HYSOGs2class.tif")
                  , overwrite = T)
      toc()
      
      cat("Resampling HSG points...\n")
      tic("rast resample")
      # resample, to match lcm
      hsgRast2resamp <- resample(hsgRast2, raster(data.lcm)) %>%
        # crop to common extent
        raster::crop(., commonExtent) 
      cat("Saving HSG resampled - 2nd resolution...\n")
      writeRaster(hsgRast2resamp, file.path(intermediatePath, "HYSOGs2classResample.tif")
                  , overwrite = T)
      
      cat("Resampling HSG points by nearest neighbour...\n")
      hsgRast2ngb <- resample(hsgRast2, raster(data.lcm), method = "ngb") %>%
        # crop to common extent
        raster::crop(., commonExtent) 
      cat("Saving HSG resampled, by nearest neighbour - 2nd resolution...\n")
      writeRaster(hsgRast2ngb, file.path(intermediatePath, "HYSOGs2classNgb.tif")
                  , overwrite = T)
      toc()
      
      ## ------------ Notes --------------  ##
      ## The next bit was specifically for the UK dataset
      
      ## add corrections using the land cover map - this is due to initial noData
      ## cells as lakes /  water bodies were there at the 250 m2
      ## ------------ ----- --------------  ##
      hsgRast2ngb <- rast(file.path(intermediatePath, "HYSOGs2classNgb.tif"))
      
      hsgFix <- terra::focal(hsgRast2ngb
                             , w = 9 # 3 x 3 window
                             , fun = mean
                             , na.policy = "only" # "only" (only for cells that are NA) 
                             , na.rm = T)
      hsgFix2 <- terra::focal(hsgFix
                              , w = 9 # 3 x 3 window
                              , fun = mean
                              , na.policy = "only" # "only" (only for cells that are NA) 
                              , na.rm = T)
      # round - to integer
      hsgFix2[]=as.integer(hsgFix2[])
      
      # use lcm to see which non-water or coastal pixels do not need to be converted
      lcmIn <- rast(data.lcm)
      # remove coastal / littoral
      myFun <- function(x) {ifelse(x %in% c(13:19), 0, 1)}
      Rn <- app(lcmIn, myFun)
      # extent to meet ext
      Rn <- terra::extend(Rn, e)
      Rn <- terra::crop(Rn, e)
      Rn
      # if 1, multiply
      lcmNonWater <- Rn * hsgFix2
      # make 0 NA
      lcmNonWater[lcmNonWater == 0] <- NA
      # save
      stopifnot(xres(lcmNonWater) == resolution.base)
      writeRaster(lcmNonWater
                  , finalHsg
                  , overwrite = T)
    }
  }
}

# Slope (from DEM)
if(file.exists(finalSlope)){
  cat("saved version of cropped DEM already exists at", 
      finalSlope
      , "...\n")
} else {
  # load in DEM
  demIn <- read_stars(file.path(data.slope))
  demIn.espg <- sf::st_crs(demIn)$epsg # get current projection (epsg)
  demIn.ext <- extent(st_bbox(demIn)) # get current extent (epsg)
  demIn.res <- xres(as(demIn, "Raster"))
  demIn <- rast(file.path(data.slope))
  
  cat("Calculating slope...\n")
  # calculate slope from DEM
  slope <- terrain(demIn, v="slope", neighbors=8, unit="degrees")
  # save
  writeRaster(slope, file.path(intermediatePath, "demSlopeDegrees.tif"), overwrite = T)
  # convert slope (in degrees) to Percent
  cat("Calculating slope, in per cent...\n")
  slopePercent <- tan(slope*pi/180)*100
  # see https://www.quora.com/What-is-the-process-to-convert-slope-degrees-to-percent for ref
  # and https://rechneronline.de/winkel/percent.php
  # tan(88*pi/180)*100 # should be 2864
  writeRaster(slopePercent, file.path(intermediatePath, "demSlopePercent.tif"), overwrite = T)
  
  if(demIn.res != resolution.base){
    # resample, to match resolution of analysis
    cat("resampling slope...\n")
    xFactor <- demIn.res / resolution.base # determine factor difference
    slopePercent2 <- disagg(slopePercent, fact = xFactor)
    # save
    cat("writing slope...\n")
    writeRaster(slopePercent2
                , finalSlope
                , overwrite = T)
    # save as the previous df, for the purpose of cropping
    slopePercent <- slopePercent2
  }
  
  cat("Resampling slope, by nearest neighbour...\n")
  slopeRast2ngb <- resample(as(slopePercent, "Raster"), raster(data.lcm), method = "ngb") %>%
    # crop to common extent
    raster::crop(., commonExtent) 
  cat("Saving resampled slope, calculated by nearest neighbour...\n")
  writeRaster(slopeRast2ngb
              , finalSlope
              , overwrite = T)
  toc()
  
  rm(slope)
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
    "\tFinal projection: 27700")
)
, fileConn)
close(fileConn)
