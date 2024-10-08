## ---------------------------
##
## Script name: Q_final_images.R
##
## Purpose of script: to create visual maps of the Q flood output
##                  
## Run after: final_resolution_Q.R
##
## Specific numbered tasks:
## 1 - load tifs of final 1 km outputs
## 2 - create image maps
##
## Author: Dr. Paul M. Evans
##
## Date Created: 2023-06-09
##
## Copyright (c) Paul M. Evans, 2023
## Email: paueva@ceh.ac.uk
## Git: https://github.com/pevans13

## ---------------------------
options(scipen = 6, digits = 4) # for non-scientific notation
## ---------------------------

#### 0 - load libraries ####
## automatic install of packages if they are not installed already
list.of.packages <- c(
  "RColorBrewer", "ggplot2", "stars"
  , "circlize", "pbapply", "raster", "dplyr", "tidyr"
  # , "ComplexHeatmap" # for complex heatmap legend
  , "patchwork" # for multigraphs
  , "sf", "gridExtra", "ggstance", "viridis")
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

# create directories to store images
dir.create(file.path("results", "images"), showWarnings = F, recursive = T)

#### 1 - load tifs of final Q outputs ####
floodTifs <- list.files(file.path("results", "final_results")
                        , pattern = "_Qmm"
                        , full.names = T
                        , recursive = T)
## just medians
floodTifs <- floodTifs[grepl("median", floodTifs)]
cat(floodTifs, sep = "\n")
floodToDo <- floodTifs

# load in all the rasters
floodRasters <- pblapply(floodToDo, raster)
# tidy
rm(floodTifs)

#### 2 - create image maps ####
plot(floodRasters[[1]])
floodRasters[[1]]
floodRasters
floodToDo

for(i in 1:length(floodToDo)){
  # for(i in 1){
  
  ##### 2a - Q (runoff) #####
  # convert to df
  cat("reading raster for flood raster", i, "...\n")
  rastDf <- as.data.frame(floodRasters[[i]], xy = TRUE) %>%
    #--- remove cells with NA for any of the layers ---#
    na.omit() %>%
    # convert name
    rename(Q = 3)
  # check
  head(rastDf)
  rastDf
  
  # get name
  nm <- basename(floodToDo[[i]])
  nmTitle <- gsub("^(.+)_Qmm.*", "\\1", nm)
  
  # use name to get correct precip
  stormAmount <- as.numeric(sub("storm(.+)_Ia.*", "\\1", nmTitle))
  rastDf <- rastDf %>%
    mutate(precip = stormAmount) 
  str(rastDf)
  
  # get quantiles, for the purposes of showing them on the histogram
  rastQuan <- quantile(rastDf$Q)
  
  gHis <- ggplot() +
    geom_histogram(data = rastDf, aes(x = Q), bins = 200, fill = "lightblue", color = "black") +
    theme_bw() + 
    scale_x_continuous(expand = c(0,0), breaks = c(seq(0, stormAmount, 2), stormAmount), limits = c(0, (stormAmount + 1))) +  
    scale_y_continuous(expand = c(0,0), breaks = seq(0, 80000, 10000)) + 
    # Add quantiles
    geom_vline(xintercept = rastQuan[3], linetype = "solid", color = "purple", linewidth = .8) + 
    geom_vline(xintercept = rastQuan[c(2,4)], linetype = "solid", color = "grey", linewidth = 1) + 
    geom_vline(xintercept = rastQuan[c(1,5)], linetype = "dashed", color = "grey", linewidth = 1) + 
    labs(y = bquote("Total land area (km"^{2}~")")
         , x = paste0("Depth of run-off (mm) from a ", stormAmount, " mm storm")) + 
    theme(axis.title = element_text(size = 12)
          , axis.text = element_text(size = 8))
  
  # calculate percentage
  rastDf2 <- rastDf %>%
    mutate(pcRunoff = Q / stormAmount * 100)
  ## get quantiles for per cent, for the purposes of showing them on the histogram
  rastQuan <- quantile(rastDf2$pcRunoff)
  
  gHis2 <- ggplot() +
    geom_histogram(data = rastDf2, aes(x = pcRunoff), bins = 200, fill = "lightblue", color = "black") +
    theme_bw() + 
    scale_x_continuous(expand = c(0,0), breaks = seq(0, 100, 5), limits = c(0, 102)) + 
    scale_y_continuous(expand = c(0,0), breaks = seq(0, 80000, 10000)) + 
    # Add quantiles
    geom_vline(xintercept = rastQuan[3], linetype = "solid", color = "purple", size = .8) + 
    geom_vline(xintercept = rastQuan[c(2,4)], linetype = "solid", color = "grey", size = 1) + 
    geom_vline(xintercept = rastQuan[c(1,5)], linetype = "dashed", color = "grey", size = 1) + 
    labs(y = bquote("Total land area (km"^{2}~")")
         , x = paste0("Per cent of run-off from a ", stormAmount, " mm storm")) + 
    theme(axis.title = element_text(size = 12)
          , axis.text = element_text(size = 8))
  
  # save map
  png(file.path("results", "images"
                , paste0(nmTitle
                         , "_hist.png"))
      , height = 800, width = 1000
      , res = 150)
  grid.arrange(gHis, gHis2)
  dev.off()
  
  # Calculate quantile breaks
  quantiles <- round(quantile(rastDf$Q, probs = c(seq(0, 1, 0.2))
                              , na.rm = T), 2)
  cat(quantiles, sep = "\n")
  
  hist(rastDf$Q)
  # Choose a divergent color palette from RColorBrewer
  # Choose a color-blind friendly palette from viridisLite
  palette <- viridisLite::inferno(11)
  scales::show_col(palette)
  
  # Adjust the 'Q' column values for equal spacing between quantiles on the legend
  # rescale
  rastDf$rescaleQ <- scales::rescale(rastDf$Q)
  str(rastDf)
  head(rastDf)
  
  # divide into two rasters, so that the legend can be split between continuous and 
  # a single higher number
  rastDfHigh <- rastDf %>%
    filter(Q > 20)
  head(rastDfHigh)
  
  ## ------------ Notes --------------  ##
  ## below is work on changing the legend
  ## ------------ ----- --------------  ##
  col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
  # lgd = Legend(col_fun = col_fun, title = "foo")
  # lgd
  
  # create map
  xMap <- ggplot(data = rastDf) +
    geom_raster(aes(x = x, y = y, fill = rescaleQ)) +
    coord_sf(default_crs = sf::st_crs(27700)) +
    scale_fill_gradientn(
      colors = palette,
      labels = quantiles,
      breaks = scales::rescale(quantiles)) +
    theme_void() +
    labs(fill="Depth of runoff (mm)") +
    theme(legend.position = "right") +
    ggtitle(paste("Runoff from", stormAmount, "mm storm"))
  
  # save map
  png(file.path("results", "images", paste0(nmTitle
                                              , ".png"))
      , res = 200
      , width = 1000, height = 1200)
  print(xMap)
  dev.off()
  
  range(log2(rastDf$Q))
  range(log(rastDf$Q))
  range(rastDf$Q)
  range(log10(rastDf$Q))  
  rastDf$Q <- ifelse(rastDf$Q == 0, 0.00001, rastDf$Q)
  rastDf$Q <- ifelse(rastDf$Q < 0.25, 0.25, rastDf$Q)
  
  jstart <- 0.125/2
  for(j in 1:11){
    jstart <- jstart * 2
    print(jstart)
    if(j == 1){
      jend <- jstart
    } else {
      jend <- c(jend, jstart)
    }
  }
  print(jend)
  
  # get squares
  jj <- as.numeric(lapply(seq(1, 75,by = 1), function(x) x * x))
  jjsmall <- as.numeric(lapply(seq(0, 0.9,by = 0.05), function(x) x * x))
  jj
  jjsmall
  ## limit by max storm amount
  jjMax <- jj[which(jj <= stormAmount)]
  jjMax
  ### add small
  jjMax <- c(jjsmall, jjMax)
  jjMax
  ## get the logs of these
  jjLog <- log(jjMax)
  jjLog
  
  brks.labs <- round(seq(0, stormAmount, length.out = 10),0)
  brks.labs
  brks.labsLog <- log(brks.labs)
  brks.labsLog
  
  brks <- scales::trans_breaks("log", function(x) 2^x)(seq(0, stormAmount))
  brks.labs <- seq(0, stormAmount, length.out = length(brks))
  brks
  
  
  # create map
  xLog <- ggplot(data = rastDf) +
    geom_raster(aes(x = x, y = y, fill = Q)) +
    coord_sf(default_crs = sf::st_crs(27700)) +
    scale_fill_viridis_c(option = 'D', trans = "log2"
                         , breaks = jend
                         , labels = gsub("0.25", "≤0.25", jend)
    ) +
    theme_void() +
    labs(fill="Depth of runoff (mm)") +
    theme(legend.position = "right") +
    ggtitle(paste("Runoff from", stormAmount, "mm storm"))
  
  # save map
  png(file.path("results", "images", paste0(nmTitle
                                              , "_log.png"))
      , res = 200
      , width = 1000, height = 1200)
  print(xLog)
  dev.off()
  
  ##### 2b - Q (runoff) capped #####
  # convert to df
  rastDf <- as.data.frame(floodRasters[[i]], xy = TRUE) %>%
    #--- remove cells with NA for any of the layers ---#
    na.omit() %>%
    # convert name
    rename(Q = 3) 
  # check
  head(rastDf)
  landCoversponginess <- rastDf
  
  # Create bins based on user-defined bounds
  bounds <- c(0, 1.5, 3, 4.53, 6.03, 7.54, 10)
  landCoversponginess$bin <- cut(landCoversponginess$Q, breaks = bounds)
  head(landCoversponginess)
  ## make point data
  landCoversponginess <- st_as_sf(landCoversponginess
                                  , coords = c("x", "y"))
  st_crs(landCoversponginess) <- 27700
  head(landCoversponginess)
  
  rasterDf <- st_rasterize(landCoversponginess %>% dplyr::select(Q, geometry)
                           , dx = 1000, dy = 1000
                           , crs = 27700)
  rasterDfBin <- st_rasterize(landCoversponginess %>% dplyr::select(bin, geometry)
                              , dx = 1000, dy = 1000
                              , crs = 27700)
  
  min(landCoversponginess$Q, na.rm = T)
  mean(landCoversponginess$Q, na.rm = T)
  max(landCoversponginess$Q, na.rm = T)
  
  ## slightly changed to include 0 and 1
  fill_values_quantiles <- seq(0.0, 1, length.out = 5)
  fill_values_quantiles
  ## use this for a vector of your quantile breaks for the labels (!)
  quants <- quantile(landCoversponginess$Q, fill_values_quantiles)
  quants
  ## convert every value in your fill to quantiles
  landCoversponginess$ptile_var <- ecdf(landCoversponginess$Q)(landCoversponginess$Q)
  head(landCoversponginess)
  
  rasterDfpp <- st_rasterize(landCoversponginess %>% dplyr::select(ptile_var, geometry)
                             , dx = 1000, dy = 1000
                             , crs = 27700)
  colors <- c("lightblue", "blue", "navyblue", "purple3", "springgreen4")
  colors <- viridis(5)
  scales::show_col(colors)
  
  gg <- ggplot() +
    geom_stars(data = rasterDfpp, aes(x = x, y = y, fill = ptile_var)) +
    scale_fill_gradientn(
      ## use your vectors from above for breaks and labels 
      colours = colors,
      breaks = c(0.01, fill_values_quantiles[2:5]),
      labels = c(0.01, round(quants[2:5], 2))
      , na.value="white"
    ) +
    theme_void() +
    coord_equal() +
    labs(fill = expression(paste("Runoff (mm)"))) +
    ggtitle(paste("Runoff from", stormAmount, "mm storm")) +
    theme(legend.text = element_text(size = 8))
  
  png(file.path("results", "images", paste0(nmTitle, "runoff_cap"
                                              , ".png"))
      , res = 200
      , width = 1000, height = 1200)
  print(gg)
  dev.off()
  
  ##### 2c - get percentage images #####
  rastDf <- rastDf %>%
    mutate(pc = (Q/precip) * 100) %>%
    mutate(pc = ifelse(pc > 100, 100, pc))
  head(rastDf)
  
  # Define your custom blue color palette
  bluePalette <- c("#154360", "#21618C", "#85C1E9", "#AED6F1", "#D6EAF8")
  
  # create map
  yMap <- ggplot(data = rastDf) +
    geom_raster(aes(x = x, y = y, fill = pc)) +
    coord_sf(default_crs = sf::st_crs(27700)) +
    scale_fill_gradientn(
      colors = bluePalette
      , na.value = "white"
    ) +
    theme_void() +
    labs(fill="Per cent runoff (%)") +
    theme(legend.position = "right") +
    ggtitle(nm)
  
  # save map
  png(file.path("results", "images", paste0(nmTitle, "pc"
                                              , ".png"))
      , height = 1000, width = 500)
  print(yMap)
  dev.off()
}

##### 2d - Q panelled graphs for just 25 mm scenario #####
stop("2d")
# convert to df
rastDf <- as.data.frame(floodRasters[[1]], xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit() %>%
  # convert name
  rename(Q = 3)  %>%
  ## make point data
  st_as_sf(.
           , coords = c("x", "y"))
st_crs(rastDf) <- 27700
# check
head(rastDf)
landCoversponginess <- rastDf
hist(landCoversponginess$Q)

rasterDf <- st_rasterize(landCoversponginess %>% dplyr::select(Q, geometry)
                         , dx = 1000, dy = 1000
                         , crs = 27700)
plot(rasterDf)

min(landCoversponginess$Q, na.rm = T)
mean(landCoversponginess$Q, na.rm = T)
max(landCoversponginess$Q, na.rm = T)

## slightly changed to include 0 and 1
fill_values_quantiles <- seq(0.0, 1, length.out = 5)
fill_values_quantiles
## use this for a vector of your quantile breaks for the labels (!)
quants <- quantile(landCoversponginess$Q, fill_values_quantiles)
quants
## convert every value in your fill to quantiles
landCoversponginess$ptile_var <- ecdf(landCoversponginess$Q)(landCoversponginess$Q)
head(landCoversponginess)

rasterDfpp <- st_rasterize(landCoversponginess %>% dplyr::select(ptile_var, geometry)
                           , dx = 1000, dy = 1000
                           , crs = 27700)

# create breaks
brks <- seq(min(rasterDf$Q, na.rm = T) # lowest
            , max(rasterDf$Q, na.rm = T) # highest
            , (max(rasterDf$Q, na.rm = T) - min(rasterDf$Q, na.rm = T)) / 6)
brks

## breaks function
breaksFunc <- function(x, ny){
  brks <- seq(min(x, na.rm = T) # lowest
              , max(x, na.rm = T) # highest
              , (max(x, na.rm = T) - min(x, na.rm = T))/ny)
}
brks.labels <- breaksFunc(rasterDf$Q, 6)
brks.labels
# change last one to above
brks.labels <- c(round(brks.labels[c(1:(length(brks.labels)-1))], 0)
                 , paste0(">", round(brks.labels[(length(brks.labels))], 0)))
brks.labels

plot(rasterDfpp)
colors <- c("lightblue", "blue", "navyblue", "purple3", "springgreen4")
colors <- viridis(5)
scales::show_col(colors)

gg <- ggplot() +
  geom_stars(data = rasterDf, aes(x = x, y = y, fill = Q)) +
  scale_fill_gradientn(
    ## use your vectors from above for breaks and labels 
    colours = colors,
    breaks = brks,
    labels = brks.labels,
    , na.value="white"
  ) +
  theme_void() +
  coord_equal() +
  labs(fill = expression(paste("Runoff (mm)"))) +
  ggtitle(paste("Runoff from", "25", "mm storm")) +
  theme(legend.text = element_text(size = 8))

png(file.path("results", "images", paste0(nmTitle, "runoff_cap"
                                            , ".png"))
    , res = 200
    , width = 1000, height = 1200)
print(gg)
dev.off()

#### 2e - load in 'sponginess' ####
# link to Ia: C:\Users\paueva\OneDrive - UKCEH\flood\results\2024-03-15\final_results\median\IaSlopeLC.tif"
sponginess.Ia <- raster("C:/Users/paueva/OneDrive - UKCEH/flood/results/2024-03-15/final_results/median/IaSlopeLC.tif") %>%
  # convert to df
  as.data.frame(., xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit() %>%
  # convert name
  rename(sponginess.Ia = 3) %>%
  ## make point data
  st_as_sf(.
           , coords = c("x", "y"))
st_crs(sponginess.Ia) <- 27700
# check
head(sponginess.Ia)
range(sponginess.Ia$sponginess.Ia)

sponginess.Ia.rast <- st_rasterize(sponginess.Ia %>% dplyr::select(sponginess.Ia, geometry)
                                   , dx = 1000, dy = 1000
                                   , crs = 27700)
plot(sponginess.Ia.rast)
hist(sponginess.Ia.rast)

sponginess.S <- raster("C:/Users/paueva/OneDrive - UKCEH/flood/results/2024-03-15/final_results/median/S_amc2_slope.tif") %>%
  # convert to df
  as.data.frame(., xy = TRUE) %>%
  #--- remove cells with NA for any of the layers ---#
  na.omit() %>%
  # convert name
  rename(sponginess.S = 3) %>%
  ## make point data
  st_as_sf(.
           , coords = c("x", "y"))
st_crs(sponginess.S) <- 27700
# check
head(sponginess.S)
range(sponginess.S$sponginess.S)

sponginess.S.rast <- st_rasterize(sponginess.S %>% dplyr::select(sponginess.S, geometry)
                                  , dx = 1000, dy = 1000
                                  , crs = 27700)
plot(sponginess.S.rast)
hist(sponginess.S.rast)

## ------------ Notes --------------  ##
## due to a a few few extremely high values, truncate the results of the map
## so that the legend is unstandable, showing the variation in lower values
## ------------ ----- --------------  ##
# stop("final graph")
# create an extra column, which will be capped at the 95 percentile
Top95pc <- quantile(sponginess.S$sponginess.S, prob=c(seq(0., .95, 0.05)), na.rm = T)[20] # 95 percentile
sponginess.S$rescaleValueTop <- sponginess.S$sponginess.S
sponginess.S$rescaleValueTop[sponginess.S$rescaleValueTop > Top95pc] <- Top95pc * 1.01
par(mfrow = c(2,1))
hist(sponginess.S$sponginess.S)
hist(sponginess.S$rescaleValueTop)
# rasterise the capped version
s.rast.cap <- st_rasterize(sponginess.S %>% dplyr::select(rescaleValueTop, geometry)
                           , dx = 1000, dy = 1000
                           , crs = 27700)
plot(s.rast.cap)
par(mfrow = c(1,1))
hist(s.rast.cap)

# create breaks
brks <- seq(min(s.rast.cap$rescaleValueTop, na.rm = T) # lowest
            , Top95pc # highest
            , (Top95pc - min(s.rast.cap$rescaleValueTop, na.rm = T)) / 6)
brks

# create scale breaks
## breaks function
breaksFunc <- function(x, ny){
  brks <- seq(min(x, na.rm = T) # lowest
              , max(x, na.rm = T) # highest
              , (max(x, na.rm = T) - min(x, na.rm = T))/ny)
}
brks.labels <- breaksFunc(s.rast.cap$rescaleValueTop, 6)
brks.labels
# change last one to above
brks.labels <- c(round(brks.labels[c(1:(length(brks.labels)-1))], 2)
                 , paste0(">", round(brks.labels[(length(brks.labels))], 2)))
brks.labels

# set pallette 
custom_palette <- c("brown", "orange", "yellow", "lightblue", "darkblue")

ggSponge <- ggplot() +
  geom_stars(data = s.rast.cap, aes(x = x, y = y, fill = rescaleValueTop)) +
  scale_fill_gradientn(
    colors = custom_palette,
    values = scales::rescale(c(min(s.rast.cap$rescaleValueTop), brks[1], brks[2], brks[3], brks[4], brks[5], brks[6], max(s.rast.cap$rescaleValueTop))),
    breaks = brks,
    labels = brks.labels,
    guide = guide_colorbar(
      reverse = FALSE,
      title.position = "top",
      direction = "vertical"
    )
    , na.value="white"
  ) +
  theme_void() +
  coord_equal() +
  labs(fill = expression(paste("Maximum potential\nretention (mm)"))) +
  ggtitle(paste("Maximum potential retention")) +
  theme(legend.text = element_text(size = 8))

##### 3 - save panelled graphs #####
nested <- (wrap_elements(ggSponge|gg))+
  plot_annotation(tag_levels = 'A') # add figure labels
nested #view multi-panel figure

png(file.path("results", "images", paste0("panel_25_S"
                                            , ".png"))
    , res = 300
    , width = 2500, height = 1600)
print(nested)
dev.off()
