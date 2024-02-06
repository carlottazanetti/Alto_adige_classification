# Packages for spatial data processing & visualization
library(rgdal)
library(gdalUtils)
library(raster)
library(sf)
library(sp)
library(RStoolbox)
library(getSpatialData)
library(rasterVis)
library(mapview)

library(RColorBrewer)
library(plotly)
library(grDevices)

# Machine learning packages
library(caret)
library(randomForest)
library(ranger)
library(MLmetrics)
library(nnet)
library(NeuralNetTools)
library(LiblineaR)

# Packages for general data processing and parallel computation
library(data.table)
library(dplyr)
library(stringr)
library(doParallel)
library(snow)
library(parallel)


###IF THE PACKAGES ARE NOT INSTALLED YET, PLEASE INSTALL THEM USING THE FOLLOWING CODE
# Packages for spatial data processing & visualization
install.packages('rgdal')
devtools::install_github("gearslaboratory/gdalUtils")
install.packages('raster')
install.packages('sf')
install.packages('sp')
install.packages('RStoolbox')
devtools::install_github("16EAGLE/getSpatialData")
install.packages('rasterVis')
install.packages('mapview')

install.packages('RColorBrewer')
install.packages('plotly')
install.packages('grDevices')

# Machine learning packages
install.packages('caret')
install.packages('randomForest')
install.packages('ranger')
install.packages('MLmetrics')
install.packages('nnet')
install.packages('NeuralNetTools')
install.packages('LiblineaR')

# Packages for general data processing and parallel computation
install.packages('data.table')
install.packages('dplyr')
install.packages('stringr')
install.packages('doParallel')
install.packages('snow')
install.packages('parallel')
####

# set the temporary folder for raster package operations
rasterOptions(tmpdir = "./cache/temp")

# Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/tesi_davvero/sentinel2/unzipped'
#Read the raster bands: B2, B3, B4, B5, B6, B7, B8, B8A, B11 and B12:
sentinel <- c(list.files(paste0(data_path), pattern = ".*B.*[2345678].*tiff", full.names = TRUE),
              'C:/Users/carlo/Desktop/tesi/tesi_davvero/sentinel2/unzipped/B11_(Raw).tiff')
rst_lst <- lapply(sentinel, FUN = raster)

#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
suppressWarnings({viewRGB(brick(rst_lst[1:3]), r = 3, g = 2, b = 1)})

#Resampling bands so that they all have a 10m resolution
rst_for_prediction <- vector(mode = "list", length = length(rst_lst))
names(rst_for_prediction) <- names(rst_lst)

for (b in c("B05", "B06", "B07", "B8A", "B11", "B12")){
  beginCluster(n = round(3/4 * detectCores()))
  try(
    rst_for_prediction[[b]] <- raster::resample(x = rst_lst[[b]],
                                                y = rst_lst$B02)
  )
  endCluster()
}

b_10m <- c("B02", "B03", "B04", "B08")
rst_for_prediction[b_10m] <- rst_crop_lst[b_10m]
brick_for_prediction <- brick(rst_for_prediction)









