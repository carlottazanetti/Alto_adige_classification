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
library(tidyr)


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


# Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/tesi_davvero/sentinel2/unzipped'
#Read the raster bands: B2, B3, B4, B5, B6, B7, B8, B8A, B11 and B12:
sentinel <- c(list.files(paste0(data_path), pattern = ".*B.*[2345678].*tiff", full.names = TRUE),
              'C:/Users/carlo/Desktop/tesi/tesi_davvero/sentinel2/unzipped/B11_(Raw).tiff')
rst_lst <- lapply(sentinel, FUN = raster)

#Reorganizing the bands in the right order
b_08A <- rst_lst[[9]]
b_11 <- rst_lst[[10]]
b_12 <- rst_lst[[8]]

bands_names <- c("B02","B03","B04","B05","B06", "B07","B08", "B8A", "B11", "B12")
names(rst_lst) <- bands_names


#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
suppressWarnings({viewRGB(brick(rst_lst[1:3]), r = 3, g = 2, b = 1)})

#Resampling bands so that they all have a 10m resolution
rst_for_prediction <- vector(mode = "list", length = length(rst_lst))
names(rst_for_prediction) <- names(rst_lst)

for (b in c("B05", "B06", "B07", "B8A", "B11", "B12")){
    rst_for_prediction[[b]] <- raster::resample(x = rst_lst[[b]],
                                                y = rst_lst$B02)}

b_10m <- c("B02", "B03", "B04", "B08")
rst_for_prediction[b_10m] <- rst_lst[b_10m]
#se usi stack invece che brick non funziona?
brick_for_prediction <- brick(rst_for_prediction)

#importing the shp file of the woods in area A
poly_boschi_A<-shapefile('C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/boschi_A_31N.shp')
poly_boschi_A@data$id <- as.integer(factor(poly_boschi_A@data$id))
setDT(poly_boschi_A@data)

#setting the coordinates from m to km so that they match the extent of the sentinel image
poly_boschi_A <- sp::spTransform(poly_boschi_A,  sp::CRS("+proj=longlat +datum=WGS84 +units=km +no_defs"))

#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_boschi_A)


ptsamp1<-subset(poly_boschi_A, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file="_ptsamp1_1_boschi_A.rds"))

ptsamp2<-subset(poly_boschi_A, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file= "_ptsamp2_2_boschi_A.rds"))

ptsamp3<-subset(poly_boschi_A, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file= "_ptsamp3_3_boschi_A.rds"))

ptsamp4<-subset(poly_boschi_A, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file= "_ptsamp4_4_boschi_A.rds"))

ptsamp5<-subset(poly_boschi_A, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file="_ptsamp5_5_boschi_A.rds"))

ptsamp6<-subset(poly_boschi_A, id == "6") #seleziono solo i poigoni con id=6
ptsamp6_6 <- spsample(ptsamp6, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=6
ptsamp6_6$class <- over(ptsamp6_6, ptsamp6)$id #do il valore di id=6 ai punti random
saveRDS(ptsamp6_6, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file= "_ptsamp6_6_boschi_A.rds"))

ptsamp7<-subset(poly_boschi_A, id == "7") #seleziono solo i poigoni con id=7
ptsamp7_7 <- spsample(ptsamp7, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=7
ptsamp7_7$class <- over(ptsamp7_7, ptsamp7)$id #do il valore di id=7 ai punti random
saveRDS(ptsamp7_7, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file= "_ptsamp7_7_boschi_A.rds"))

ptsamp8<-subset(poly_boschi_A, id == "8") #seleziono solo i poigoni con id=8
ptsamp8_8 <- spsample(ptsamp8, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=8 
ptsamp8_8$class <- over(ptsamp8_8, ptsamp8)$id #do il valore di id=8 ai punti random
saveRDS(ptsamp8_8, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file= "_ptsamp8_8_boschi_A.rds"))

ptsamp9<-subset(poly_boschi_A, id == "9") #seleziono solo i poigoni con id=9
ptsamp9_9 <- spsample(ptsamp9, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=9
ptsamp9_9$class <- over(ptsamp9_9, ptsamp9)$id #do il valore di id=9 ai punti random
saveRDS(ptsamp9_9, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file= "_ptsamp9_9_boschi_A.rds"))

ptsamp10<-subset(poly_boschi_A, id == "10") #seleziono solo i poigoni con id=10
ptsamp10_10 <- spsample(ptsamp10, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=10
ptsamp10_10$class <- over(ptsamp10_10, ptsamp10)$id #do il valore di id=10 ai punti random
saveRDS(ptsamp10_10, file=paste0 ("C:/Users/carlo/Desktop/tesi/tesi_davvero/aree_di_studio/punti_random/area_A", file= "_ptsamp10_10_boschi_A.rds"))


#in quest parte prendo le informazioni dei pixel dove il punto random è caduto e lo salvo in un dataframe. 
dt1 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp1_1) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp1_1@data] # add the class names to each row

dt2 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp2_2) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp2_2@data]

dt3 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp3_3) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp3_3@data] # add the class names to each row

dt4 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp4_4) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp4_4@data] # add the class names to each row

dt5 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp5_5) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp5_5@data] # add the class names to each row

dt6 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp6_6) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp6_6@data]

dt7 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp7_7) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp7_7@data] # add the class names to each row

dt8 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp8_8) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp8_8@data] # add the class names to each row

dt9 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp9_9) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp9_9@data] # add the class names to each row

dt10 <- brick_for_prediction %>% 
  raster::extract(y = ptsamp10_10) %>% 
  as.data.table %>% 
  .[, id_cls := ptsamp10_10@data] # add the class names to each row


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9, dt10)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))



#inizio random forest
set.seed(321)
# A stratified random split of the data
idx_train <- createDataPartition(dt$class,
                                 p = 0.7, # percentage of data as training
                                 list = FALSE)


dt_train <- dt[idx_train]
dt_test <- dt[-idx_train]


# create cross-validation folds (splits the data into n random groups)
n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dt_train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds)
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model


ctrl <- trainControl(summaryFunction = multiClassSummary,
                     method = "cv",
                     number = n_folds,
                     search = "grid",
                     classProbs = TRUE, # not implemented for SVM; will just get a warning
                     savePredictions = TRUE,
                     index = folds,
                     seeds = seeds)

model_rf <- caret::train(class ~ . , method = "rf", data = dt_train, #neural network o vector machine
                                                  importance = TRUE,
                                                  tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                                                  trControl = ctrl)

#saving the model
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/tesi_davvero/modello","model_rf_","boschi_area_A",".rds")) 









