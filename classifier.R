#importing packages
library(raster)
library(rgdal)
library(sf)
library(sp)
library(RStoolbox)
library(rasterVis)
library(mapview)
library(data.table)
library(RColorBrewer)
library(plotly)
library(grDevices)
library(caret)
library(randomForest)#mis
library(ranger)
library(MLmetrics)
library(nnet)
library(NeuralNetTools)
library(LiblineaR)
library(data.table)
library(dplyr)
library(stringr)
library(doParallel)
library(snow)
library(parallel)
library(tidyr)
library(maptools)


###############WORKING WITH SENTINEL2 DATASET 10m RESOLUTION
#B2, B3, B4, B8 at 10m resolution
#B5, B6, B7, B8A, B11, B12 at 20m resolution
#B1, B9, B10 at 60m resolution
#Projected resolution: 92 m/px?


#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/unzipped'
#Read the raster bands: B2, B3, B4, B5, B6, B7, B8, B8A, B11 and B12:
sentinel <- c(list.files(paste0(data_path), pattern = ".*B.*[2345678].*tiff", full.names = TRUE),
              'C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/unzipped/B11_(Raw).tiff')
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

brick_for_prediction <- brick(rst_for_prediction)



#####WORKING ON AREA A
#importing the shp file of area A
poly_area_A <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/poly_training_A32N.shp')
poly_area_A@data$id <- as.integer(factor(poly_area_A@data$id))
setDT(poly_area_A@data)

#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_A)


ptsamp1<-subset(poly_area_A, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/punti_random/", file="_ptsamp1_A.rds"))

ptsamp2<-subset(poly_area_A, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/punti_random/", file= "_ptsamp2_A.rds"))

ptsamp3<-subset(poly_area_A, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/punti_random/", file= "_ptsamp3_A.rds"))

ptsamp4<-subset(poly_area_A, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/punti_random/", file= "_ptsamp4_A.rds"))

ptsamp5<-subset(poly_area_A, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/punti_random/", file="_ptsamp5_A.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/modello/","model_rf_10m","area_A",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/modello/","sentinel_10m_area_A_classification",".tiff"),overwrite=T )




####WORKING ON AREA B
#importing the shp file of area B
poly_area_B <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/poly_training_B32N.shp')
poly_area_B@data$id <- as.integer(factor(poly_area_B@data$id))
setDT(poly_area_B@data)


#rewriting brick_for_prediction so that it's not cropped on the area A
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_B)


ptsamp1<-subset(poly_area_B, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m/punti_random/", file="_ptsamp1_B.rds"))

ptsamp2<-subset(poly_area_B, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m/punti_random/", file= "_ptsamp2_B.rds"))

ptsamp3<-subset(poly_area_B, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m/punti_random/", file= "_ptsamp3_B.rds"))

ptsamp4<-subset(poly_area_B, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m/punti_random/", file= "_ptsamp4_B.rds"))

ptsamp5<-subset(poly_area_B, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m/punti_random/", file="_ptsamp5_B.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m/modello/","model_rf_10m","area_B",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m/modello/","sentinel_10m_area_B_classification",".tiff"),overwrite=T )




###àWORKING ON AREA C
#importing the shp file of area C
poly_area_C <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/poly_training_C32N.shp')
poly_area_C@data$id <- as.integer(factor(poly_area_C@data$id))
setDT(poly_area_C@data)

#rewriting brick_for_prediction so that it's not cropped on the area B
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_C)


ptsamp1<-subset(poly_area_C, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m/punti_random/", file="_ptsamp1_C.rds"))

ptsamp2<-subset(poly_area_C, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m/punti_random/", file= "_ptsamp2_C.rds"))

ptsamp3<-subset(poly_area_C, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m/punti_random/", file= "_ptsamp3_C.rds"))

ptsamp4<-subset(poly_area_C, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m/punti_random/", file= "_ptsamp4_C.rds"))

ptsamp5<-subset(poly_area_C, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m/punti_random/", file="_ptsamp5_C.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m/modello/","model_rf_10m","area_C",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m/modello/","sentinel_10m_area_C_classification",".tiff"),overwrite=T )



####WORKING ON AREA D
#importing the shp file of area D
poly_area_D <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/poly_training_D32N.shp')
poly_area_D@data$id <- as.integer(factor(poly_area_D@data$id))
setDT(poly_area_D@data)

#rewriting brick_for_prediction so that it's not cropped on the area C
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_D)


ptsamp1<-subset(poly_area_D, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m/punti_random/", file="_ptsamp1_D.rds"))

ptsamp2<-subset(poly_area_D, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m/punti_random/", file= "_ptsamp2_D.rds"))

ptsamp3<-subset(poly_area_D, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m/punti_random/", file= "_ptsamp3_D.rds"))

ptsamp4<-subset(poly_area_D, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m/punti_random/", file= "_ptsamp4_D.rds"))

ptsamp5<-subset(poly_area_D, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m/punti_random/", file="_ptsamp5_D.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m/modello/","model_rf_10m","area_D",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m/modello/","sentinel_10m_area_D_classification",".tiff"),overwrite=T )

###############WORKING WITH SENTINEL2 DATASET 20m RESOLUTION
#B2, B3, B4, B8 at 10m resolution
#B5, B6, B7, B8A, B11, B12 at 20m resolution
#B1, B9, B10 at 60m resolution
#Projected resolution: 92 m/px?

#Resampling bands so that they all have a 20m resolution
rst_for_prediction <- vector(mode = "list", length = length(rst_lst))
names(rst_for_prediction) <- names(rst_lst)
for (b in c("B02", "B03", "B04", "B08")){
    rst_for_prediction[[b]] <- raster::resample(x = rst_lst[[b]],
                                                y = rst_lst$B05)}

b_20m <- c("B05", "B06", "B07", "B8A", "B11", "B12")
rst_for_prediction[b_20m] <- rst_lst[b_20m]

brick_for_prediction <- brick(rst_for_prediction)

#####WORKING ON AREA A
#importing the shp file of area A
poly_area_A <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/poly_training_A32N.shp')
poly_area_A@data$id <- as.integer(factor(poly_area_A@data$id))
setDT(poly_area_A@data)

#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_A)


ptsamp1<-subset(poly_area_A, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/20m/punti_random/", file="_ptsamp1_A.rds"))

ptsamp2<-subset(poly_area_A, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/20m/punti_random/", file= "_ptsamp2_A.rds"))

ptsamp3<-subset(poly_area_A, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/20m/punti_random/", file= "_ptsamp3_A.rds"))

ptsamp4<-subset(poly_area_A, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/20m/punti_random/", file= "_ptsamp4_A.rds"))

ptsamp5<-subset(poly_area_A, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/20m/punti_random/", file="_ptsamp5_A.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/20m/modello/","model_rf_20m","area_A",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/20m/modello/","sentinel_20m_area_A_classification",".tiff"),overwrite=T )




####WORKING ON AREA B
#importing the shp file of area B
poly_area_B <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/poly_training_B32N.shp')
poly_area_B@data$id <- as.integer(factor(poly_area_B@data$id))
setDT(poly_area_B@data)


#rewriting brick_for_prediction so that it's not cropped on the area A
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_B)


ptsamp1<-subset(poly_area_B, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/20m/punti_random/", file="_ptsamp1_B.rds"))

ptsamp2<-subset(poly_area_B, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/20m/punti_random/", file= "_ptsamp2_B.rds"))

ptsamp3<-subset(poly_area_B, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/20m/punti_random/", file= "_ptsamp3_B.rds"))

ptsamp4<-subset(poly_area_B, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/20m/punti_random/", file= "_ptsamp4_B.rds"))

ptsamp5<-subset(poly_area_B, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/20m/punti_random/", file="_ptsamp5_B.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/20m/modello/","model_rf_20m","area_B",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/20m/modello/","sentinel_20m_area_B_classification",".tiff"),overwrite=T )




####WORKING ON AREA C
#importing the shp file of area C
poly_area_C <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/poly_training_C32N.shp')
poly_area_C@data$id <- as.integer(factor(poly_area_C@data$id))
setDT(poly_area_C@data)

#rewriting brick_for_prediction so that it's not cropped on the area B
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_C)


ptsamp1<-subset(poly_area_C, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/20m/punti_random/", file="_ptsamp1_C.rds"))

ptsamp2<-subset(poly_area_C, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/20m/punti_random/", file= "_ptsamp2_C.rds"))

ptsamp3<-subset(poly_area_C, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/20m/punti_random/", file= "_ptsamp3_C.rds"))

ptsamp4<-subset(poly_area_C, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/20m/punti_random/", file= "_ptsamp4_C.rds"))

ptsamp5<-subset(poly_area_C, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/20m/punti_random/", file="_ptsamp5_C.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/20m/modello/","model_rf_20m","area_C",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/20m/modello/","sentinel_20m_area_C_classification",".tiff"),overwrite=T )



####WORKING ON AREA D
#importing the shp file of area D
poly_area_D <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/poly_training_D32N.shp')
poly_area_D@data$id <- as.integer(factor(poly_area_D@data$id))
setDT(poly_area_D@data)

#rewriting brick_for_prediction so that it's not cropped on the area C
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_D)


ptsamp1<-subset(poly_area_D, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/20m/punti_random/", file="_ptsamp1_D.rds"))

ptsamp2<-subset(poly_area_D, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/20m/punti_random/", file= "_ptsamp2_D.rds"))

ptsamp3<-subset(poly_area_D, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/20m/punti_random/", file= "_ptsamp3_D.rds"))

ptsamp4<-subset(poly_area_D, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/20m/punti_random/", file= "_ptsamp4_D.rds"))

ptsamp5<-subset(poly_area_D, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/20m/punti_random/", file="_ptsamp5_D.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/20m/modello/","model_rf_20m","area_D",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/20m/modello/","sentinel_20m_area_D_classification",".tiff"),overwrite=T )



######################LANDSAT8 DATASET########################
#7 bands with a 30m resolution and 1 band (B10) in the IR with a 100 m resolution

#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/landsat'
#Read the raster bands: B1, B2, B3, B4, B5, B6, B7, B10
landsat <- c(list.files(paste0(data_path), pattern = "landsat*.*B", full.names = TRUE))
rst_lst <- lapply(landsat, FUN = raster)

bands_names <- c("B01","B02","B03","B04","B05","B06", "B07", "B10")
names(rst_lst) <- bands_names

#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
suppressWarnings({viewRGB(brick(rst_lst[1:4]), r = 4, g = 3, b = 2)})

brick_for_prediction <- brick(rst_lst)

#####WORKING ON AREA A
#importing the shp file of area A
poly_area_A <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/poly_training_A32N.shp')
poly_area_A@data$id <- as.integer(factor(poly_area_A@data$id))
setDT(poly_area_A@data)

#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_A)


ptsamp1<-subset(poly_area_A, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/landsat8/punti_random/", file="_ptsamp1_A.rds"))

ptsamp2<-subset(poly_area_A, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/landsat8/punti_random/", file= "_ptsamp2_A.rds"))

ptsamp3<-subset(poly_area_A, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/landsat8/punti_random/", file= "_ptsamp3_A.rds"))

ptsamp4<-subset(poly_area_A, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/landsat8/punti_random/", file= "_ptsamp4_A.rds"))

ptsamp5<-subset(poly_area_A, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/landsat8/punti_random/", file="_ptsamp5_A.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na() #drops rows containing missing values
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/landsat8/modello/","model_rf_","area_A",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/landsat8/modello/","landsat_area_A_classification",".tiff"),overwrite=T )




####WORKING ON AREA B
#importing the shp file of area B
poly_area_B <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/poly_training_B32N.shp')
poly_area_B@data$id <- as.integer(factor(poly_area_B@data$id))
setDT(poly_area_B@data)

#rewriting brick_for_prediction so that it's not cropped on the area A
brick_for_prediction <- brick(rst_lst)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_B)


ptsamp1<-subset(poly_area_B, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/landsat8/punti_random/", file="_ptsamp1_B.rds"))

ptsamp2<-subset(poly_area_B, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/landsat8/punti_random/", file= "_ptsamp2_B.rds"))

ptsamp3<-subset(poly_area_B, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/landsat8/punti_random/", file= "_ptsamp3_B.rds"))

ptsamp4<-subset(poly_area_B, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/landsat8/punti_random/", file= "_ptsamp4_B.rds"))

ptsamp5<-subset(poly_area_B, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/landsat8/punti_random/", file="_ptsamp5_B.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/landsat8/modello/","model_rf_","area_B",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/landsat8/modello/","landsat_area_B_classification",".tiff"),overwrite=T )




####WORKING ON AREA C
#importing the shp file of area C
poly_area_C <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/poly_training_C32N.shp')
poly_area_C@data$id <- as.integer(factor(poly_area_C@data$id))
setDT(poly_area_C@data)

#rewriting brick_for_prediction so that it's not cropped on the area B
brick_for_prediction <- brick(rst_lst)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_C)


ptsamp1<-subset(poly_area_C, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/landsat8/punti_random/", file="_ptsamp1_C.rds"))

ptsamp2<-subset(poly_area_C, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/landsat8/punti_random/", file= "_ptsamp2_C.rds"))

ptsamp3<-subset(poly_area_C, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/landsat8/punti_random/", file= "_ptsamp3_C.rds"))

ptsamp4<-subset(poly_area_C, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/landsat8/punti_random/", file= "_ptsamp4_C.rds"))

ptsamp5<-subset(poly_area_C, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/landsat8/punti_random/", file="_ptsamp5_C.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/landsat8/modello/","model_rf_","area_C",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/landsat8/modello/","landsat_area_C_classification",".tiff"),overwrite=T )



####WORKING ON AREA D
#importing the shp file of area D
poly_area_D <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/poly_training_D32N.shp')
poly_area_D@data$id <- as.integer(factor(poly_area_D@data$id))
setDT(poly_area_D@data)

#rewriting brick_for_prediction so that it's not cropped on the area C
brick_for_prediction <- brick(rst_lst)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_D)


ptsamp1<-subset(poly_area_D, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/landsat8/punti_random/", file="_ptsamp1_D.rds"))

ptsamp2<-subset(poly_area_D, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/landsat8/punti_random/", file= "_ptsamp2_D.rds"))

ptsamp3<-subset(poly_area_D, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/landsat8/punti_random/", file= "_ptsamp3_D.rds"))

ptsamp4<-subset(poly_area_D, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/landsat8/punti_random/", file= "_ptsamp4_D.rds"))

ptsamp5<-subset(poly_area_D, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/landsat8/punti_random/", file="_ptsamp5_D.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/landsat8/modello/","model_rf_","area_D",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/landsat8/modello/","landsat_area_D_classification",".tiff"),overwrite=T )



###############WORKING WITH ENMAP DATASET
rst_lst <- stack('C:/Users/carlo/Desktop/tesi/alto_adige/enmap/dataset/ENMAP02/ENMAP01-____L2A-DT0000041009_20230909T102954Z_002_V010303_20230910T054452Z-SPECTRAL_IMAGE.tif')

#library(terra)
#rst_lst <- rast('C:/Users/carlo/Desktop/tesi/alto_adige/enmap/dataset/ENMAP02/ENMAP01-____L2A-DT0000041009_20230909T102954Z_002_V010303_20230910T054452Z-SPECTRAL_IMAGE.tif')
setMinMax(rst_lst)

#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
suppressWarnings({viewRGB(brick(rst_lst[1:44]), r = 44, g = 21, b = 5)})

brick_for_prediction <- brick(rst_lst)



#####WORKING ON AREA A
#importing the shp file of area A
poly_area_A <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/poly_training_A32N.shp')
poly_area_A@data$id <- as.integer(factor(poly_area_A@data$id))
setDT(poly_area_A@data)

#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_A)


ptsamp1<-subset(poly_area_A, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/enmap/punti_random/", file="_ptsamp1_A.rds"))

ptsamp2<-subset(poly_area_A, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/enmap/punti_random/", file= "_ptsamp2_A.rds"))

ptsamp3<-subset(poly_area_A, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/enmap/punti_random/", file= "_ptsamp3_A.rds"))

ptsamp4<-subset(poly_area_A, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/enmap/punti_random/", file= "_ptsamp4_A.rds"))

ptsamp5<-subset(poly_area_A, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/enmap/punti_random/", file="_ptsamp5_A.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/modello/","model_rf_","area_A",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/modello/","sentinel_area_A_classification",".tiff"),overwrite=T )




####WORKING ON AREA B
#importing the shp file of area B
poly_area_B <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/poly_training_B31N.shp')
poly_area_B@data$id <- as.integer(factor(poly_area_B@data$id))
setDT(poly_area_B@data)

#setting the coordinates from m to km so that they match the extent of the sentinel image
poly_area_B <- sp::spTransform(poly_area_B,  sp::CRS("+proj=longlat +datum=WGS84 +units=km +no_defs"))

#rewriting brick_for_prediction so that it's not cropped on the area A
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_B)


ptsamp1<-subset(poly_area_B, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/punti_random/", file="_ptsamp1_B.rds"))

ptsamp2<-subset(poly_area_B, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/punti_random/", file= "_ptsamp2_B.rds"))

ptsamp3<-subset(poly_area_B, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/punti_random/", file= "_ptsamp3_B.rds"))

ptsamp4<-subset(poly_area_B, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/punti_random/", file= "_ptsamp4_B.rds"))

ptsamp5<-subset(poly_area_B, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/punti_random/", file="_ptsamp5_B.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/modello/","model_rf_","area_B",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/modello/","sentinel_area_B_classification",".tiff"),overwrite=T )




###àWORKING ON AREA C
#importing the shp file of area C
poly_area_C <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/poly_training_C31N.shp')
poly_area_C@data$id <- as.integer(factor(poly_area_C@data$id))
setDT(poly_area_C@data)

#setting the coordinates from m to km so that they match the extent of the sentinel image
poly_area_C <- sp::spTransform(poly_area_C,  sp::CRS("+proj=longlat +datum=WGS84 +units=km +no_defs"))

#rewriting brick_for_prediction so that it's not cropped on the area B
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_C)


ptsamp1<-subset(poly_area_C, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/punti_random/", file="_ptsamp1_C.rds"))

ptsamp2<-subset(poly_area_C, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/punti_random/", file= "_ptsamp2_C.rds"))

ptsamp3<-subset(poly_area_C, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/punti_random/", file= "_ptsamp3_C.rds"))

ptsamp4<-subset(poly_area_C, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/punti_random/", file= "_ptsamp4_C.rds"))

ptsamp5<-subset(poly_area_C, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/punti_random/", file="_ptsamp5_C.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/modello/","model_rf_","area_C",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/modello/","sentinel_area_C_classification",".tiff"),overwrite=T )



####WORKING ON AREA D
#importing the shp file of area D
poly_area_D <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/poly_training_D31N.shp')
poly_area_D@data$id <- as.integer(factor(poly_area_D@data$id))
setDT(poly_area_D@data)

#setting the coordinates from m to km so that they match the extent of the sentinel image
poly_area_D <- sp::spTransform(poly_area_D,  sp::CRS("+proj=longlat +datum=WGS84 +units=km +no_defs"))

#rewriting brick_for_prediction so that it's not cropped on the area C
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_D)


ptsamp1<-subset(poly_area_D, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/punti_random/", file="_ptsamp1_D.rds"))

ptsamp2<-subset(poly_area_D, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/punti_random/", file= "_ptsamp2_D.rds"))

ptsamp3<-subset(poly_area_D, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/punti_random/", file= "_ptsamp3_D.rds"))

ptsamp4<-subset(poly_area_D, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/punti_random/", file= "_ptsamp4_D.rds"))

ptsamp5<-subset(poly_area_D, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/punti_random/", file="_ptsamp5_D.rds"))


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


#merge i due dataframe in un unio dataframe. 
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na()
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e'))



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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/modello/","model_rf_","area_D",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/modello/","sentinel_area_D_classification",".tiff"),overwrite=T )



















