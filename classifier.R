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


############### WORKING WITH SENTINEL2 DATASET 10m RESOLUTION
#B2, B3, B4, B8 at 10m resolution
#B1, B5, B6, B7, B8A, B11, B12 at 20m resolution
#B9, B10 at 60m resolution


#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_sx/GRANULE/L2A_T32TPS_A042919_20230910T101420/IMG_DATA/R10m'
#Read the raster bands: B2, B3, B4, B8:
sentinel <- list.files(paste0(data_path), pattern = ".*B.*.jp2", full.names = TRUE)
#the dot means that you ONLY pick those specific names
rst_lst <- lapply(sentinel, FUN = raster)

bands_names <- c("B02","B03","B04","B08")
names(rst_lst) <- bands_names


#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
#suppressWarnings({viewRGB(brick(rst_lst[1:3]), r = 3, g = 2, b = 1)})

brick_for_prediction <- brick(rst_lst)



#####WORKING ON AREA A
#importing the shp file of area A
poly_area_A <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/poly_training_A32N.shp')
poly_area_A@data$id <- as.integer(factor(poly_area_A@data$id))
setDT(poly_area_A@data)

#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_A)


ptsamp1<-subset(poly_area_A, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1, usando regular sampling
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
dt1 <- brick_for_prediction %>%    #%>% takes the output of one function and passes it into another function as an argument, read it as 'and then'
  raster::extract(y = ptsamp1_1) %>% 
  as.data.table %>%   #Functions to check if an object is data.table, or coerce it if possible
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
dt$class <- factor(dt$class, labels=c('a','b', 'c', 'd', 'e')) #The function factor is used to encode a vector as a factor 



#inizio random forest
set.seed(321) #Since it's a pseudo random number generator, you need to choose the seed
# A stratified random split of the data
idx_train <- createDataPartition(dt$class,    #A series of test/training partitions are created
                                 p = 0.7, # percentage of data as training
                                 list = FALSE) #if it's false then you don't want the result to be a list


dt_train <- dt[idx_train]
dt_test <- dt[-idx_train]


# create cross-validation folds (splits the data into n random groups)
n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dt_train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running CV in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model. It's an empty vector?
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds) #It gives you a random sample of n_folds (10) numbers from 1 to 1000
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model


ctrl <- trainControl(summaryFunction = multiClassSummary,  #Control the computational nuances of the train function
                     method = "cv",
                     number = n_folds, #number of folds
                     search = "grid", #describing how the tuning parameter grid is determined
                     classProbs = TRUE, #should class probabilities be computed for classification models (along with predicted values) in each resample?
                                       #not implemented for SVM; will just get a warning. 
                     savePredictions = TRUE,
                     index = folds, #a list with elements for each resampling iteration. 
                                    #Each list element is a vector of integers corresponding to the rows used for training at that iteration.
                     seeds = seeds)

#This function sets up a grid of tuning parameters for a number of classification and regression routines
model_rf <- caret::train(class ~ . , method = "rf", data = dt_train, #neural network o vector machine
                                                  importance = TRUE,
                                                  tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)),
                                                  trControl = ctrl)
  
#saving the model
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/modello/","model_rf_10m","area_A",".rds")) 

#predict values based on the input data
predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/modello/","sentinel_10m_area_A_classification",".tiff"),overwrite=T )


####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)


####WORKING ON AREA B
#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_dx/GRANULE/L2A_T32TQS_A042919_20230910T101420/IMG_DATA/R10m'
#Read the raster bands: B2, B3, B4, B8:
sentinel <- list.files(paste0(data_path), pattern = ".*B.*.jp2", full.names = TRUE)
#the dot means that you ONLY pick those specific names
rst_lst <- lapply(sentinel, FUN = raster)

bands_names <- c("B02","B03","B04","B08")
names(rst_lst) <- bands_names

#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
#suppressWarnings({viewRGB(brick(rst_lst[1:3]), r = 3, g = 2, b = 1)})

brick_for_prediction <- brick(rst_lst)


#importing the shp file of area B
poly_area_B <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/poly_training_B32N.shp')
poly_area_B@data$id <- as.integer(factor(poly_area_B@data$id))
setDT(poly_area_B@data)

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


####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)

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


####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)



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


####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)




###############WORKING WITH SENTINEL2 DATASET 20m RESOLUTION
#B2, B3, B4, B8 at 10m resolution
#B1, B5, B6, B7, B8A, B11, B12 at 20m resolution
#B9, B10 at 60m resolution

#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_sx/GRANULE/L2A_T32TPS_A042919_20230910T101420/IMG_DATA/R20m'
#Read the raster bands: B1, B2, B3, B4,B5, B6, B7, B8A, B11, B12:
sentinel <- c(list.files(paste0(data_path), pattern = ".*B.*.jp2", full.names = TRUE),
                       'C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_sx/GRANULE/L2A_T32TPS_A042919_20230910T101420/IMG_DATA/R10m/T32TPS_20230910T100601_B08_10m.jp2')
#the dot means that you ONLY pick those specific names
rst_lst <- lapply(sentinel, FUN = raster)

#Reorganizing the bands in the right order
b_08 <- rst_lst[[11]]
b_08A <- rst_lst[[10]]
b_11 <- rst_lst[[8]]
b_12 <- rst_lst[[9]]

rst_lst[[8]] <- b_08
rst_lst[[9]] <- b_08A
rst_lst[[10]] <- b_11 
rst_lst[[11]] <- b_12

bands_names <- c('B01', "B02","B03","B04",'B05', 'B06', 'B07', "B08","B8A","B11","B12")
names(rst_lst) <- bands_names


#Resampling bands so that they all have a 20m resolution
rst_lst[["B08"]] <- raster::resample(x = rst_lst[["B08"]],
                                                y = rst_lst$B05)


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

####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)



####WORKING ON AREA B
#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_dx/GRANULE/L2A_T32TQS_A042919_20230910T101420/IMG_DATA/R20m'
#Read the raster bands: B1, B2, B3, B4,B5, B6, B7, B8A, B11, B12:
sentinel <- c(list.files(paste0(data_path), pattern = ".*B.*.jp2", full.names = TRUE),
                       'C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_dx/GRANULE/L2A_T32TQS_A042919_20230910T101420/IMG_DATA/R10m/T32TQS_20230910T100601_B08_10m.jp2')
#the dot means that you ONLY pick those specific names
rst_lst <- lapply(sentinel, FUN = raster)

#Reorganizing the bands in the right order
b_08 <- rst_lst[[11]]
b_08A <- rst_lst[[10]]
b_11 <- rst_lst[[8]]
b_12 <- rst_lst[[9]]

rst_lst[[8]] <- b_08
rst_lst[[9]] <- b_08A
rst_lst[[10]] <- b_11 
rst_lst[[11]] <- b_12

bands_names <- c('B01', "B02","B03","B04",'B05', 'B06', 'B07', "B08","B8A","B11","B12")
names(rst_lst) <- bands_names


#Resampling bands so that they all have a 20m resolution
rst_lst[["B08"]] <- raster::resample(x = rst_lst[["B08"]],
                                                y = rst_lst$B05)


brick_for_prediction <- brick(rst_lst)


#importing the shp file of area B
poly_area_B <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/poly_training_B32N.shp')
poly_area_B@data$id <- as.integer(factor(poly_area_B@data$id))
setDT(poly_area_B@data)

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



####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)


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


####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)


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




####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)


###############WORKING WITH SENTINEL2 DATASET 10m RESOLUTION RESAMPLED
#Taking all the bands at 20m of resolution and resampling them at 10
#B2, B3, B4, B8 at 10m resolution
#B1, B5, B6, B7, B8A, B11, B12 at 20m resolution
#B9, B10 at 60m resolution

#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_sx/GRANULE/L2A_T32TPS_A042919_20230910T101420/IMG_DATA/R20m'
#Read the raster bands: B1, B2, B3, B4,B5, B6, B7, B8A, B11, B12 and B8:
sentinel <- c(list.files(paste0(data_path), pattern = ".*B.*.jp2", full.names = TRUE),
                       'C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_sx/GRANULE/L2A_T32TPS_A042919_20230910T101420/IMG_DATA/R10m/T32TPS_20230910T100601_B08_10m.jp2')
#the dot means that you ONLY pick those specific names
rst_lst <- lapply(sentinel, FUN = raster)

#Reorganizing the bands in the right order
b_08 <- rst_lst[[11]]
b_08A <- rst_lst[[10]]
b_11 <- rst_lst[[8]]
b_12 <- rst_lst[[9]]

rst_lst[[8]] <- b_08
rst_lst[[9]] <- b_08A
rst_lst[[10]] <- b_11 
rst_lst[[11]] <- b_12

bands_names <- c('B01', "B02","B03","B04",'B05', 'B06', 'B07', "B08","B8A","B11","B12")
names(rst_lst) <- bands_names

area_A <- shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/area_di_studio_A.shp')

#Resampling bands so that they all have a 10m resolution
rst_for_prediction <- vector(mode = "list", length = length(rst_lst))
names(rst_for_prediction) <- names(rst_lst)
rst_for_prediction[['B08']] <- crop(rst_lst[['B08']],area_A)
for (x in bands_names) {
     if (x == 'B08'){
          cat(x, 'is already at 10m of resolution ')
}
     else{
          cat('resampling ',x)
          print('')
          rst_for_prediction[[x]] <- crop(rst_lst[[x]],area_A)
          rst_for_prediction[[x]] <- raster::resample(x = rst_for_prediction[[x]],
                                                y = rst_for_prediction$B08)
} }



brick_for_prediction <- brick(rst_for_prediction)

#####WORKING ON AREA A
#importing the shp file of area A
poly_area_A <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/poly_training_A32N.shp')
poly_area_A@data$id <- as.integer(factor(poly_area_A@data$id))
setDT(poly_area_A@data)

ptsamp1<-subset(poly_area_A, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m_resample/punti_random/", file="_ptsamp1_A.rds"))

ptsamp2<-subset(poly_area_A, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m_resample/punti_random/", file= "_ptsamp2_A.rds"))

ptsamp3<-subset(poly_area_A, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m_resample/punti_random/", file= "_ptsamp3_A.rds"))

ptsamp4<-subset(poly_area_A, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m_resample/punti_random/", file= "_ptsamp4_A.rds"))

ptsamp5<-subset(poly_area_A, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m_resample/punti_random/", file="_ptsamp5_A.rds"))


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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m_resample/modello/","model_rf_10m_resample","area_A",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m_resample/modello/","sentinel_10m_resample_area_A_classification",".tiff"),overwrite=T )

####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)



####WORKING ON AREA B
#Taking all the bands at 20m of resolution and resampling them at 10
#B2, B3, B4, B8 at 10m resolution
#B1, B5, B6, B7, B8A, B11, B12 at 20m resolution
#B9, B10 at 60m resolution

#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_dx/GRANULE/L2A_T32TQS_A042919_20230910T101420/IMG_DATA/R20m'
#Read the raster bands: B1, B2, B3, B4,B5, B6, B7, B8A, B11, B12 and B8:
sentinel <- c(list.files(paste0(data_path), pattern = ".*B.*.jp2", full.names = TRUE),
                       'C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_dx/GRANULE/L2A_T32TQS_A042919_20230910T101420/IMG_DATA/R10m/T32TQS_20230910T100601_B08_10m.jp2')
#the dot means that you ONLY pick those specific names
rst_lst <- lapply(sentinel, FUN = raster)

#Reorganizing the bands in the right order
b_08 <- rst_lst[[11]]
b_08A <- rst_lst[[10]]
b_11 <- rst_lst[[8]]
b_12 <- rst_lst[[9]]

rst_lst[[8]] <- b_08
rst_lst[[9]] <- b_08A
rst_lst[[10]] <- b_11 
rst_lst[[11]] <- b_12

bands_names <- c('B01', "B02","B03","B04",'B05', 'B06', 'B07', "B08","B8A","B11","B12")
names(rst_lst) <- bands_names

area_B <- shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/area_di_studio_B.shp')

#Resampling bands so that they all have a 10m resolution
rst_for_prediction <- vector(mode = "list", length = length(rst_lst))
names(rst_for_prediction) <- names(rst_lst)
rst_for_prediction[['B08']] <- crop(rst_lst[['B08']],area_B)
for (x in bands_names) {
     if (x == 'B08'){
          print(paste0(x, 'is already at 10m of resolution '))
}
     else{
          print(paste0('resampling ',x))
          rst_for_prediction[[x]] <- crop(rst_lst[[x]],area_B)
          rst_for_prediction[[x]] <- raster::resample(x = rst_for_prediction[[x]],
                                                y = rst_for_prediction$B08)
} }



brick_for_prediction <- brick(rst_for_prediction)



#importing the shp file of area B
poly_area_B <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/poly_training_B32N.shp')
poly_area_B@data$id <- as.integer(factor(poly_area_B@data$id))
setDT(poly_area_B@data)

ptsamp1<-subset(poly_area_B, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m_resample/punti_random/", file="_ptsamp1_B.rds"))

ptsamp2<-subset(poly_area_B, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m_resample/punti_random/", file= "_ptsamp2_B.rds"))

ptsamp3<-subset(poly_area_B, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m_resample/punti_random/", file= "_ptsamp3_B.rds"))

ptsamp4<-subset(poly_area_B, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m_resample/punti_random/", file= "_ptsamp4_B.rds"))

ptsamp5<-subset(poly_area_B, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m_resample/punti_random/", file="_ptsamp5_B.rds"))


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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m_resample/modello/","model_rf_10m_resample","area_B",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/sentinel2/10m_resample/modello/","sentinel_10m_resample_area_B_classification",".tiff"),overwrite=T )



####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)


####WORKING ON AREA C
area_C <- shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/area_di_studio_C.shp')

#Resampling bands so that they all have a 10m resolution
rst_for_prediction <- vector(mode = "list", length = length(rst_lst))
names(rst_for_prediction) <- names(rst_lst)
rst_for_prediction[['B08']] <- crop(rst_lst[['B08']],area_C)
for (x in bands_names) {
     if (x == 'B08'){
          print(paste0(x, 'is already at 10m of resolution '))
}
     else{
          print(paste0('resampling ',x))
          rst_for_prediction[[x]] <- crop(rst_lst[[x]],area_C)
          rst_for_prediction[[x]] <- raster::resample(x = rst_for_prediction[[x]],
                                                y = rst_for_prediction$B08)
} }



brick_for_prediction <- brick(rst_for_prediction)


#importing the shp file of area C
poly_area_C <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/poly_training_C32N.shp')
poly_area_C@data$id <- as.integer(factor(poly_area_C@data$id))
setDT(poly_area_C@data)

ptsamp1<-subset(poly_area_C, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m_resample/punti_random/", file="_ptsamp1_C.rds"))

ptsamp2<-subset(poly_area_C, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m_resample/punti_random/", file= "_ptsamp2_C.rds"))

ptsamp3<-subset(poly_area_C, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m_resample/punti_random/", file= "_ptsamp3_C.rds"))

ptsamp4<-subset(poly_area_C, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m_resample/punti_random/", file= "_ptsamp4_C.rds"))

ptsamp5<-subset(poly_area_C, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m_resample/punti_random/", file="_ptsamp5_C.rds"))


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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m_resample/modello/","model_rf_10m_resample","area_C",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/sentinel2/10m_resample/modello/","sentinel_10m_resample_area_C_classification",".tiff"),overwrite=T )


####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)


####WORKING ON AREA D
area_D <- shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/area_di_studio_D.shp')

#Resampling bands so that they all have a 10m resolution
rst_for_prediction <- vector(mode = "list", length = length(rst_lst))
names(rst_for_prediction) <- names(rst_lst)
rst_for_prediction[['B08']] <- crop(rst_lst[['B08']],area_D)
for (x in bands_names) {
     if (x == 'B08'){
          print(paste0(x, 'is already at 10m of resolution '))
}
     else{
          print(paste0('resampling ',x))
          rst_for_prediction[[x]] <- crop(rst_lst[[x]],area_D)
          rst_for_prediction[[x]] <- raster::resample(x = rst_for_prediction[[x]],
                                                y = rst_for_prediction$B08)
} }



brick_for_prediction <- brick(rst_for_prediction)


#importing the shp file of area D
poly_area_D <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/poly_training_D32N.shp')
poly_area_D@data$id <- as.integer(factor(poly_area_D@data$id))
setDT(poly_area_D@data)

ptsamp1<-subset(poly_area_D, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m_resample/punti_random/", file="_ptsamp1_D.rds"))

ptsamp2<-subset(poly_area_D, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m_resample/punti_random/", file= "_ptsamp2_D.rds"))

ptsamp3<-subset(poly_area_D, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m_resample/punti_random/", file= "_ptsamp3_D.rds"))

ptsamp4<-subset(poly_area_D, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m_resample/punti_random/", file= "_ptsamp4_D.rds"))

ptsamp5<-subset(poly_area_D, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m_resample/punti_random/", file="_ptsamp5_D.rds"))


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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m_resample/modello/","model_rf_10m_resample","area_D",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/sentinel2/10m_resample/modello/","sentinel_10m_resample_area_D_classification",".tiff"),overwrite=T )




####EVALUATION OF THE MODEL 
model_rf$times$everything   # total computation time

plot(model_rf) # tuning results

#confusion matrix and statistics
cm_rf <- confusionMatrix(data = predict(model_rf, newdata = dt_test),
                         dt_test$class)
cm_rf

#preditcor importance
randomForest::importance(model_rf$finalModel) %>% 
  .[, - which(colnames(.) %in% c("MeanDecreaseAccuracy", "MeanDecreaseGini"))] %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)

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
#suppressWarnings({viewRGB(brick(rst_lst[1:4]), r = 4, g = 3, b = 2)})

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


####EVALUATION OF THE MODEL 
x <- cbind(dt_test$class, as.integer(matrix(predict_rf)))

y <- rbind(x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
#colnames(conmat) <- classdf$classnames
#rownames(conmat) <- classdf$classnames

n <- sum(conmat)
# number of correctly classified cases per class
diag <- diag(conmat)
# Overall Accuracy
OA <- sum(diag) / n
OA
#OA -> 0.2075203

rowsums <- apply(conmat, 1, sum)
p <- rowsums / n
# predicted cases per class
colsums <- apply(conmat, 2, sum)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa
#kappa -> 0.005528627

PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc

#  producerAccuracy userAccuracy
#1        0.2073695   0.68077295
#2        0.2177243   0.03845411
#3        0.2066906   0.13196034
#4        0.2078969   0.10584615
#5        0.2043081   0.06526272



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
#224 bands with 30 m resolution
#bands 131, 132, 133, 134, 135 have missing values

#####WORKING ON AREA A
#Importing only the area of interest (A)
rst_lst <- stack('C:/Users/carlo/Desktop/tesi/alto_adige/enmap/enmap_A.tif')
rst_lst <- as.list(rst_lst)   #transforming rasterstack into list
names(rst_lst) <- 1:224

#dropping the columns with missing values
rst_lst <- rst_lst[-c(131:135)]

#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
#suppressWarnings({viewRGB(brick(rst_lst[1:44]), r = 44, g = 21, b = 5)})

brick_for_prediction <- brick(rst_lst)

#importing the shp file of area A
poly_area_A <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/poly_training_A32N.shp')
poly_area_A@data$id <- as.integer(factor(poly_area_A@data$id))
setDT(poly_area_A@data)


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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/enmap/modello/","model_rf_","area_A",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/enmap/modello/","enmap_area_A_classification",".tiff"),overwrite=T )




####WORKING ON AREA B
#Importing only the area of interest (B)
rst_lst <- stack('C:/Users/carlo/Desktop/tesi/alto_adige/enmap/enmap_B.tif')
rst_lst <- as.list(rst_lst)   #transforming rasterstack into list
names(rst_lst) <- 1:224

#dropping the columns with missing values
rst_lst <- rst_lst[-c(131:135)]

#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
#suppressWarnings({viewRGB(brick(rst_lst[1:44]), r = 44, g = 21, b = 5)})

brick_for_prediction <- brick(rst_lst)

#importing the shp file of area B
poly_area_B <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/poly_training_B32N.shp')
poly_area_B@data$id <- as.integer(factor(poly_area_B@data$id))
setDT(poly_area_B@data)

#Cropping since in this case the area is slightly bigger than the polygons'area of interest
brick_for_prediction <-crop(brick_for_prediction, poly_area_B)


ptsamp1<-subset(poly_area_B, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/enmap/punti_random/", file="_ptsamp1_B.rds"))

ptsamp2<-subset(poly_area_B, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/enmap/punti_random/", file= "_ptsamp2_B.rds"))

ptsamp3<-subset(poly_area_B, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/enmap/punti_random/", file= "_ptsamp3_B.rds"))

ptsamp4<-subset(poly_area_B, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/enmap/punti_random/", file= "_ptsamp4_B.rds"))

ptsamp5<-subset(poly_area_B, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/enmap/punti_random/", file="_ptsamp5_B.rds"))


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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/enmap/modello/","model_rf_","area_B",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_B/enmap/modello/","enmap_area_B_classification",".tiff"),overwrite=T )

####EVALUATION OF THE MODEL 
x <- cbind(dt_test$class, as.integer(matrix(predict_rf)))

y <- rbind(x)
y <- data.frame(y)
colnames(y) <- c('observed', 'predicted')
conmat <- table(y)
# change the name of the classes
#colnames(conmat) <- classdf$classnames
#rownames(conmat) <- classdf$classnames

n <- sum(conmat) #number of cases
diag <- diag(conmat) # number of correctly classified cases per class
# Overall Accuracy
OA <- sum(diag) / n
OA
#OA -> 0.1976157

rowsums <- apply(conmat, 1, sum) #sum of all the values of the rows (1 means rows and 2 means columns). It's a value for each column
#So basically every column is the number of times that that specific class was predicted (either correctly or incorrectly)
p <- rowsums / n # predicted cases per class
colsums <- apply(conmat, 2, sum) #every row is the number of times a certain class was observed (either correctly or incorrectly predicted)
q <- colsums / n
expAccuracy <- sum(p*q)
kappa <- (OA - expAccuracy) / (1 - expAccuracy)
kappa
#kappa -> 0.005528627

PA <- diag / colsums
# User accuracy
UA <- diag / rowsums
outAcc <- data.frame(producerAccuracy = PA, userAccuracy = UA)
outAcc

#  producerAccuracy userAccuracy
#1        0.2017083   0.34368878
#2        0.2093458   0.06726727
#3        0.1949458   0.09600000
#4        0.1871921   0.24880952
#5        0.2012293   0.22212389



####WORKING ON AREA C
rst_lst <- stack('C:/Users/carlo/Desktop/tesi/alto_adige/enmap/enmap_C.tif')
rst_lst <- as.list(rst_lst)   #transforming rasterstack into list
names(rst_lst) <- 1:224

#dropping the columns with missing values
rst_lst <- rst_lst[-c(131:135)]

#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
#suppressWarnings({viewRGB(brick(rst_lst[1:44]), r = 44, g = 21, b = 5)})

brick_for_prediction <- brick(rst_lst)

#importing the shp file of area C
poly_area_C <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/poly_training_C32N.shp')
poly_area_C@data$id <- as.integer(factor(poly_area_C@data$id))
setDT(poly_area_C@data)

#setting the coordinates from m to km so that they match the extent of the sentinel image
#poly_area_C <- sp::spTransform(poly_area_C,  sp::CRS("+proj=longlat +datum=WGS84 +units=km +no_defs"))

#rewriting brick_for_prediction so that it's not cropped on the area B
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_C)


ptsamp1<-subset(poly_area_C, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/enmap/punti_random/", file="_ptsamp1_C.rds"))

ptsamp2<-subset(poly_area_C, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/enmap/punti_random/", file= "_ptsamp2_C.rds"))

ptsamp3<-subset(poly_area_C, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/enmap/punti_random/", file= "_ptsamp3_C.rds"))

ptsamp4<-subset(poly_area_C, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/enmap/punti_random/", file= "_ptsamp4_C.rds"))

ptsamp5<-subset(poly_area_C, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/enmap/punti_random/", file="_ptsamp5_C.rds"))


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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/enmap/modello/","model_rf_","area_C",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_C/enmap/modello/","enmap_area_C_classification",".tiff"),overwrite=T )



####WORKING ON AREA D
rst_lst <- stack('C:/Users/carlo/Desktop/tesi/alto_adige/enmap/enmap_D.tif')
rst_lst <- as.list(rst_lst)   #transforming rasterstack into list
names(rst_lst) <- 1:224

#dropping the columns with missing values
rst_lst <- rst_lst[-c(131:135)]

#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
#suppressWarnings({viewRGB(brick(rst_lst[1:44]), r = 44, g = 21, b = 5)})

brick_for_prediction <- brick(rst_lst)

#importing the shp file of area D
poly_area_D <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/poly_training_D32N.shp')
poly_area_D@data$id <- as.integer(factor(poly_area_D@data$id))
setDT(poly_area_D@data)

#setting the coordinates from m to km so that they match the extent of the sentinel image
#poly_area_D <- sp::spTransform(poly_area_D,  sp::CRS("+proj=longlat +datum=WGS84 +units=km +no_defs"))

#rewriting brick_for_prediction so that it's not cropped on the area C
brick_for_prediction <- brick(rst_for_prediction)
#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_D)


ptsamp1<-subset(poly_area_D, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1 
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id #do il valore di id=1 ai punti random
saveRDS(ptsamp1_1, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/enmap/punti_random/", file="_ptsamp1_D.rds"))

ptsamp2<-subset(poly_area_D, id == "2") #seleziono solo i poigoni con id=2
ptsamp2_2 <- spsample(ptsamp2, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=2 
ptsamp2_2$class <- over(ptsamp2_2, ptsamp2)$id #do il valore di id=2 ai punti random
saveRDS(ptsamp2_2, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/enmap/punti_random/", file= "_ptsamp2_D.rds"))

ptsamp3<-subset(poly_area_D, id == "3") #seleziono solo i poigoni con id=3
ptsamp3_3 <- spsample(ptsamp3, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=3 
ptsamp3_3$class <- over(ptsamp3_3, ptsamp3)$id #do il valore di id=3 ai punti random
saveRDS(ptsamp3_3, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/enmap/punti_random/", file= "_ptsamp3_D.rds"))

ptsamp4<-subset(poly_area_D, id == "4") #seleziono solo i poigoni con id=4
ptsamp4_4 <- spsample(ptsamp4, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=4 
ptsamp4_4$class <- over(ptsamp4_4, ptsamp4)$id #do il valore di id=4 ai punti random
saveRDS(ptsamp4_4, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/enmap/punti_random/", file= "_ptsamp4_D.rds"))

ptsamp5<-subset(poly_area_D, id == "5") #seleziono solo i poigoni con id=5
ptsamp5_5 <- spsample(ptsamp5, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=5
ptsamp5_5$class <- over(ptsamp5_5, ptsamp5)$id #do il valore di id=5 ai punti random
saveRDS(ptsamp5_5, file=paste0 ("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/enmap/punti_random/", file="_ptsamp5_D.rds"))


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
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/enmap/modello/","model_rf_","area_D",".rds")) 

predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_D/enmap/modello/","enmap_area_D_classification",".tiff"),overwrite=T )



















