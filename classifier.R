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


############### WORKING WITH SENTINEL2 DATASET 10m RESOLUTION  ##########################################
#B2, B3, B4, B8 at 10m resolution
#B1, B5, B6, B7, B8A, B11, B12 at 20m resolution
#B9, B10 at 60m resolution


#Path to tiff files
data_path='C:/Users/carlo/Desktop/tesi/alto_adige/sentinel2/sentinel_sx/GRANULE/L2A_T32TPS_A042919_20230910T101420/IMG_DATA/R10m'
#Read the raster bands: B2, B3, B4, B8:
sentinel <- list.files(paste0(data_path), pattern = ".*B.*.jp2", full.names = TRUE)
#list.files() Lists all files matching the pattern in the directory.
#the dot means that you ONLY pick those specific names
rst_lst <- lapply(sentinel, FUN = raster)

bands_names <- c("B02","B03","B04","B08")
names(rst_lst) <- bands_names


#Visualize the image in Natural Color (R = Red, G = Green, B = Blue).
#suppressWarnings({viewRGB(brick(rst_lst[1:3]), r = 3, g = 2, b = 1)})

#Combine the raster layers into a single brick for further processing
brick_for_prediction <- brick(rst_lst)



#####WORKING ON AREA A
#importing the shp file of area A
poly_area_A <-shapefile('C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/poly_training_A32N.shp')
#Need to have a numeric id for each class - converting class labels to numeric IDs
poly_area_A@data$id <- as.integer(factor(poly_area_A@data$id))
#Use of @: extract the contents of a slot in a object with a formal (S4) class structure (object@name) 
#Converts data frame to data table for efficient processing
setDT(poly_area_A@data). 

#only focusing on the area where the polygons are
brick_for_prediction <-crop(brick_for_prediction, poly_area_A)

#Creating 750 random points for each id
ptsamp1<-subset(poly_area_A, id == "1") #seleziono solo i poigoni con id=1
ptsamp1_1 <- spsample(ptsamp1, 750, type='regular') # lancio 750 punti a caso nei poligoni con id=1, usando regular sampling
#spsample: Generates random points within the polygons
ptsamp1_1$class <- over(ptsamp1_1, ptsamp1)$id
#over: Associates each random point with the class ID.
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


#Taking the infromation of the pixel where the random point landed and saving them in a dataframe
dt1 <- brick_for_prediction %>%    #%>% takes the output of one function and passes it into another function as an argument, read it as 'and then'
  raster::extract(y = ptsamp1_1) %>% 
#extract: Retrieves the raster values at the locations of the random points.
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


#Merging the 5 dataframes in a single dataframe
dt<-rbind(dt1, dt2, dt3, dt4, dt5)
#rbind: Combines data tables by rows
names(dt)[names(dt) == 'id_cls'] <- 'class'
dt<-dt %>% drop_na() #deletes the rows with null values
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))  
#factor: Converts the class column to a factor with meaningful labels


#RANDOM FOREST
#Splitting the data into the training dataset and the test dataset
set.seed(321) #Since it's a pseudo random number generator, you need to choose the seed
#set.seed: Sets the seed for reproducibility.

# A stratified random split of the data
idx_train <- createDataPartition(dt$class,    #A series of test/training partitions are created
                                 p = 0.7, # percentage of data as training
                                 list = FALSE) #if it's false then you don't want the result to be a list


dt_train <- dt[idx_train] #takes the index idx_train
dt_test <- dt[-idx_train] #takes the index that is not idx_train

#The training dataset is used for carrying cross-validation and grid search for model tuning. 
#Once the optimal/best parameters were found a final model is fit to the entire training dataset using those findings.
#Creating cross-validation folds (splits the data into n random groups) and then cross validation works on them in parallel

#Why using cross validation? Let's say we have 10 folds. Instead of deciding which of these folds
#to use to estimate the tuning parameter, we try all the different cominations (using a different fold each time)
n_folds <- 10
set.seed(321)
folds <- createFolds(1:nrow(dt_train), k = n_folds)
# Set the seed at each resampling iteration. Useful when running Cross Validation in parallel.
seeds <- vector(mode = "list", length = n_folds + 1) # +1 for the final model. It's an empty vector
for(i in 1:n_folds) seeds[[i]] <- sample.int(1000, n_folds) #It gives you a random sample of n_folds (10) numbers from 1 to 1000
seeds[n_folds + 1] <- sample.int(1000, 1) # seed for the final model


#specifying how the training should be controlled and validated
ctrl <- trainControl(summaryFunction = multiClassSummary,  #This specifies the function used to summarize the performance 
                                                           #of the model during cross-validation. multiClassSummary is used 
                                                           #to handle multi-class classification problems, providing metrics 
                                                           #like accuracy, Kappa, etc.
                     method = "cv", #specifies the resampling method, in this case it's cross validation
                     number = n_folds, #number of folds to be used in cross validation
                     search = "grid", #This specifies that a grid search is used to tune the model parameters. 
                                      #Grid search involves searching over a specified set of hyperparameters.
                     classProbs = TRUE, #should class probabilities be computed for classification models (along with predicted values) in each resampl
                     savePredictions = TRUE,
                     index = folds, #a list with elements for each resampling iteration. 
                                    #Each list element is a vector of integers corresponding to the rows used for training at that iteration.
                     seeds = seeds)#This specifies the seeds for each resampling iteration to ensure reproducibility. 
                                   #Each element in the seeds list is a vector of integers used to set the seed at each resampling iteration.

#This function sets up a grid of tuning parameters for a number of classification and regression routines
model_rf <- caret::train(class ~ . , #This is the formula for the model. 
                                     #It specifies that we want to predict the class variable using all other variables in the dt_train dataset.
                         method = "rf", #stands for Random Forest
                         data = dt_train, 
                         importance = TRUE, #This parameter specifies that the function should calculate the importance of each variable (feature). 
                                            #Variable importance is useful for understanding which features contribute most to the prediction.
                         tuneGrid = data.frame(mtry = c(2, 3, 4, 5, 8)), #table of tuning parameters
                                            #mtry: This is the only tuning parameter for the Random Forest algorithm. 
                                            #It stands for the number of variables (features) randomly selected at each split in the tree
                         trControl = ctrl) #specifies how the training should be controlled and validated
  
#saving the model
saveRDS(model_rf, file = paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/modello/","model_rf_10m","area_A",".rds")) 

#predict: Applies the trained model to the raster data to predict classes.
predict_rf <- raster::predict(object = brick_for_prediction,
                              model = model_rf, type = 'raw')
writeRaster(predict_rf, paste0("C:/Users/carlo/Desktop/tesi/alto_adige/aree_di_studio/area_A/sentinel2/10m/modello/","sentinel_10m_area_A_classification",".tiff"),overwrite=T )


####EVALUATION OF THE MODEL 
## tuning results: optimisation of tuning was done via CV so here you can see the different predictors and how well they performed
#the one with the best performance was the one saved in the final model
plot(model_rf) 

#confusion matrix and statistics: the rows are what the algorithm has predicted, and the columns correspond to the known truth. The diagonal is where the model classified correctly
#sensitivity = correct classification of class i/(correct classification of class i + cases where it was classified as something else but it was actually class i)
#sensitivity: percentage of the i-th class was correctly predicted (true positives)
#specificity = cases were it was correctly classified as something different than the i-th class / (cases were it was correctly classified as something different than the i-th class + cases predicted to be class i but were actually somthing else)
#specificity: percentage of cases that were correctly identified as a different class than i (true negatives)
#accuracy = True negatives + true positives/total
#accuracy: how often is the classfier correct
#kappa statistics: he kappa statistic is used not only to evaluate a single classifier, but also to evaluate classifiers amongst themselves. 
#In addition, it takes into account random chance (agreement with a random classifier), which generally means it is less misleading than simply using accuracy as a metric

test_s10A <- predict(model_rf, newdata = dt_test) #testing the model on the test dataset
class_s10A <- dt_test$class #saving the classes that I will be using later
cm <- confusionMatrix(data = test_s10A, class_s10A)
cm

#preditcor importance: a higher value means that that band was more important in determing the classification
caret::varImp(model_rf)$importance %>%
  as.matrix %>% 
  plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
          width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
#The Gini Index or Impurity measures the probability for a random instance being misclassified when chosen randomly. 
#The lower the Gini Index, the lower the likelihood of misclassification.
#gini = 1 - sum(Pi^2) Where Pi denotes the probability of an element being classified for a distinct class.
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s10B <- predict(model_rf, newdata = dt_test)
class_s10B <- dt_test$class
cm <- confusionMatrix(data = test_s10B , class_s10B)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
  as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s10C <- predict(model_rf, newdata = dt_test)
class_s10C <- dt_test$class
cm <- confusionMatrix(data = test_s10C , class_s10C)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))


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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s10D <- predict(model_rf, newdata = dt_test)
class_s10D <- dt_test$class
cm <- confusionMatrix(data = test_s10D, class_s10D)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
     plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)

#SENTINEL 10m CONFUSION MATRIX
test_s10 <- c(test_s10A, test_s10B, test_s10C, test_s10D)
class_s10 <- c(class_s10A, class_s10B, class_s10C, class_s10D)
cm_s10 <- confusionMatrix(data = test_s10, class_s10)
cm_s10
#accuracy=0.8846


############### WORKING WITH SENTINEL2 DATASET 20m RESOLUTION #################################################
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s20A <- predict(model_rf, newdata = dt_test)
class_s20A <- dt_test$class
cm <- confusionMatrix(data = test_s20A , class_s20A)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
test_s20B <- predict(model_rf, newdata = dt_test)
class_s20B <- dt_test$class
cm <- confusionMatrix(data = test_s20B, class_s20B)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
test_s20C <- predict(model_rf, newdata = dt_test)
class_s20C <- dt_test$class
cm <- confusionMatrix(data = test_s20C , class_s20C)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s20D <- predict(model_rf, newdata = dt_test)
class_s20D <- dt_test$class
cm <- confusionMatrix(data = test_s20D, class_s20D)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
     plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)

#SETINEL 20m ACCURACY
test_s20 <- c(test_s20A, test_s20B, test_s20C, test_s20D)
class_s20 <- c(class_s20A, class_s20B, class_s20C, class_s20D)
cm_s20  <- confusionMatrix(data = test_s20, class_s20)
cm_s20

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
          print(paste0(x, 'is already at 10m of resolution '))
}
     else{
          print(paste0('resampling ',x))
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s10rA <- predict(model_rf, newdata = dt_test)
class_s10rA <- dt_test$class
cm <- confusionMatrix(data = test_s10rA, class_s10rA)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))




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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s10rB <- predict(model_rf, newdata = dt_test)
class_s10rB <- dt_test$class
cm <- confusionMatrix(data = test_s10rB, class_s10rB)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s10rC <- predict(model_rf, newdata = dt_test)
class_s10rC <- dt_test$class
cm <- confusionMatrix(data = test_s10rC, class_s10rC)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_s10rD <- predict(model_rf, newdata = dt_test)
class_s10rD <- dt_test$class
cm <- confusionMatrix(data = test_s10rD, class_s10rD)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
     plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)


#SETINEL 10m resampled ACCURACY
test_s10r <- c(test_s10rA, test_s10rB, test_s10rC, test_s10rD)
class_s10r <- c(class_s10rA, class_s10rB, class_s10rC, class_s10rD)
cm_s10r  <- confusionMatrix(data = test_s10r, class_s10r)
cm_s10r

######################LANDSAT8 DATASET########################
#30m resolution

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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_l30A <- predict(model_rf, newdata = dt_test)
class_l30A <- dt_test$class
cm <- confusionMatrix(data = test_l30A, class_l30A)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
     plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)


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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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


####EVALUATION OF THE MODEL 
plot(model_rf) # tuning results

#confusion matrix and statistics
test_l30B <- predict(model_rf, newdata = dt_test)
class_l30B <- dt_test$class
cm <- confusionMatrix(data = test_l30B, class_l30B)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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


####EVALUATION OF THE MODEL 
plot(model_rf) # tuning results

#confusion matrix and statistics
test_l30C <- predict(model_rf, newdata = dt_test)
class_l30C <- dt_test$class
cm <- confusionMatrix(data = test_l30C, class_l30C)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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


####EVALUATION OF THE MODEL 
plot(model_rf) # tuning results

#confusion matrix and statistics
test_l30D <- predict(model_rf, newdata = dt_test)
class_l30D <- dt_test$class
cm <- confusionMatrix(data = test_l30D, class_l30D)
cm

#preditcor importance
caret::varImp(model_rf)$importance %>%
     as.matrix %>% 
     plot_ly(x = colnames(.), y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)

#LANDSAT 30m ACCURACY
test_l30 <- c(test_l30A, test_l30B, test_l30C, test_l30D)
class_l30 <- c(class_l30A, class_l30B, class_l30C, class_l30D)
cm_l30  <- confusionMatrix(data = test_l30, class_l30)
cm_l30


############### WORKING WITH ENMAP DATASET ##########################################
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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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


####EVALUATION OF THE MODEL 
plot(model_rf) # tuning results

#confusion matrix and statistics
test_e30A <- predict(model_rf, newdata = dt_test)
class_e30A <- dt_test$class
cm <- confusionMatrix(data = test_e30A, class_e30A)
cm


#ordering them by predictor importance across the classes 
vi <- varImp(model_rf)$importance  
vi$max <- apply(vi, 1, max)
vi <- vi[order(-vi$max),]
#selecting only the 20 most important bands
vi20 <- head(vi, 20)

vi20%>%
     as.matrix %>% 
     plot_ly(x = colnames(.)[1:5], y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)




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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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
plot(model_rf) # tuning results

#confusion matrix and statistics
test_e30B <- predict(model_rf, newdata = dt_test)
class_e30B <- dt_test$class
cm <- confusionMatrix(data = test_e30B, class_e30B)
cm


#ordering them by predictor importance across the classes 
vi <- varImp(model_rf)$importance  
vi$max <- apply(vi, 1, max)
vi <- vi[order(-vi$max),]
#selecting only the 20 most important bands
vi20 <- head(vi, 20)

vi20%>%
     as.matrix %>% 
     plot_ly(x = colnames(.)[1:5], y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)




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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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

####EVALUATION OF THE MODEL 
plot(model_rf) # tuning results

#confusion matrix and statistics
test_e30C <- predict(model_rf, newdata = dt_test)
class_e30C <- dt_test$class
cm <- confusionMatrix(data = test_e30C, class_e30C)
cm


#ordering them by predictor importance across the classes 
vi <- varImp(model_rf)$importance  
vi$max <- apply(vi, 1, max)
vi <- vi[order(-vi$max),]
#selecting only the 20 most important bands
vi20 <- head(vi, 20)

vi20%>%
     as.matrix %>% 
     plot_ly(x = colnames(.)[1:5], y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)



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
dt$class <- factor(dt$class, labels=c('forest','urban', 'mountain', 'vaia', 'pasture'))



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

####EVALUATION OF THE MODEL 
plot(model_rf) # tuning results


#confusion matrix and statistics
test_e30D <- predict(model_rf, newdata = dt_test)
class_e30D <- dt_test$class
cm <- confusionMatrix(data = test_e30D, class_e30D)
cm


#ordering them by predictor importance across the classes 
vi <- varImp(model_rf)$importance  
vi$max <- apply(vi, 1, max)
vi <- vi[order(-vi$max),]
#selecting only the 20 most important bands
vi20 <- head(vi, 20)

vi20%>%
     as.matrix %>% 
     plot_ly(x = colnames(.)[1:5], y = rownames(.), z = ., type = "heatmap",
             width = 350, height = 300)

#mean decrease accuracy and mean decrease gini 
randomForest::varImpPlot(model_rf$finalModel)

#ENMAP 30m ACCURACY
test_e30 <- c(test_e30A, test_e30B, test_e30C, test_e30D)
class_e30 <- c(class_e30A, class_e30B, class_e30C, class_e30D)
cm_e30  <- confusionMatrix(data = test_e30, class_e30)
cm_e30

















