# MODELO PARA SPACEID
# Se eliminan las columnas de Relative Position y PhoneID porque no contienen datos en el dataset de validación
trainingData_075_r_spaceID <- trainingData_075_r[,-c(ncol(trainingData_075_r)-2,ncol(trainingData_075_r)-3)]

#Spliting data into training and testing set
set.seed(123)
training_indices <- createDataPartition(trainingData_075_r_spaceID$SPACEID, p = 0.75, list = FALSE)
trainSet <- trainingData_075_r_spaceID[training_indices, ]
testSet <- trainingData_075_r_spaceID[-training_indices, ]

control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
# Modeling with Support Vector Machine (svm)
svmTrain <- train(SPACEID~., data = trainSet, method = "svmLinear", trControl = control, scale = FALSE)
# Modeling with K-Nearest Neighbour (KNN)
knnTrain <- train(SPACEID~., data = trainSet, method = "knn", trControl = Control)
# modeling with C4.5-like trees
c45Train <- train(SPACEID~., data = trainSet, method = "J48", trControl = control)

stopCluster(cl)


#prueba github