# MODELO PARA SPACEID
# Se eliminan las columnas de Relative Position y PhoneID porque no contienen datos en el dataset de validación
trainingData_red_spaceID <- trainingData_red[,-c(ncol(trainingData_red)-1,ncol(trainingData_red)-3)]

#Spliting data into training and testing set
set.seed(123)
training_indices <- createDataPartition(c(1:nrow(trainingData_red_spaceID)), p = 0.75, list = FALSE)
trainSet <- trainingData_red_spaceID[training_indices, ]
testSet <- trainingData_red_spaceID[-training_indices, ]

control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
# Modeling with Support Vector Machine (svm)
svmTrain <- train(SPACEID~., data = trainSet, method = "svmLinear", trControl = control, scale = FALSE)
# Modeling with K-Nearest Neighbour (KNN)
knnTrain <- train(trainSet$SPACEID~., data = trainSet, method = "knn", trControl = control)
# modeling with C4.5-like trees
c45Train <- train(SPACEID~., data = trainSet, method = "J48", trControl = control)

stopCluster(cl)


