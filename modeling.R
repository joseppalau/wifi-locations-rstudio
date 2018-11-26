library(caret, lattice, ggplot2)
# MODELO PARA SPACEID
# Solo nos quedamos con las columnas de bUILDINGID, latitude y longitude
trainingData_red_para_building <- trainingData_red[,-c(229,228,227,226,225,223)]

#Spliting data into training and testing set
set.seed(123)
training_indices <- createDataPartition(c(1:nrow(trainingData_red_para_building)), p = 0.75, list = FALSE)
trainSet <- trainingData_red_para_building[training_indices, ]
testSet <- trainingData_red_para_building[-training_indices, ]

control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

#cl <- makeCluster(detectCores())
#registerDoParallel(cl)
# Modeling with Support Vector Machine (svm)
svmTrain <- train(BUILDINGID~., data = trainSet, method = "svmLinear", trControl = control, scale = FALSE)
# Modeling with K-Nearest Neighbour (KNN)
knnTrain <- train(BUILDINGID~., data = trainSet, method = "knn", trControl = control)
# modeling with C4.5-like trees
c45Train <- train(BUILDINGID~., data = trainSet, method = "J48", trControl = control)

#stopCluster(cl)

knnPredict<-predict(knnTrain, newdata = testSet)

