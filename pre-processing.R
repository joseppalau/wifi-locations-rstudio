#1. CAMBIO TIPO DATOS
#1.1. cambio tipo de datos trainingData
trainingData$FLOOR <- as.factor(trainingData$FLOOR)
trainingData$BUILDINGID <- as.factor(trainingData$BUILDINGID)
trainingData$SPACEID <- as.factor(trainingData$SPACEID)
trainingData$RELATIVEPOSITION <- as.factor(trainingData$RELATIVEPOSITION)
trainingData$USERID <- as.factor(trainingData$USERID)
trainingData$PHONEID <- as.factor(trainingData$PHONEID)
#1.2. cambio tipo de datos validationData
validationData$FLOOR <- as.factor(validationData$FLOOR)
validationData$BUILDINGID <- as.factor(validationData$BUILDINGID)
validationData$SPACEID <- as.factor(validationData$SPACEID)
validationData$RELATIVEPOSITION <- as.factor(validationData$RELATIVEPOSITION)
validationData$USERID <- as.factor(validationData$USERID)
validationData$PHONEID <- as.factor(validationData$PHONEID)

#2. DISTRIBUCIÓN SOBRE CIERTOS ATRIBUTOS
#2.1. Distribución SPACEID
spaceID_count <- trainingData %>% group_by(SPACEID) %>% summarise(count = n())
ggplot(spaceID_count, aes(x = spaceID_count$SPACEID, y = spaceID_count$count)) + geom_col() + ggtitle("Nº SPACEID medidos") + labs(x="SpacesId", y="Count")
summary(spaceID_count)
var(spaceID_count$count)
#2.2. distribución buildings and floors
building_floor_count <- trainingData %>% group_by(BUILDINGID, FLOOR) %>% summarise(count = n())
ggplot(building_floor_count, aes(x = building_floor_count$BUILDINGID, y = building_floor_count$count, fill = building_floor_count$FLOOR)) + 
geom_col(position = "dodge") + ggtitle("Nº veces Buildings & Floors") + labs(x="Buildings & Floors", y = "Count")
building_floor_count
var(building_floor_count$count)  
#2.3. distribución PhoneId
phoneId_count <- trainingData %>% group_by(PHONEID) %>% summarise(count = n())
ggplot(phoneId_count, aes(x=PHONEID, y=phoneId_count$count)) + geom_col() + ggtitle("Nº Veces móviles") + labs(x="Móviles", y="Count")
phoneId_count
var(phoneId_count$count)
#2.4. distribución USERID
userID_count <- trainingData %>% group_by(USERID) %>% summarise(count = n())
ggplot(userID_count, aes(x=USERID, y=userID_count$count)) + geom_col() + ggtitle("Nº Veces usuario") + labs(x="User", y="Count")
userID_count 
var(userID_count$count)

#3. REDUCING TRAININGDATA 
#3.1. Reducing in rows
tdr <- nrow(trainingData)
trainingData_075 <- trainingData[sample(nrow(trainingData), tdr*0.75), ]

#3.2. Reducing in atributes
#3.2.1 código para reducir columnas desde 1 a 520 (puntos wifi) usando la media/mediana - tan solo hay que cambiar FUN 
medias_075 <- sapply(trainingData_075[,1:520], mean)
medianas_075 <- sapply(trainingData_075[,1:520], median)
indices_medias_075 <- which(medias_075 > 95)
length(indices_medias_075)
trainingData_075_r <- trainingData_075[,-indices_medias_075]

#3.2.2. código para reducir puntos wifi aplicando FUN que genere un vector TRUE/FASLE según la proporción de valores de una misma columna 
#que están dentro de un rango dado. x = dataframe, a = rango, b = proporción
minNivelWifi <- function(x,a,b) {
  vLower_a <- c()
  for(i in c(1:520)) {
    countV = 0
    for(j in c(1:nrow(x))){
      if(x[j,i] >= a && x[j,i] <= 0){
        countV =+ 1
      }
    }
    if (countV >= round(nrow(x)*b)){
      append(vLower_a,TRUE)
    }
    else {
      append(vLower_a,FALSE) 
    }
  }
  return(vLower_a)
}

col_trainingData_075 <- minNivelWifi(trainingData,-80,0.25)

#3.2.3 Se genera una función para quedarse con los 10 primeros valores más cercanos a 0. Parámetro x es tabla y q númerode valores a mostrar por fila

soloMejoresPuntos <- function(x,a){
    matriz <- matrix()
    for(row in c(1:nrow(x))) {
      rowVector = c()
      for (col in c(1:ncol(x))) {
        append(rowVector,x[row,col]) 
      }
      row_vector_sort <- (sort(abs(rowVector))) 
      row_vector_a <- row_vector_sort[1:a]
      matriz <- rbind(matriz,row_vector_a)
      rm(rowVector,row_vector_sort,row_vector_a)
    } 
    return(matriz)
}
  





