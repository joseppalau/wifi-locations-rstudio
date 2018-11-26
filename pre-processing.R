library(dplyr, tidyr)


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
#3.1. Reducing in atributes

#3.1.1 (opción 1) código para reducir columnas desde 1 a 520 (puntos wifi) usando la media/mediana - tan solo hay que cambiar FUN 
totalSet <- bind_rows(trainingData,validationData)
medias_075 <- sapply(totalSet[,1:520], mean)
medianas_075 <- sapply(totalSet[,1:520], median)
indices_medias <- which(medias_075 > 95)
length(indices_medias)
trainingData_2 <- trainingData[,-indices_medias]
#finado medias_075 > 95 nos quedamos con 285 columnas 

#3.1.2 (opción 2) código para reducir columnas mediante función apply
my_func <- function(x) {
  c <- length(which(x <= -5 & x>= -60))
  if(c>20){
    return (1)
  }
  else{return(0)}
}

vector_col_1_0 <- apply(totalSet[,1:520],2,my_func)
vector_col_1 <- which(vector_col_1_0 == 1)
vector_col_0 <- which(vector_col_1_0 == 0)
trainingData_2_2 <- trainingData[,-vector_col_0]
# si fijamos que c en la función my_func devuele 1 si es más grande de 20, nos quedamos con 213 columnas. 

#3.1.3. (Opción 3) seria elimnar aquellas columnas que todos los valores son 100, y por lo tanto no aportan nada. Este método ya estaría integrado en la opción 2
valores_min <- apply(totalSet[,1:520],2,min)
valores_min_100 <- which(valores_min == 100)
trainingData_2_3 <- trainingData[,-valores_min_100]

#3.2. Reducing in rows

#primero de todo eliminaremos aquellas filas que tengan todos los valores igual a 100
valores_min_f <- apply(trainingData_2_2[1:nrow(trainingData_2_2),],1,min)
valores_min_f <- as.integer(valores_min_f)
valores_min_100_f <- which(valores_min_f == 100)
trainingData_2_2_f <- trainingData_2_2[-valores_min_100_f,]

#Para estratificar según variables BUILDINGID necesitamos saber la distribución de sus 3 categorías.
building_id_count <- trainingData_2_2_f %>% group_by(BUILDINGID) %>% summarise(count = n())
strata_vector <- createDataPartition(trainingData_2_2_f$BUILDINGID, p = 0.75, list = FALSE)
trainingData_red <- trainingData_2_2_f[strata_vector,]

#miramos cuál es el valor mínimo del totalset
min(apply(totalSet[1:520],2,min))
#el valor obtenido es -104. Por lo tanto, los valores 100 deben ser cambiados a -105
trainingData_red[,1:220][trainingData_red[,1:220] == 100] <- -105

