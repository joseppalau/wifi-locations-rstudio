library(readr)
library(dplyr)
library(ggplot2)

trainingData <- read_csv("~/UBIQUM/MENTOR/Josep Palau/Wifi/trainingData.csv")
View(trainingData)

validationData <- read_csv("~/UBIQUM/MENTOR/Josep Palau/Wifi/validationData.csv")
View(validationData)

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
# Ignacio: Are the SPACEID unique? What do you can see in the histogram of SPACEID? Remember that the SPACEID is not something that you need to
# predict. If you analyze thi you will know why
spaceID_count <- trainingData %>% group_by(SPACEID) %>% summarise(count = n())
ggplot(spaceID_count, aes(x = spaceID_count$SPACEID, y = spaceID_count$count)) + geom_col() + ggtitle("Nº SPACEID medidos") + labs(x="SpacesId", y="Count")
summary(spaceID_count)
var(spaceID_count$count)

# Josep, you didn't get the point. Is the SPACEID unique? In other words, knowing only the SPACEID can you know to which combination of
# building and floor belongs?

#2.2. distribución buildings and floors
# Ignacio: What about the locations of the records inside each/building floor?
building_floor_count <- trainingData %>% group_by(BUILDINGID, FLOOR) %>% summarise(count = n())
ggplot(building_floor_count, aes(x = building_floor_count$BUILDINGID, y = building_floor_count$count, fill = building_floor_count$FLOOR)) + 
geom_col(position = "dodge") + ggtitle("Nº veces Buildings & Floors") + labs(x="Buildings & Floors", y = "Count")
building_floor_count
var(building_floor_count$count)  
# Ignacio: According to your results, what building and/or floor will be easier to predict?
#2.3. distribución PhoneId
# Ignacio: What is telling you the histogram of phoneID? 
phoneId_count <- trainingData %>% group_by(PHONEID) %>% summarise(count = n())
ggplot(phoneId_count, aes(x=PHONEID, y=phoneId_count$count)) + geom_col() + ggtitle("Nº Veces móviles") + labs(x="Móviles", y="Count")
phoneId_count
var(phoneId_count$count)

# Going an step further. Take the role of an student just landed to the campus. Do you know beforehand his/her phoneID? Moreover, his/her
# phoneID will be in the training set? ;)

#2.4. distribución USERID
# Ignacio: The same as in the previous point
userID_count <- trainingData %>% group_by(USERID) %>% summarise(count = n())
ggplot(userID_count, aes(x=USERID, y=userID_count$count)) + geom_col() + ggtitle("Nº Veces usuario") + labs(x="User", y="Count")
userID_count 
var(userID_count$count)

#3. REDUCING TRAININGDATA 
#3.1. Reducing in rows
# Ignacio: Stratified sampling!!!
tdr <- nrow(trainingData)
trainingData_075 <- trainingData[sample(nrow(trainingData), tdr*0.75), ]

#3.2. Reducing in atributes
#3.2.1 código para reducir columnas desde 1 a 520 (puntos wifi) usando la media/mediana o modo - tan solo hay que cambiar FUN 
# Ignacio: Por que haces solo la media de tu trainingData_075 y no del dataset completo?

# Correct! You should do it on the whole dataset. Not only in your split!
medias_075 <- sapply(trainingData_075[,1:520], mean)
medianas_075 <- sapply(trainingData_075[,1:520], median)
modas_075 <- sapply(trainingData_075[,1:520], mo)

# Ignacio: Very good, well done!!
indices_medias_075 <- which(medias_075 == 100)
length(indices_medias_075)
trainingData_075_r <- trainingData_075[,-indices_medias_075]

#3.2.2. código para reducir ountos wifi aplicando FUN que genere un vector TRUE/FASLE según la proporción de valores de una misma columna 
#que están dentro de un rango dado. x = dataframe, a = rango, b = proporción
# Ignacio: Consider using the "apply function, as "for" loops as extremly time consuming, and the apply function reduces a lot the time
#my_func <- function(df,col,a,b) {
#    c <- length(which(df <= a & df>= b))
#    return(c)            
#}
# apply(x,2,my_func,a = -5, b = -60)

#apply(trainingData[,c(1,520)],2,function(x) length(x[x <= -5 & x >= -80]))

minNivelWifi <- function(x,a,b) {
  vLower_a <- c()
  for(i in c(1:ncol(x))) {
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

