#Librerias#
library(raster)
library(caret)
library("klaR")
library(e1071)

#Lectura de datos calicatas
cali <- read.csv("C:/Users/USER/Documents/INDESCES/calic.csv",header = TRUE)
cali.sp <- cali
coordinates(cali.sp) = ~X+Y
proj4string(cali.sp) <- CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
levels(cali$ST) <- make.names(levels(factor(cali$ST)))

#Lectura de covariables
covariables <- readRDS(file = "C:/Users/USER/Documents/INDESCES/Cualitativo/covariables.rds")

#Extraer datos de covariables
cali <- cbind(cali, extract(covariables, cali.sp))

############MODELADO
######################

#Balanceo de clases
set.seed(9560)
up_train <- upSample(x = cali[,5:25],
                     y = cali$ST)
#Data splitting
set.seed(3456)
trainIndex <- createDataPartition(up_train$Class, p = 1, #originalmente era 0.7
                                  list = FALSE, 
                                  times = 1)
Train <- up_train[ trainIndex,]
Test  <- up_train[-trainIndex,]

#Validacion Cruzada
set.seed(25)
setControl <- trainControl(
  method = "repeatedcv",
  number = 6, #usar 64/n = numero de taxas
  repeats=1000,
  verboseIter = FALSE,
  classProbs=TRUE # ,
  # sampling = "up" 
)

#Naive Bayes Parameters
grid <- data.frame(fL=1000, usekernel=TRUE,adjust=1)
set.seed(11)
#Naive Bayes Model
model <- train(Class~.,data=Train,'nb',
               metric=c("Accuracy","Kappa"),
               tuneGrid=grid,
               preProcess=c("center", "scale", "YeoJohnson"),
               trControl=setControl,
               na.action = na.omit
)

#Matriz de confusion
pred.test <- predict(model,Test)
confusionMatrix(data=pred.test,reference=Test$Class)

#Importancia de la variable
vi <- varImp(model,scale=TRUE,sort=TRUE)
plot(vi)
