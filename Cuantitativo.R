#Librerias#
library(raster)
library(car)
library(rgdal)
library(gstat)

#Lectura de datos
datos <- read.csv("C:/Users/USER/Documents/INDESCES/pH.csv")
datos<-datos[,2:4]

#Lectura de covariables#
startdir <- getwd()
setwd("C:/Users/USER/Documents/INDESCES/covariables")
files <- list.files(pattern="sdat$")
files
stack1 <- list()
for(i in 1:length(files)) {
  stack1[[i]] <- raster(files[i])}
stack1
covariables <- do.call(stack, stack1) ### JO!
setwd(startdir)
covariables

#Transformar en datos espaciales#
datos.sp <- datos
coordinates(datos.sp) = ~X+Y
plot(datos.sp, pch=1,  cex=0.4)

#Extraer covariables a datos#
datos <- cbind(datos, extract(covariables, datos.sp))
datos <- na.omit(datos)
datos.sp <- datos
coordinates(datos.sp) = ~X+Y

#Modelo de regresion
modelo.MLR <- lm(pH_0.15~.-X-Y, datos) 
summary(modelo.MLR)

#Modelo de regresion seleccionando covariables
modelo.MLR.step <- step(modelo.MLR, direction="both")
summary(modelo.MLR.step)

#Prediccion de pH en base a covariables
pH0.15.MLR <- predict(covariables, modelo.MLR.step, progress="text", se.fit=TRUE)
plot(pH0.15.MLR)

#Generacion de residuos del modelo
residuos <- modelo.MLR.step$residuals
residuos <- cbind(datos,residuos)
write.table(residuos, "C:/Users/USER/Documents/INDESCES/residuos_pH0.15.csv")
writeRaster(pH0.15.MLR, "C:/Users/USER/Documents/INDESCES/Resultados/pH0.15.MLR.tif", overwrite=TRUE)

#Generar grilla de interpolacion
halfres <- res(pH0.15.MLR)[1]/2
grilla <- expand.grid(x=seq(from=xmin(pH0.15.MLR)+halfres, to=xmax(pH0.15.MLR)-halfres, by=res(pH0.15.MLR)[1]), y=seq(from=ymin(pH0.15.MLR)+halfres, to=ymax(pH0.15.MLR)-halfres, by=res(pH0.15.MLR)[2]))
coordinates(grilla) <- ~ x+y
gridded(grilla) <- TRUE
extent(grilla) == extent(pH0.15.MLR)

#Calcular Variograma
coordinates(residuos ) = ~X+Y
variog = variogram(residuos~1, residuos, cutoff=5000,width=100)
plot(variog)
#Ajustar parametros del variograma
variog.ajust <- fit.variogram(variog, model=vgm(psill=0.2, model="Gau", range=500, nugget=0), fit.ranges =  F)
variog.ajust
plot(variog, variog.ajust)

#Realizamos la interpolacion
Residuos.krig <- krige(residuos~1, residuos, newdata=grilla, model=variog.ajust,nmax=20)
Residuos.MLR <- raster(Residuos.krig["var1.pred"]) 
Residuos.Var <- raster(Residuos.krig["var1.var"]) 
rm(Residuos.krig)
#Ploteamos
plot(Residuos.MLR, main="Kriging de los residuos")
points(datos.sp, pch=1, cex=0.2)
plot(Residuos.Var)
writeRaster(x=Residuos.MLR, filename="C:/Users/USER/Documents/INDESCES/Resultados/pH_0.5_Residuos.tif",overwrite=TRUE)
writeRaster(x=Residuos.Var, filename="C:/Users/USER/Documents/INDESCES/Resultados/pH_0.5_Varianza.tif",overwrite=TRUE)

#Resultado Final
Resultado <- pH0.15.MLR + Residuos.MLR
error <- qnorm(0.95)*sqrt(Residuos.Var)/sqrt(nrow(datos))
plot(Resultado, main="Predicción por RK")
plot(error, main="Error")