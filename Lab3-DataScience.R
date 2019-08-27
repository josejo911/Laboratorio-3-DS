'''
Javier Jo 14343
Marco Flores
'''


#Librerias a utilizar
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(ggplot2)
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

data <- read.csv(file="datosimp.csv", header=TRUE)

View(data)
class(data)

##Merge de variables cuantitativas 
cuantivar <- data[,-1]
cuantivar <- cuantivar[,-1]
View(cuantivar)

res <- cor(cuantivar)
round(res, 2)


##Graficos e Histogramas 

summary(data)
qplot(data$Diesel, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Diesel, main = "GLP", col = 2)
qqline(data$Diesel, col = 3)

qplot(data$DieselLSl, geom="histogram",main = "Histograma de glp") 
qqnorm(data$DieselLS, main = "GLP", col = 2)
qqline(data$DieselLS, col = 3)

qplot(data$DieselULS, geom="histogram",main = "Histograma de glp") 
qqnorm(data$DieselULSl, main = "GLP", col = 2)
qqline(data$DieselULSl, col = 3)

barplot(table(data$Anio),main = "Cantidad segun año")
barplot(table(data$Mes),main = "Cantidad segun mes")

qplot(data$GasAviacion, geom="histogram",main = "Histograma de glp") 
qqnorm(data$GasAviacion, main = "GLP", col = 2)
qqline(data$GasAviacion, col = 3)

qplot(data$GasSuperior, geom="histogram",main = "Histograma de glp") 
qqnorm(data$GasSuperior, main = "GLP", col = 2)
qqline(data$GasSuperior, col = 3)

qplot(data$GasRegular, geom="histogram",main = "Histograma de glp") 
qqnorm(data$GasRegular, main = "GLP", col = 2)
qqline(data$GasRegular, col = 3)

qplot(data$Kerosina, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Kerosina, main = "GLP", col = 2)
qqline(data$Kerosina, col = 3)

qplot(data$rTurboJet, geom="histogram",main = "Histograma de glp") 
qqnorm(data$rTurboJet, main = "GLP", col = 2)
qqline(data$rTurboJet, col = 3)

qplot(data$AceitesLub, geom="histogram",main = "Histograma de glp") 
qqnorm(data$AceitesLub, main = "GLP", col = 2)
qqline(data$AceitesLub,col = 3)

qplot(data$gGrasasLub, geom="histogram",main = "Histograma de glp") 
qqnorm(data$GrasasLub, main = "GLP", col = 2)
qqline(data$GrasasLub, col = 3)

qplot(data$Bunker, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Bunker, main = "GLP", col = 2)
qqline(data$Bunker, col = 3)

qplot(data$Asfalto, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Asfalto, main = "GLP", col = 2)
qqline(data$Asfalto, col = 3)

qplot(data$PetCoke, geom="histogram",main = "Histograma de glp") 
qqnorm(data$PetCoke, main = "GLP", col = 2)
qqline(data$PetCoke, col = 3)

qplot(data$Solventes, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Solventes, main = "GLP", col = 2)
qqline(data$Solventes, col = 3)

qplot(data$Naftas, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Naftas, main = "GLP", col = 2)
qqline(data$Naftas, col = 3)

qplot(data$Ceras, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Ceras, main = "GLP", col = 2)
qqline(data$Ceras, col = 3)

qplot(data$Butano, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Butano, main = "GLP", col = 2)
qqline(data$Butano, col = 3)

qplot(data$PetroleoReconst, geom="histogram",main = "Histograma de glp") 
qqnorm(data$PetroleoReconst, main = "GLP", col = 2)
qqline(data$PetroleoReconst, col = 3)

qplot(data$MTBE, geom="histogram",main = "Histograma de glp") 
qqnorm(data$MTBE, main = "GLP", col = 2)
qqline(data$MTBE, col = 3)

qplot(data$Orimulsion, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Orimulsion, main = "GLP", col = 2)
qqline(data$Orimulsion, col = 3)

qplot(data$MezclasOleosas, geom="histogram",main = "Histograma de glp") 
qqnorm(data$MezclasOleosas, main = "GLP", col = 2)
qqline(data$MezclasOleosas, col = 3)

qplot(data$Total, geom="histogram",main = "Histograma de glp") 
qqnorm(data$Total, main = "GLP", col = 2)
qqline(data$Total, col = 3)


'''
Matriz de Correlacion 

Instalar paquete Hmisc
install.packages("Hmisc")
'''

library("Hmisc")
res2 <- rcorr(as.matrix(cuantivar))
res2

'''
Se extraen los coeficientes de correlacion y los valores de P
'''
res2$r
res2$P

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(res2$r, res2$P)

install.packages("corrplot")
library(corrplot)
corrplot(res, type = "upper",
         tl.col = "black", tl.srt = 45)

##ts

##Serie de tiempo para gasolina superior
superior <- ts(data$GasSuperior, start=c(2001, 1), end=c(2019, 6), frequency=12) 

##analisis gasolina superior
#Saber cuando empieza la serie y cuando termina
start(superior)
end(superior)
#Saber la frecuencia de la serie
frequency(superior)

plot(superior)
abline(reg=lm(superior~time(superior)), col=c("red"))


plot(aggregate(superior,FUN=mean))
dec.Sup<-decompose(superior)
plot(dec.Sup)
plot(dec.Sup$seasonal)


#Aplicaremos una transformaciÃ³n logarÃ?tmica
logSup <- log(superior)
plot(decompose(logSup))


#Ver el grÃ¡fico de la serie
plot(logSup)


#Para saber si hay raÃ?ces unitarias
adfTest(logSup)
adfTest(diff(logSup))
#GrÃ¡fico de autocorrelaciÃ³n
acf(logSup)
# funciones de autocorrelaciÃ³n y autocorrelaciÃ³n parcial
acf(diff(logSup),12)
pacf(diff(logSup))

# Hacer el modelo

auto.arima(superior)

fit <- arima(log(superior), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(superior,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(superior), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP)



##------------------------------------------------------------------
##Serie de tiempo para gasolina regular
regular <- ts(data$GasRegular, start=c(2001, 1), end=c(2019, 6), frequency=12) 
plot(regular)

# loading packages
library(forecast)
library(Metrics)
# splitting data into train and valid sets
train = regular[1:100]
valid = regular[101:nrow(data)]
model = auto.arima(train)



# model summary
summary(model)

# forecasting
forecast = predict(model,44)
plot(forecast)
# evaluation
rmse(valid$International.airline.passengers, forecast$pred)
##------------------------------------------------------------------
#Uniendo diesel y diesel ls 
library(dplyr)
data$newDiesel<- data %>% mutate(newDiesel = coalesce(Diesel,DieselLS)) %>%
  select( newDiesel)

##Serie de tiempo para gasolina diesel
diesel <- ts(data$newDiesel, start=c(2001, 1), end=c(2019, 6), frequency=12) 
plot(diesel)

##analisis gasolina superior
#Saber cuando empieza la serie y cuando termina
start(diesel)
end(diesel)
#Saber la frecuencia de la serie
frequency(diesel)

plot(diesel)
abline(reg=lm(diesel~time(diesel)), col=c("red"))


plot(aggregate(diesel,FUN=mean))
dec.Dis<-decompose(diesel)
plot(dec.Dis)
plot(dec.Dis$seasonal)
##no presenta estacionalidad¿? varia practicamente igual siempre. 
##si presenta tendencia, va en aumento la importacion 

#Aplicaremos una transformaciÃ³n logarÃ?tmica
logdis <- log(diesel)
plot(decompose(logdis))


#Ver el grÃ¡fico de la serie
plot(logdis)


#Para saber si hay raÃ?ces unitarias
adfTest(logdis)
adfTest(diff(logdis))
#GrÃ¡fico de autocorrelaciÃ³n
acf(logdis)
# funciones de autocorrelaciÃ³n y autocorrelaciÃ³n parcial
acf(diff(logdis),12)
pacf(diff(logdis))

# Hacer el modelo

auto.arima(diesel)

fit <- arima(log(diesel), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(superior,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(diesel), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP)
coeftest(fit)
library(lmtest)

Box.test(resid(fit), lag = 1, type = c("Ljung-Box"), fitdf = 0)

acf(fit$residuals)

Box.test(resid(fit), lag = 1, type = c("Ljung-Box"), fitdf = 0)
#vs 2

data$Diesel <- na.replace(data$Diesel,0)
data$DieselLS <- na.replace(data$DieselLS,0)
data$DieselULS <- na.replace(data$DieselULS,0)
data$Diesel <- data$Diesel+data$DieselLS+data$DieselULS
data <- within(data,rm("DieselLS", "DieselULS"))

descdist(data$Diesel)
years <- table(data$Anio)
seriedetiempo <- ts(data$Diesel, start=c(2001, 1), end=c(2019, 6), frequency=12) 
plot(seriedetiempo)
abline(reg=lm(seriedetiempo~time(seriedetiempo)), col=c("red"))
plot(aggregate(seriedetiempo,FUN=mean))
dec.diesel<-decompose(seriedetiempo)
plot(dec.diesel)
plot(dec.diesel$seasonal)
#transformacion logaritmica
tiempodelogaritmo <- log(seriedetiempo)
plot(decompose(tiempodelogaritmo))
#raices unitarias
adfTest(tiempodelogaritmo)
adfTest(diff(tiempodelogaritmo))
#autocorrelacion
acf(tiempodelogaritmo)
auto.arima(seriedetiempo)
fit <- arima(log(seriedetiempo), c(3, 1, 1),seasonal = list(order = c(1, 0, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(seriedetiempo,2.718^pred$pred, log = "y", lty = c(1,3))
fit2 <- arima(log(seriedetiempo), c(3, 1, 1),seasonal = list(order = c(3, 1, 1), period = 12))
forecastAP <- forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP)
#modelo 1:comprobando independencia de residuos y suma de cuadrados modelo 1
Box.test(resid(fit), lag = 1, type = c("Ljung-Box"), fitdf = 0)
#modelo 2:comprobando independencia de residuos y suma de cuadrados 
Box.test(resid(fit2), lag = 1, type = c("Ljung-Box"), fitdf = 0)
e <- tsCV(seriedetiempo, forecastfunction = naive,h=1)
plot(e)
gasolina<- ts(data$GasRegular, start=c(2001, 1), end=c(2018, 12), frequency=12) 
plot(gasolina)
abline(reg=lm(gasolina~time(gasolina)), col=c("red"))
plot(aggregate(gasolina,FUN=mean))
dec.reg<-decompose(gasolina)
plot(dec.reg)
plot(dec.diesel$seasonal)
#transformacion logaritmica
logreg <- log(gasolina)
plot(decompose(logreg))
#raices unitarias
adfTest(logreg)
adfTest(diff(logreg))
#autocorrelacion
acf(logreg)
pacf(logreg)
auto.arima(gasolina)
fitreg <- arima(log(gasolina), c(3, 1, 1),seasonal = list(order = c(1, 0, 1), period = 12))
predreg <- predict(fitreg, n.ahead = 10*12)
ts.plot(gasolina,2.718^predreg$pred, log = "y", lty = c(1,3))
fit2reg <- arima(log(gasolina), c(1, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))
forecastAPreg <- forecast(fit2reg, level = c(95), h = 120)
autoplot(forecastAPreg)
forecastAPreg
coeftest(fit2reg)
#comparando modelos
#modelo 2
Box.test(resid(fit2reg), lag = 1, type = c("Ljung-Box"), fitdf = 0)
acf(fit2reg$residuals)
fit2reg
accuracy(fit2reg)
# modelo 1
Box.test(resid(fit), lag = 1, type = c("Ljung-Box"), fitdf = 0)
gasolinaSup <- ts(data$GasSuperior, start=c(2001, 1), end=c(2018, 12), frequency=12) 
plot(gasolina)
abline(reg=lm(gasolinaSup~time(gasolinaSup)), col=c("red"))
plot(aggregate(gasolinaSup,FUN=mean))
dec.sup<-decompose(gasolinaSup)
plot(dec.sup)
plot(dec.sup$seasonal)
#transformacion logaritmica
logsup <- log(gasolinaSup)
plot(decompose(logsup))
#raices unitarias
adfTest(logsup)
adfTest(diff(logsup))
acf(logsup)
pacf(logsup)
auto.arima(gasolinaSup)
fitsup <- arima(log(gasolina), c(3, 1, 1),seasonal = list(order = c(1, 0, 1), period = 12))
predsup <- predict(fitsup, n.ahead = 10*12)
ts.plot(gasolinaSup,2.718^predsup$pred, log = "y", lty = c(1,3))
fit2sup <- arima(log(gasolinaSup), c(1, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12))
forecastAPsup <- forecast(fit2sup, level = c(95), h = 120)
autoplot(forecastAPsup)
coeftest(fit2sup)
modelo2 <- fit2sup
modelo1 <- fit
Box.test(resid(modelo1), lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(resid(modelo2), lag = 1, type = c("Ljung-Box"), fitdf = 0)



