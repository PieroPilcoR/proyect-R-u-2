#All the problems we're gonna mention are in the TG2-RML pdf file

#Prob1
text_root <- "~/R-Programs/Proyect-2/datoshelado.txt"
datoshelado <- read.table(text_root, header = TRUE, sep = ",")
datoshelado

#Prob3
# It plots considering each variable from the data
plot(datoshelado)
# It shows a summary of the data
summary(datoshelado)

#Prob4
total_data <- nrow(datoshelado)
# We select the 70% of the data in 'datoshelado', obviously range goes from 1 to 70%
dataAprendizaje <- datoshelado[1:(total_data*0.7),]
dataAprendizaje
# We select the remaining data in 'datoshelado', we put the '+1' 
# because the data will be ranged from 70+1% to 100%
dataTest <- datoshelado[(total_data*0.7+1):(total_data),]
dataTest

#Prob5
# To do a multilinear regression with the variables from 'dataAprendizaje'
modelo1 <- lm(cons ~ income + price + temp, data = dataAprendizaje)
modelo1
summary(modelo1)

#Prob8
SSE <- function(model){
  return(sum(model$residuals^2))
}
BIC_ML <- function(model, data){
  #modelo <- lm(data[[y_num]] ~ ., data=data)
  n <- nrow(data)
  k <- length(data)-1
  return(n*log(SSE(model)/n) + k*log(n))
}


n <- nrow(dataAprendizaje)
# Modelo sin variables independientes
modZero <- lm(cons ~ 1, data = dataAprendizaje)
# Modelo con todas las variables independientes
modFull <- lm(cons ~ . , data = dataAprendizaje)
# Modelo intermedio con algunas variables
modInt <- lm(cons ~ income + price + temp, data = dataAprendizaje)

#Preg9

# Busqueda backward
MASS::stepAIC(modelo1, direction = "backward", k= log(n))

#Preg10
# Busqueda forward
MASS::stepAIC(modZero, direction = "forward",
             scope = list( lower = modZero, upper = modFull),
             k= log(n))
# Busqueda both
modelo2 <- MASS::stepAIC(modInt, direction = "both",
                         scope = list( lower = modZero, upper = modFull),
                         k= log(n))
modelo2

#Preg11
# Predict the values using the 'object' model to the data 'newdata'
predict1 <- predict(object=modelo1, newdata=dataTest)
predict1
predict2 <- predict(object=modelo2, newdata=dataTest)
predict2

#Preg12
MSE <- function(data_original, data_predicted){
  n <- length(data_original)
  return( sum((data_original-data_predicted)^2)/n )
}
MSE1 <- MSE(dataTest$cons, predict1)
MSE1
MSE2 <- MSE(dataTest$cons, predict2)
MSE2
