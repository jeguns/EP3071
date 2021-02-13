
# Lectura de datos

datos_empleo = read.csv2("empleo_discriminante2.csv")
datos_empleo$Situacion = as.factor(datos_empleo$Situacion)
datos_empleo
attach(datos_empleo)

# Análisis exploratorio inicial

library(GGally)
ggpairs(datos_empleo)
hist(Edad)
hist(Hijos18)
hist(NumTrab)

# Evaluación de supuestos

attach(datos_empleo)
library(MVN)
mvn(datos_empleo[,-1], mvnTest = "royston")
mvn(datos_empleo[,-1], mvnTest = "mardia")

library(dplyr)
datos_empleo %>% 
  filter(Situacion=="0") %>% 
  select(Edad,Hijos18,NumTrab) %>% 
  cov()

datos_empleo %>% 
  filter(Situacion=="1") %>% 
  select(Edad,Hijos18,NumTrab) %>% 
  cov()

datos_empleo %>% 
  filter(Situacion=="2") %>% 
  select(Edad,Hijos18,NumTrab) %>% 
  cov()

library(biotools)
boxM(datos_empleo[,-1], grouping = datos_empleo[, 1])

# Modelo

library(MASS)
modelo_disc2 = lda(formula = Situacion ~ Edad + Hijos18 + NumTrab, data = datos_empleo)
modelo_disc2
modelo_disc2$prior
modelo_disc2$scaling
plot(modelo_disc2)
plot(modelo_disc2, dimen=1, type="b")

library(klaR)
partimat(Situacion ~ Edad + Hijos18 + NumTrab, data=datos_empleo, method="lda", nplots.hor=3)

# Predicción para datos observados

predicciones_disc2 = predict(object = modelo_disc2)
predicciones_disc2

comparacion  = data.frame(OBS  = Situacion,
                          PRED = predicciones_disc2$class,
                          PROB = predicciones_disc2$posterior) 
comparacion

# Matriz de confusión

library(caret)
confusionMatrix(comparacion$PRED, comparacion$OBS)

# Predicción para datos no observados

predict(modelo_disc2, data.frame(Edad=50,Hijos18=3,NumTrab=3))

(X = data.frame(Edad=c(50,30,26),Hijos18=c(3,0,1),NumTrab=c(1,3,1)))
predict(modelo_disc2, X)



