
# Lectura de datos

datos_empleo = read.csv2("empleo_discriminante.csv")
datos_empleo
datos_empleo$Situacion = as.factor(datos_empleo$Situacion)
attach(datos_empleo)

# Análisis exploratorio inicial

library(GGally)
ggpairs(datos_empleo)
hist(Edad)
hist(Hijos18)

# Evaluación de supuestos

library(nortest)
shapiro.test(Edad)
ad.test(Edad)
shapiro.test(Hijos18)
ad.test(Hijos18)

shapiro.test(Hijos18)
ad.test(Hijos18)

library(MVN)
mvn(datos_empleo[,-1], mvnTest = "royston")
mvn(datos_empleo[,-1], mvnTest = "mardia")

library(dplyr)

datos_empleo %>% 
  filter(Situacion=="0") %>% 
  select(Edad,Hijos18) %>% 
  cov()

datos_empleo %>% 
  filter(Situacion=="1") %>% 
  select(Edad,Hijos18) %>% 
  cov()

library(biotools)
boxM(datos_empleo[,-1], grouping = datos_empleo[, 1])

# Modelo

library(MASS)
modelo_disc = lda(formula = Situacion ~ Edad + Hijos18, data = datos_empleo)
modelo_disc
modelo_disc$prior
modelo_disc$scaling
plot(modelo_disc)
library(klaR)
partimat(Situacion ~ Edad + Hijos18, data=datos_empleo, method="lda", nplots.hor=3)

# Predicción para datos observados

predicciones_disc = predict(object = modelo_disc)
predicciones_disc
predicciones_disc$posterior

comparacion  = data.frame(OBS  = Situacion,
                          PRED = predicciones_disc$class,
                          PROB = predicciones_disc$posterior) 
comparacion

# Matriz de confusión

library(caret)
confusionMatrix(Situacion, predicciones_disc$class, positive = '1')

# Curva ROC

library(pROC)

apply(predicciones_disc$posterior, 1, max)

rocobj = roc(Situacion, apply(predicciones_disc$posterior, 1, max), auc = TRUE, ci = TRUE  )
plot.roc(rocobj, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = FALSE,
         max.auc.polygon = FALSE, 
         col  = "darkblue", 
         grid = TRUE )


# Predicción para datos no observados

predict(modelo_disc, data.frame(Edad=50,Hijos18=3))

(X = data.frame(Edad=c(50,30,26),Hijos18=c(3,0,1)))
predict(modelo_disc, X)

# Comparando con un modelo de regresión logística


modelo_logistico   = glm(Situacion ~ Edad + Hijos18, data = datos_empleo, family = binomial)
predicciones_logis = predict(modelo_logistico, type=c("response"))

confusionMatrix(Situacion, as.factor(round(predicciones_logis,0)), positive = "1")

rocobj = roc(Situacion, predicciones_logis, auc = TRUE, ci = TRUE)
plot.roc(rocobj, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = FALSE,
         max.auc.polygon = FALSE, 
         col  = "darkblue", 
         grid = TRUE )

