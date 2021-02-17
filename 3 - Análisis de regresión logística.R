datos = read.table("maestria.txt", header = TRUE)

modelo_lineal = lm(Interes~Edad, data = datos)
predict(modelo_lineal, data.frame(Edad=c(25,35,45,55)))

datos$Interes = as.factor(datos$Interes)
modelo_logistico = glm(Interes~Edad, data = datos, family = "binomial")
summary(modelo_logistico)
coef(modelo_logistico)

modelo_nulo = glm(Interes~1, data = datos, family = "binomial")
library(lmtest)
TestRV = lrtest(modelo_nulo,modelo_logistico)
TestRV
TestRV = lrtest(modelo_logistico,modelo_nulo)
TestRV

anova(modelo_logistico, test = "Chisq") # Equivalente solo para regresión logística simple

TestRV$LogLik
1-TestRV$LogLik[1]/TestRV$LogLik[2]

predict(modelo_logistico, data.frame(Edad=25))
5.77413 - 0.21906*25

predict(modelo_logistico, data.frame(Edad=25), type="response")
exp(5.77413 - 0.21906*25)/(1+exp(5.77413 - 0.21906*25))

predict(modelo_logistico, data.frame(Edad=53))
5.77413 - 0.21906*53

predict(modelo_logistico, data.frame(Edad=53),type="response")
exp(5.77413 - 0.21906*53)/(1+exp(5.77413 - 0.21906*53))

predicciones = predict(modelo_logistico, type="response")
comparacion  = data.frame(OBS  = datos$Interes,
                          PRED = as.factor(round(predicciones,0)))

library(caret)
confusionMatrix(comparacion$PRED, comparacion$OBS,  positive = "1")
library(pROC)
rocobj = roc(predicciones, comparacion$OBS, ci = TRUE)
plot(rocobj)
plot.roc(rocobj,
         legacy.axes = F, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = F,
         max.auc.polygon = F, 
         col  = "blue", 
         grid = T )

data.frame(datos$Interes, predicciones, predicciones>=0.098)
table(datos$Interes, predicciones>=0.098)
