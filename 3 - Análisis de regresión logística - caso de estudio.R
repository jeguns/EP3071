library(readxl)
datos = read_excel("incumplimiento.xlsx")

datos$incumplimiento = as.factor(datos$incumplimiento)
datos$minimo         = as.factor(datos$minimo)
datos$tipo           = as.factor(datos$tipo)

summary(datos)
attach(datos)

table(incumplimiento,minimo)

library(dplyr)
table(incumplimiento,minimo) %>% prop.table(margin=1)
table(incumplimiento,minimo) %>% prop.table(margin=2)
plot(minimo,incumplimiento,ylab="incumplimiento",xlab="realiza pago mínimo")

table(incumplimiento,tipo)
table(incumplimiento,tipo) %>% prop.table(margin=2)
plot(tipo,incumplimiento,ylab="incumplimiento",xlab="tipo de tarjeta")



library(DataExplorer)
plot_correlation(datos, maxcat = 5L)

modelo_logistico = glm(incumplimiento~sueldo+numeroi+minimo+tipo, family = "binomial")
summary(modelo_logistico)

modelo_nulo = glm(incumplimiento~1, family = "binomial")
library(lmtest)
TestRV = lrtest(modelo_nulo,modelo_logistico)
TestRV

modelo_sintipo = glm(incumplimiento~sueldo+numeroi+minimo, family = "binomial")
TestRV = lrtest(modelo_sintipo,modelo_logistico)
TestRV

modelo_logistico = modelo_sintipo
summary(modelo_logistico)

modelo_nulo = glm(incumplimiento~1, family = "binomial")
TestRV = lrtest(modelo_nulo,modelo_logistico)
TestRV

exp(coef(modelo_logistico))
modelo_logistico %>% coef() %>% exp()

TestRV$LogLik
1-TestRV$LogLik[2]/TestRV$LogLik[1]

predicciones = predict(modelo_logistico, type=c("response"))
comparacion  = data.frame(OBS  = incumplimiento,
                          PRED = as.factor(round(predicciones,2))) %>% arrange(PRED)
comparacion


library(caret)
confusionMatrix(comparacion$PRED, comparacion$OBS, positive = "1")

rocobj = roc(incumplimiento, predicciones, auc = TRUE, ci = TRUE  )
plot(rocobj)
plot.roc(rocobj, 
         legacy.axes = TRUE, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = FALSE,
         max.auc.polygon = FALSE, 
         col  = "darkblue", 
         grid = TRUE )
