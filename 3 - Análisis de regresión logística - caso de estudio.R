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
plot_correlation(datos)


# Primera propuesta

modelo_logistico1 = glm(incumplimiento~sueldo+numeroi+minimo+tipo, family = "binomial")
summary(modelo_logistico1)

modelo_nulo = glm(incumplimiento~1, family = "binomial")
library(lmtest)
TestRV = lrtest(modelo_nulo,modelo_logistico1)
TestRV

# Segunda propuesta

modelo_logistico2 = glm(incumplimiento~sueldo+numeroi+minimo, family = "binomial")
summary(modelo_logistico2)

TestRV = lrtest(modelo_logistico2,modelo_logistico1)
TestRV

TestRV = lrtest(modelo_nulo,modelo_logistico2)
TestRV

# Interpretación de coeficientes

exp(coef(modelo_logistico2))
modelo_logistico2 %>% coef() %>% exp()

# Pseudo R²
TestRV$LogLik
1-TestRV$LogLik[2]/TestRV$LogLik[1]

# Predicciones
predicciones = predict(modelo_logistico2, type=c("response"))
comparacion  = data.frame(OBS  = incumplimiento,
                          PRED = as.factor(round(predicciones,0))) 
comparacion


library(caret)
confusionMatrix(comparacion$PRED, comparacion$OBS, positive = "1")


rocobj = roc(incumplimiento, predicciones, auc = TRUE, ci = TRUE  )
plot(rocobj)
plot.roc(rocobj, 
         print.thres = "best",
         print.auc   = TRUE,
         auc.polygon = FALSE,
         max.auc.polygon = FALSE, 
         col  = "darkblue", 
         grid = TRUE )
