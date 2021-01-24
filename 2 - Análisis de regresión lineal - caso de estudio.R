
# CASO DE ESTUDIO: ANÁLISIS DE REGRESIÓN LINEAL MÚLTIPLE

# Carga de paquetes

library(corrplot) # para la función corrplot
library(PerformanceAnalytics) # para la función chart.Correlation
library(ggplot2)  # para la función ggplot2
library(olsrr)    # para las funciones de selección de variables
library(nortest)  # para la función ad.test
library(lmtest)   # para la función dwtest
library(car)      # para la función vif

# Lectura de datos

datosreg = read.table("CasoRegresionLineal.txt", header = T)

# Análisis exploratorio 

summary(datosreg)

cor(datosreg)

corrplot(cor(datosreg))
corrplot(cor(datosreg),addCoef.col = "black")

heatmap(x = cor(datosreg),symm = TRUE)

chart.Correlation(datosreg, histogram=TRUE, pch=19)

boxplot(datosreg$Area~datosreg$Secundaria)

datosreg %>% 
  ggplot(aes(x=as.factor(Secundaria),y=Area)) +
  geom_boxplot(fill = "dodgerblue2") + 
  geom_jitter(width=0.1, colour="darkblue") +
  labs(x = "Estudió Secundaria",
       y = "Area") +
  theme_minimal()
  
# Modelo 

modelo = lm(Area~Ingreso+Familia+AdultoMayor+Posgrado+Secundaria, data = datosreg)
summary(modelo)

# Selección de variables

ols_step_forward_p(modelo, details = TRUE)
ols_step_backward_p(modelo, details = TRUE)
ols_step_both_p(modelo, details = TRUE)

ols_step_forward_aic(modelo, details = TRUE)
ols_step_backward_aic(modelo, details = TRUE)
ols_step_both_aic(modelo, details = TRUE)

modelof = lm(Area~Ingreso+Familia+AdultoMayor, data = datosreg)
summary(modelof)

# Verificación de supuestos

residuales = residuals(modelof)

hist(residuales,main="Histograma de los residuales")
shapiro.test(residuales)
ad.test(residuales)

plot(datosreg$Area,residuales,pch=18,main="Area (Y) vs residuales")
abline(h=0)
ncvTest(modelo)

plot(residuales,type="b",pch=18,main="Índice vs residuales")
abline(h=0)
dwtest(modelof, alternative="two.sided")

plot(fitted(modelof),residuales,pch=18,main="Valores ajustados vs residuales")
abline(h=0)

# Multicolinealidad

vif(modelof)

# Predicción

predict(modelof, data.frame(Ingreso=100,Familia=3,AdultoMayor=1))
predict(modelof, data.frame(Ingreso=100,Familia=3,AdultoMayor=1),interval="confidence")
predict(modelof, data.frame(Ingreso=100,Familia=3,AdultoMayor=1),interval="prediction")

X = data.frame(Ingreso=c(100,150,200),Familia=c(3,4,3),AdultoMayor=c(0,0,1))
predict(modelo_final,X)
predict(modelo_final,X,interval="confidence")
predict(modelo_final,X,interval="prediction")
predict(modelo_final,X,interval="prediction",level=0.9)
