
# ---------------- #
# Lectura de datos #
# ---------------- #

library(readxl)
datos = read_xlsx('Bolsa.xlsx')
datos$DISTRITO = factor(datos$DISTRITO)
datos$BOLSA    = factor(datos$BOLSA, ordered = T,
                        levels = c("NUNCA","A VECES","REGULARMENTE","SIEMPRE"))

# ------------------ #
# Gráficos iniciales #
# ------------------ #

library(ggplot2)
datos %>% 
  count(BOLSA) %>% 
  ggplot(aes(x = BOLSA, y = n, label = n)) + 
  geom_bar(stat = "identity", fill = "gold") + 
  geom_text(position = position_stack(vjust = 0.5)) + 
  labs(x = "Frecuencia con la que pide bolsa",
       y = "Número de personas") + 
  theme_minimal()

tabla = table(datos)

mosaicplot(tabla)
mosaicplot(tabla, col=c("dodgerblue","gold","mediumorchid","red"), main="")

library(gplots)
balloonplot(tabla)
balloonplot(t(tabla))

# ------------------------------------------- #
# Viabilidad del análisis de correspondencias #
# ------------------------------------------- #

chisq.test(tabla)

# ---------------------------- #
# Análisis de correspondencias #
# ---------------------------- #

library(FactoMineR)
library(FactoClass)

# Perfiles
corresp  = CA(tabla, graph = FALSE)
perfiles = plotct(tabla, 
                  profiles    = "none",
                  tables      = T)

perfiles$ctm

perfiles$perR
plotct(tabla,"row",col=c("dodgerblue","gold","mediumorchid","red"))

perfiles$perC
plotct(tabla,"col",col=c("dodgerblue","gold","mediumorchid","red"))

# Masas
corresp$call$marge.col*100
corresp$call$marge.row*100

# Número de dimensiones 
(ndime = min(ncol(tabla),nrow(tabla))-1)
get_eigenvalue(corresp)
fviz_screeplot(corresp, addlabels = TRUE)

(media = 100/ndime)

fviz_screeplot(corresp, addlabels = TRUE) + 
  geom_hline(yintercept = media, col = "red", size = 2)


# Gráficos
corresp$row$coord
fviz_ca_row(corresp, repel = TRUE)

corresp$col$coord
fviz_ca_col(corresp)

fviz_ca_biplot(corresp)

plot(corresp,map="symbioplot")

# Asociación entre categoría y eje
corresp$row$cos2
corresp$col$cos2

# Contribución de cada categoría al eje
corresp$row$contrib
fviz_contrib(corresp, choice = "row", axes = 1)
fviz_contrib(corresp, choice = "row", axes = 2)
fviz_contrib(corresp, choice = "row", axes = 3)

corresp$col$contrib
fviz_contrib(corresp, choice = "col", axes = 1)
fviz_contrib(corresp, choice = "col", axes = 2)
fviz_contrib(corresp, choice = "col", axes = 3)

# Resumen
summary(corresp)

# ----------------------------------------- #
# Análisis de correspondencias (paquete ca) #
# ----------------------------------------- #

library(ca)
corresp2 = ca(tabla)
corresp2
plot(corresp2)

