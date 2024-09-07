## PARA REGRESION LINEAL

ansiedad <- c(28,41,35,39,31,42,50,46,45,37)
nota <- c(82,58,63,89,92,64,55,70,51,72)
examen <- data.frame(ansiedad,nota)

plot(ansiedad,nota)

correl <- cor(ansiedad,nota)
correl

regre <- lm(nota~ansiedad,data=examen)

summary(regre)

abline(lm(nota ~ ansiedad))


coefficients(regre)
anova(regre)
fitted.values(regre)
residuals(regre)






