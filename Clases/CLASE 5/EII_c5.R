library(olsrr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(purrr)
library(tibble)
library(nortest)
library(goftest)

library(haven)
un_ejemplo_de_regresion <- read_sav("un ejemplo de regresion.sav")
View(un_ejemplo_de_regresion)


# REGRESION MULTIPLE CON lm() y con ols_regress()

model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = un_ejemplo_de_regresion)

summary(model)

model1 <- ols_regress(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = un_ejemplo_de_regresion)

# CORRELACION DE CADA PREDICTOR CON LA DEPENDIENTE
ols_correlations(model)

# DIAGNOSTICOS DE COLINEALIDAD
ols_vif_tol(model)

# TODOS LOS MODELOS POSIBLES
k <- ols_step_all_possible(model)
plot(k)
k

# LOS MEJORES MODELOS DE CADA TAMAÑO
ols_step_best_subset(model)


# stepwise regression
model <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 , data = un_ejemplo_de_regresion)
kkk <- ols_step_both_p(model)
plot(kkk)
kkk

#ALGUNOS GRAFICOS DE RESIDUOS
ols_plot_resid_stud_fit(model)

ols_plot_resid_qq(model)

#test de homocedasticidad
ols_test_breusch_pagan(model)

# graficos de la dependiente
ols_plot_response(model)

