library(tidyverse)
library(olsrr)
library(dplyr)
library(gridExtra)
library(purrr)
library(nortest)
library(goftest)

model <- lm(y ~ x1 + x3 + x5, data = un_ejemplo_de_regresion)
summary(model)

model1 <- ols_regress(y ~ x1 + x3 + x5, data = un_ejemplo_de_regresion)
model1

####################################

# TEST DE NORMALIDAD
ols_test_normality(model)

# DISTRIBUCION DE LOS RESIDUOS
# grafico de boxplot
ols_plot_resid_box(model)

# grafico histograma
ols_plot_resid_hist(model)

# grafico qqplot
ols_plot_resid_qq(model)

#####################################
#gráfico de los residuos

ols_plot_resid_fit(model, print_plot = TRUE)

ols_plot_resid_stand(model, print_plot = TRUE)

ols_plot_resid_stud_fit(model10, print_plot = TRUE)

# gráfico que detecta outliers y puntos influyentes
ols_plot_resid_lev(model, print_plot = TRUE)

# punto de influencia
ols_leverage(model)

# gráficos de la d de Cook
ols_plot_cooksd_bar(model10, print_plot = TRUE)
ols_plot_cooksd_chart(model, print_plot = TRUE)

# dfits
ols_plot_dffits(model10, print_plot = TRUE)

#dfbetas
ols_plot_dfbetas(model10, print_plot = TRUE)

