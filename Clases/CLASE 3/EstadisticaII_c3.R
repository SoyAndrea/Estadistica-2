# LIBRERIAS A UTILIZAR

library(tidyverse)
library(ggpubr)
library(rstatix)
library(apa)
library(apaTables)

# CREACION DE FACTORES PARA USAR EN ANOVA 
Insomnio_2 <- Insomnio_2%>%
  mutate(genero= factor(Sexo, labels = c("masculino","femenino")),
         Fedad=factor(Edad, labels = c("menor de 20","entre 20 y 25","mayor que 25")))

# ANOVA DE DOS FACTORES SIN INTERACCION 
dd0 <- aov(PrimerNoche ~ Fedad+genero, data=Insomnio_2)
summary(dd0)

# ANOVA DE DOS FACTORES CON INTERACCION
dd <- aov(PrimerNoche ~ Fedad*genero, data=Insomnio_2)
summary(dd)

Insomnio_2 %>% 
  anova_test(PrimerNoche ~ Fedad*genero)

# TABLAS CON RESUMENES ESTADISTICOS
Insomnio_2 %>%
  group_by(genero,Fedad) %>%
  get_summary_stats(PrimerNoche, type = "mean_sd")


# testeo de normalidad de las variables
Insomnio_2 %>%
  group_by(genero,Fedad) %>%
  shapiro_test(PrimerNoche)

ggqqplot(Insomnio_2, "PrimerNoche", facet.by = c("Fedad","genero"))

# testeo de homocedasticidad de varianzas

Insomnio_2 %>% levene_test(PrimerNoche ~ Fedad*genero)


# gráfico de interacción
with(Insomnio_2, interaction.plot(Fedad, genero, PrimerNoche, fun = mean,
                                  main = "Interaction Plot"))






