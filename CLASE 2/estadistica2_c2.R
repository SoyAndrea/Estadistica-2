library(tidyverse)
library(ggpubr)
library(rstatix)
library(plyr)
library(dplyr)
library(apa)
library(apaTables)


# One-way ANOVA test
#:::::::::::::::::::::::::::::::::::::::::
Insomnio_2 %>% 
     anova_test(PrimerNoche ~ Fedad)


###### anova con la variable dependiente como factor ##################
d <- aov(PrimerNoche ~ Edad, data=Insomnio_2)
anova_apa(d)

summary(d)

Insomnio_2$Fedad <- as.factor(Insomnio_2$Edad)

dd <- aov(PrimerNoche ~ Fedad, data=Insomnio_2)
anova_apa(dd)
summary(dd)

###### test post-hoc

# HSD Tukey
require(graphics)
TukeyHSD(dd, "Fedad", ordered = TRUE)
plot(TukeyHSD(dd, "Fedad"))

tukey_hsd(dd)

# BONFERRONI
pairwise.t.test(Insomnio_2$PrimerNoche, Insomnio_2$Fedad, p.adjust.method = "bonferroni",
                pool.sd = TRUE, paired = FALSE)


# POST HOC CON VARIANZAS DESIGUALES
games_howell_test(Insomnio_2, PrimerNoche~Fedad, conf.level = 0.95, detailed = FALSE)

# ANOVA PARA VARIANZAS DESIGUALES
welch_anova_test(Insomnio_2, PrimerNoche~Fedad)


##### RESOLUCION DEL EJERCICIO 2 ########

placebo <- c(27,16,18,26,18,28,25,20,24,26)
rc <- c(10,8,14,16,18,8,12,14,9,7)
ca <- c(16,18,12,15,9,13,17,20,21,19)
eyn <- c(26,24,17,23,25,22,16,15,18,23)

depresion <- c(placebo,rc,ca,eyn)
trat <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10))
ftrat <- factor(trat, labels=c("Placebo","Reestruccog","Capacasert","Ejercynut"))
eje2 <- data.frame(trat, depresion,ftrat)
eje2

#grafico de boxplot por grupo de edad
ggboxplot(eje2, x = "ftrat", y = "depresion")

# testeo de normalidad de las variables
eje2 %>%
  group_by(ftrat) %>%
  shapiro_test(depresion)

ggqqplot(eje2, "depresion", facet.by = "ftrat")

# testeo de homocedasticidad de varianzas

eje2 %>% levene_test(depresion ~ ftrat)

# test de anova de un factor
one.way <- aov(depresion ~ ftrat, data = eje2)
summary(one.way)
anova_apa(one.way)



# HSD Tukey
require(graphics)
TukeyHSD(one.way, "ftrat", ordered = TRUE)
plot(TukeyHSD(one.way, "ftrat"))


## OTRA FORMA DE HACER EL GRAFICO DE INTERACCION ##
library(ggplot2)

ggplot(data = eje2, aes(x = ftrat, y =depresion)) + 
  stat_summary(fun = mean, geom = "line") + stat_summary(fun = mean, geom = "point") + 
  labs(y = "mean (depresión)") + theme_bw()

