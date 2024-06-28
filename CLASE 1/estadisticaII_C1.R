library(tidyverse)
library(ggpubr)
library(rstatix)
library(plyr)
library(dplyr)


#importación de la base
library(haven)
Insomnio_2 <- read_sav("Insomnio_2.sav")

View(Insomnio_2)


#descriptivo por edad

Insomnio_2 %>%
  group_by(Edad) %>%
  get_summary_stats(PrimerNoche, type = "mean_sd")

#grafico de boxplot por grupo de edad
ggboxplot(Insomnio_2, x = "Edad", y = "PrimerNoche")


# Anova de un factor

model  <- aov(PrimerNoche ~ factor(Edad), data = Insomnio_2)
summary(model)

# testeo de normalidad de las variables
Insomnio_2 %>%
  group_by(Edad) %>%
  shapiro_test(PrimerNoche)

ggqqplot(Insomnio_2, "PrimerNoche", facet.by = "Edad")

# testeo de homocedasticidad de varianzas

Insomnio_2 %>% levene_test(PrimerNoche ~ as.factor(Edad))


##################################
#graficos de medias POR GRUPO    #
##################################
cdata <- ddply(Insomnio_2, c("Edad"), summarise,
               N    = length(PrimerNoche),
               mean = mean(PrimerNoche),
               sd   = sd(PrimerNoche),
               se   = sd / sqrt(N))

cdata

ggplot(cdata, aes(x=Edad, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_line() +
  geom_point()

ggplot(cdata, aes(x=Edad, y=mean)) + 
  geom_line() +
  expand_limits(y=3) + 
  scale_y_continuous(breaks=0:50*0.5) +
  geom_point()


######################################
# EJERCICIO 2
###################################

placebo <- c(27,16,18,26,18,28,25,20,24,26)
rc <- c(10,8,14,16,18,8,12,14,9,7)
ca <- c(16,18,12,15,9,13,17,20,21,19)
eyn <- c(26,24,17,23,25,22,16,15,18,23)

depresion <- c(placebo,rc,ca,eyn)
trat <- c(rep("placebo",10),rep("Reestruccog",10),rep("Capacasert",10),rep("Ejercynut",10))
ftrat <- factor(trat, labels=c("Placebo","Reestruccog","Capacasert","Ejercynut"))
eje2 <- data.frame(trat, depresion,ftrat)
eje2

one.way <- aov(depresion ~ ftrat, data = eje2)
summary(one.way)
anova_apa(one.way)


######################################################

#descriptivo por edad

eje2 %>%
  group_by(trat) %>%
  get_summary_stats(depresion, type = "mean_sd")

#grafico de boxplot por grupo de edad
ggboxplot(eje2, x = "trat", y = "depresion")


# Anova de un factor
model  <- lm(depresion ~ trat, data = eje2)

# testeo de normalidad de las variables
eje2 %>%
  group_by(trat) %>%
  shapiro_test(depresion)

ggqqplot(eje2, "depresion", facet.by = "trat")

# testeo de homocedasticidad de varianzas

eje2 %>% levene_test(depresion ~ as.factor(trat))


##################################
#graficos de medias POR GRUPO    #
##################################
cdata <- ddply(eje2, c("trat"), summarise,
               N    = length(depresion),
               mean = mean(depresion),
               sd   = sd(depresion),
               se   = sd / sqrt(N))

cdata

ggplot(cdata, aes(x=trat, y=mean)) + 
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1) +
  geom_line() +
  geom_point()

ggplot(cdata, aes(x=trat, y=mean)) + 
  geom_point() +
  geom_line() 

