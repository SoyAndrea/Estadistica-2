library(tidyverse)
library(caret)
library(oddsratio)

# ejercicio de bajo peso al nacer
# lectura de la base
library(haven)
LOWBWT <- read_sav("LOWBWT.sav")
View(LOWBWT)

fumar <- glm(LOW~SMOKE,data=LOWBWT,family="binomial")
summary(fumar)

raza <- glm(LOW~factor(RACE),data=LOWBWT,family="binomial")
summary(raza)
or_glm(data = LOWBWT, model = raza, incr = list(RACE=1))

modelo <- glm(LOW~SMOKE+factor(RACE),data=LOWBWT,family="binomial")
summary(modelo)
or_glm(data = LOWBWT, model = modelo, incr = list(RACE=1,SMOKE=1))


predic_fumar <- plogis(predict(fumar, LOWBWT))
predic_raza <- plogis(predict(raza, LOWBWT))
predic_modelo <- plogis(predict(modelo, LOWBWT))

##################

predicted_f <- factor(predic_fumar>0.31, labels = c("0","1"))

predicted_r <- factor(predic_raza>0.31, labels = c("0","1"))

predicted_m <- factor(predic_modelo>0.31, labels = c("0","1"))


library(caret)

#Creating confusion matrix
cm_fumar <- confusionMatrix(data=predicted_f, reference = as.factor(LOWBWT$LOW), positive="1")
cm_fumar

cm_raza <- confusionMatrix(data=predicted_r, reference = as.factor(LOWBWT$LOW), positive="1")
cm_raza

cm_modelo <- confusionMatrix(data=predicted_m, reference = as.factor(LOWBWT$LOW), positive="1")
cm_modelo


#Display results 
library(pROC)

LOWBWT <- LOWBWT%>%
  mutate(dico=factor(LOW))

roc.f <- roc(as.factor(LOWBWT$LOW), predic_fumar)

roc.r <- roc(as.factor(LOWBWT$LOW), predic_raza)

roc.m <- roc(as.factor(LOWBWT$LOW), predic_modelo)

# Simple example: roc plot 
plot(roc.f,ylim=c(0,1),xlim=c(1,0))
plot(roc.r,ylim=c(0,1),xlim=c(1,0))
plot(roc.m,ylim=c(0,1),xlim=c(1,0))


#create ROC plot
ggroc(roc.f)
ggroc(roc.r)
ggroc(roc.m)



library(DescTools)
PseudoR2(fumar,c("McFadden","Nagel","CoxSnell"))


# SELECCION DE PREDICTORES
library(MASS)

# de un ejemplo. ESTE ES PARA MOSTRAR
base <- glm(LOW ~ 1,family = binomial, data = LOWBWT)
birthwt.step <- stepAIC(base,scope = list(upper = ~AGE+LWT+factor(RACE)+SMOKE+PTL+HT+UI+FTV, lower = ~1),
                        direction = c("both"),trace = T)
View(birthwt.step$anova)

final <- glm(LOW ~ LWT+PTL+HT,
             family = binomial, data = LOWBWT)
summary(final)

final2 <- glm(LOW ~ LWT+factor(RACE)+SMOKE+HT,
              family = binomial, data = LOWBWT)
summary(final2)

anova(final2,final)

# significacion del chi cuadrado
1-pchisq(birthwt.step$anova$Deviance, birthwt.step$anova$Df, ncp = 0, lower.tail = TRUE, log.p = FALSE)


1-pchisq(18, 18, ncp = 0, lower.tail = TRUE, log.p = FALSE)
