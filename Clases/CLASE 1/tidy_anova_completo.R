library(tidyverse)
library(ggpubr)
library(rstatix)

library(apa)

# como armar una variable factor en tidyverse!!

Insomnio_2 <- Insomnio_2%>%
  mutate(fedad=factor(Edad, labels=(c("menor20","entre 20 y 25","mayor25")))) 

# estadísticos descriptivos de la variable dependiente, en cada grupo

Insomnio_2 %>%
  group_by(fedad) %>%
  get_summary_stats(PrimerNoche, type = "mean_sd")

# gráficos de la dependiente en cada grupo 

ggboxplot(Insomnio_2, x = "fedad", y = "PrimerNoche")


# test de normalidad en cada grupo

Insomnio_2 %>%
  group_by(fedad) %>%
  shapiro_test(PrimerNoche)


# qqplot en cada grupo
ggqqplot(Insomnio_2, "PrimerNoche", facet.by = "fedad")


# test de homocedasticidad de varianzas
Insomnio_2 %>% levene_test(PrimerNoche ~ fedad)

############## test de anova
### original
summary(aov(PrimerNoche~fedad, data=Insomnio_2))

#### otra en tidy
Insomnio_2 %>% anova_test(PrimerNoche~fedad)


#################### post hoc

Insomnio_2 %>% pairwise_t_test(PrimerNoche ~ fedad, p.adjust.method = "bonferroni")

Insomnio_2 %>% pairwise_t_test(PrimerNoche ~ fedad, p.adjust.method = "holm")


aov(PrimerNoche ~ fedad, data = Insomnio_2) %>% tukey_hsd()

 #### tabla anova con formato apa

anova_apa(aov(PrimerNoche ~ fedad, data = Insomnio_2))

##### ANOVA ROBUSTO

Insomnio_2 %>% welch_anova_test(PrimerNoche ~ fedad)

games_howell_test(Insomnio_2, PrimerNoche ~ fedad, conf.level = 0.95, detailed = F)


###################### grafico de medias


m <- Insomnio_2%>%
  group_by(fedad)%>%
  get_summary_stats(PrimerNoche)

ggplot(m, aes(as.numeric(fedad), mean)) +
  geom_point()+
  geom_line(line=2)


