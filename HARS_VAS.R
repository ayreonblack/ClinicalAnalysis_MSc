### Bases de datos de proyecto TMS ###
### Por Alan Davalos ###

###H5. La disminución de los síntomas depresivos se correlaciona con disminución del craving.
### Prueba de correlación
## 1.  Cargar Bases HARS y VAS

TMS_HARS<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/HARS.csv", header = TRUE)
TMS_HARS$group <- factor(TMS_HARS$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"))
TMS_HARS$rid <- factor(TMS_HARS$rid)
TMS_HARS$stage <- factor(TMS_HARS$stage)
TMS_HARS$hars_categories <- factor(TMS_HARS$hars_categories)


TMS_VAS<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/VAS.csv", header = TRUE)
TMS_VAS$group <- factor(TMS_VAS$group,
                        levels = c(1, 2),
                        labels = c("Sham", "Treatment"))
TMS_VAS$rid <- factor(TMS_VAS$rid)
TMS_VAS$stage <- factor(TMS_VAS$stage)

## 2. Reordenar variables de Tratamiento
# HDRS
TMS_HARS$gstage <- interaction(TMS_HARS$group,TMS_HARS$stage, drop = T); TMS_HARS$gstage

TMS_HARST0T <- within(TMS_HARS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- "TxT0"
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_HARST0T <- na.omit(TMS_HARST0T); TMS_HARST0T

TMS_HARST1T <- within(TMS_HARS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- "TxT1"
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_HARST1T <- na.omit(TMS_HARST1T); TMS_HARST1T

TMS_HARSTX <- merge(TMS_HARST0T, TMS_HARST1T, by = "rid"); head(TMS_HARSTX)

TMS_HARSTX$diff_hars <- apply(TMS_HARSTX[,c('hars_tot.x', 'hars_tot.y')], 1, function(x) { (x[2]-x[1])} ); TMS_HARSTX$diff_hars

# VAS
TMS_VAS$gstage <- interaction(TMS_VAS$group,TMS_VAS$stage, drop = T); TMS_VAS$gstage

TMS_VAST0T <- within(TMS_VAS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- "TxT0"
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_VAST0T <- na.omit(TMS_VAST0T); TMS_VAST0T

TMS_VAST1T <- within(TMS_VAS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- "TxT1"
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_VAST1T <- na.omit(TMS_VAST1T); TMS_VAST1T

TMS_VASTX <- merge(TMS_VAST0T, TMS_VAST1T, by = "rid"); head(TMS_VASTX)

TMS_VASTX$diff_vas <- apply(TMS_VASTX[,c('vas.x', 'vas.y')], 1, function(x) { (x[2]-x[1])} ); TMS_VASTX$diff_vas


VAS_HARS_TX <- merge(TMS_HARSTX, TMS_VASTX, by = "rid")

summary(VAS_HARS_TX$diff_hars)
sd(VAS_HARS_TX$diff_hars)

## 3.  Graficas de grupo de intervención
library(ggplot2)
hav <- ggplot(VAS_HDRS_TX, aes(x=diff_hdrs, y=diff_vas)) + geom_point() + ggtitle("Delta de puntuaciones totales de HARS y VAS", subtitle = "Grupo de intervención") + xlab("HARS") + ylab("VAS") + geom_smooth(method=lm); hav

hist(VAS_HARS_TX$diff_hars, probability = T, main = "Distribución de deltas en la puntuación de HARS, grupo de Intervención", breaks=10); lines(density(VAS_HARS_TX$diff_hars, na.rm = T), col=2)

hist(VAS_HARS_TX$diff_vas, probability = T, main = "Distribución de las deltas en la puntuación de VAS, grupo de Intervención", breaks=10); lines(density(VAS_HARS_TX$diff_vas, na.rm = T), col=2)

## 4.  Prueba de normalidad

qqnorm(VAS_HARS_TX$diff_hars); qqline(VAS_HARS_TX$diff_hars, col="red")
qqnorm(VAS_HARS_TX$diff_vas); qqline(VAS_HDRS_TX$diff_vas, col="red")

shapiro.test(VAS_HARS_TX$diff_vas) #Dist. normal
shapiro.test(VAS_HARS_TX$diff_hars) #Dist. normal

### Grupo Sham ###
## 1.  Reordenar variables de grupo Sham
#Hdrs
TMS_HARS$gstage <- interaction(TMS_HARS$group,TMS_HARS$stage, drop = T); TMS_HARS$gstage

TMS_HARST0S <- within(TMS_HARS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- "ShamT0"
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_HARST0S <- na.omit(TMS_HARST0S); TMS_HARST0S

TMS_HARST1S <- within(TMS_HARS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- "ShamT1"
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_HARST1S <- na.omit(TMS_HARST1S); TMS_HARST1S

TMS_HARSSHAM <- merge(TMS_HARST0S, TMS_HARST1S, by = "rid"); head(TMS_HARSSHAM)

TMS_HARSSHAM$diff_hars <- apply(TMS_HARSSHAM[,c('hars_tot.x', 'hars_tot.y')], 1, function(x) { (x[2]-x[1])}); TMS_HARSSHAM$diff_hars

# VAS
TMS_VAS$gstage <- interaction(TMS_VAS$group,TMS_VAS$stage, drop = T); TMS_VAS$gstage

TMS_VAST0S <- within(TMS_VAS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- "ShamT0"
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_VAST0S <- na.omit(TMS_VAST0S); TMS_VAST0S

TMS_VAST1S <- within(TMS_VAS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- "ShamT1"
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_VAST1S <- na.omit(TMS_VAST1S); TMS_VAST1S

TMS_VASSHAM <- merge(TMS_VAST0S, TMS_VAST1S, by = "rid"); head(TMS_VASSHAM)

TMS_VASSHAM$diff_vas <- apply(TMS_VASSHAM[,c('vas.x', 'vas.y')], 1, function(x) { (x[2]-x[1])} ); TMS_VASSHAM$diff_vas

VAS_HARS_SH <- merge(TMS_VASSHAM, TMS_HARSSHAM, by = "rid")

summary(VAS_HARS_SH$diff_vas)
sd(VAS_HARS_SH$diff_vas)

summary(VAS_HARS_SH$diff_hars)
sd(VAS_HARS_SH$diff_hars)

## 2.  Graficas de grupo Sham
library(ggplot2)
hav2 <- ggplot(VAS_HARS_SH, aes(x=diff_hars, y=diff_vas)) + geom_point() + ggtitle("Delta de puntuaciones totales de HARS y VAS", subtitle = "Grupo Sham") + xlab("HARS") + ylab("VAS") + geom_smooth(method=lm); hav2

hist(VAS_HARS_SH$diff_hars, probability = T, main = "Distribución de deltas en la puntuación de HARS, grupo Sham", breaks=10); lines(density(VAS_HARS_SH$diff_hars, na.rm = T), col=2)

hist(VAS_HARS_SH$diff_vas, probability = T, main = "Distribución de las deltas en la puntuación de VAS, grupo Sham", breaks=10); lines(density(VAS_HDRS_SH$diff_vas, na.rm = T), col=2)

## 3.  Prueba de normalidad
qqnorm(VAS_HARS_SH$diff_hars); qqline(VAS_HARS_SH$diff_hars, col="red")
qqnorm(VAS_HARS_SH$diff_vas); qqline(VAS_HARS_SH$diff_vas, col="red")

shapiro.test(VAS_HARS_SH$diff_vas) #Dist. normal
shapiro.test(VAS_HARS_SH$diff_hars) #Dist. normal


## Ambas graficas de correlación
library(tidyverse)
VAS_HARS_SH1 <- VAS_HARS_SH %>% filter(!rid %in% c( 1, 10))
VAS_HARS_TX1 <- VAS_HARS_TX %>% filter(!rid %in% c(20, 7, 14, 3))

W <- VAS_HARS_SH1 %>% 
  select(rid, group.x.x,diff_hars,diff_vas)

W1 <- VAS_HARS_TX1 %>% 
  select(rid, group.x.x,diff_hars,diff_vas)

W2 <-rbind(W,W1)

library(reshape)
W2 = rename(W2, c(group.x.x = "Grupo"))
W2
hav2 <- ggplot(W2, aes(x=diff_hars, y=diff_vas, colour = Grupo)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 10. Delta de puntuaciones totales de HARS y VAS", subtitle = "Comparación de ambos grupos") + xlab("Delta de HARS") + ylab("Delta de VAS") + geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.001, aes(color = NULL, group = Grupo)) + geom_line(stat='smooth', method = "lm", alpha=0.001); hav2
## Grupo Sham vs Tratamiento de TO a T1

#Indice de correlación grupo Sham
corr <- cor.test(x=VAS_HARS_SH1$diff_hars, y=VAS_HARS_SH1$diff_vas, method = 'spearman'); corr

#Indice de correlación grupo de tratamiento
corr2 <- cor.test(x=VAS_HARS_TX1$diff_hars, y=VAS_HARS_TX1$diff_vas, method = 'spearman'); corr2
