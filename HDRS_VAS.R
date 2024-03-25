### Bases de datos de proyecto TMS ###
### Por Alan Davalos ###

###H3. La disminución de los síntomas depresivos se relaciona directamente a la disminución del craving.
### Prueba de correlación
## 1.  Cargar Bases HDRS y VAS

TMS_HDRS<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/HDRS.csv", header = TRUE)
TMS_HDRS$group <- factor(TMS_HDRS$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"))
TMS_HDRS$rid <- factor(TMS_HDRS$rid)
TMS_HDRS$stage <- factor(TMS_HDRS$stage)
TMS_HDRS$score_categories <- factor(TMS_HDRS$score_categories)


TMS_VAS<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/VAS.csv", header = TRUE)
TMS_VAS$group <- factor(TMS_VAS$group,
                        levels = c(1, 2),
                        labels = c("Sham", "Treatment"))
TMS_VAS$rid <- factor(TMS_VAS$rid)
TMS_VAS$stage <- factor(TMS_VAS$stage)

## 2. Reordenar variables de Tratamiento
# HDRS
TMS_HDRS$gstage <- interaction(TMS_HDRS$group,TMS_HDRS$stage, drop = T); TMS_HDRS$gstage

TMS_HDRST0T <- within(TMS_HDRS, {
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
TMS_HDRST0T <- na.omit(TMS_HDRST0T); TMS_HDRST0T

TMS_HDRST1T <- within(TMS_HDRS, {
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
TMS_HDRST1T <- na.omit(TMS_HDRST1T); TMS_HDRST1T

TMS_HDRSTX <- merge(TMS_HDRST0T, TMS_HDRST1T, by = "rid"); head(TMS_HDRSTX)

TMS_HDRSTX$diff_hdrs <- apply(TMS_HDRSTX[,c('tot_score.x', 'tot_score.y')], 1, function(x) { (x[2]-x[1])} ); TMS_HDRSTX$diff_hdrs

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


VAS_HDRS_TX <- merge(TMS_HDRSTX, TMS_VASTX, by = "rid")

summary(VAS_HDRS_TX1$diff_vas)
sd(VAS_HDRS_TX1$diff_vas)

summary(VAS_HDRS_TX1$diff_hdrs)
sd(VAS_HDRS_TX1$diff_hdrs)
## 3.  Graficas de grupo de intervención
library(ggplot2)
hv <- ggplot(VAS_HDRS_TX, aes(x=diff_hdrs, y=diff_vas)) + geom_point() + ggtitle("Delta de puntuaciones totales de HDRS y VAS", subtitle = "Grupo de intervención") + xlab("HDRS") + ylab("VAS") + geom_smooth(method=lm); hv

hist(VAS_HDRS_TX$diff_hdrs, probability = T, main = "Distribución de deltas en la puntuación de HDRS, grupo de Intervención", breaks=10); lines(density(VAS_HDRS_TX$diff_htdrs, na.rm = T), col=2)

hist(VAS_HDRS_TX$diff_vas, probability = T, main = "Distribución de las deltas en la puntuación de VAS, grupo de Intervención", breaks=10); lines(density(VAS_HDRS_TX$diff_vas, na.rm = T), col=2)

## 4.  Prueba de normalidad

qqnorm(VAS_HDRS_TX$diff_hdrs); qqline(VAS_HDRS_TX$diff_hdrs, col="red")
qqnorm(VAS_HDRS_TX$diff_vas); qqline(VAS_HDRS_TX$diff_vas, col="red")

shapiro.test(VAS_HDRS_TX$diff_vas) #Dist. normal
shapiro.test(VAS_HDRS_TX$diff_hdrs) #Dist. normal

### Grupo Sham ###
## 1.  Reordenar variables de grupo Sham
#Hdrs
TMS_HDRS$gstage <- interaction(TMS_HDRS$group,TMS_HDRS$stage, drop = T); TMS_HDRS$gstage

TMS_HDRST0S <- within(TMS_HDRS, {
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
TMS_HDRST0S <- na.omit(TMS_HDRST0S); TMS_HDRST0S

TMS_HDRST1S <- within(TMS_HDRS, {
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
TMS_HDRST1S <- na.omit(TMS_HDRST1S); TMS_HDRST1S

TMS_HDRSSHAM <- merge(TMS_HDRST0S, TMS_HDRST1S, by = "rid"); head(TMS_HDRSSHAM)

TMS_HDRSSHAM$diff_hdrs <- apply(TMS_HDRSSHAM[,c('tot_score.x', 'tot_score.y')], 1, function(x) { (x[2]-x[1])}); TMS_HDRSSHAM$diff_hdrs

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

VAS_HDRS_SH <- merge(TMS_VASSHAM, TMS_HDRSSHAM, by = "rid")

summary(VAS_HDRS_SH1$diff_hdrs)
sd(VAS_HDRS_SH1$diff_hdrs)

summary(VAS_HDRS_SH1$diff_vas)
sd(VAS_HDRS_SH1$diff_vas)

## 2.  Graficas de grupo Sham
library(ggplot2)
hv2 <- ggplot(VAS_HDRS_SH, aes(x=diff_hdrs, y=diff_vas)) + geom_point() + ggtitle("Delta de puntuaciones totales de HDRS y VAS", subtitle = "Grupo Sham") + xlab("HDRS") + ylab("VAS") + geom_smooth(method=lm); hv2

hist(VAS_HDRS_SH$diff_hdrs, probability = T, main = "Distribución de deltas en la puntuación de HDRS, grupo Sham", breaks=10); lines(density(VAS_HDRS_SH$diff_hdrs, na.rm = T), col=2)

hist(VAS_HDRS_SH$diff_vas, probability = T, main = "Distribución de las deltas en la puntuación de VAS, grupo Sham", breaks=10); lines(density(VAS_HDRS_SH$diff_vas, na.rm = T), col=2)

## 3.  Prueba de normalidad
qqnorm(VAS_HDRS_SH$diff_hdrs); qqline(VAS_HDRS_SH$diff_hdrs, col="red")
qqnorm(VAS_HDRS_SH$diff_vas); qqline(VAS_HDRS_SH$diff_vas, col="red")

shapiro.test(VAS_HDRS_SH$diff_vas) #Dist. normal
shapiro.test(VAS_HDRS_SH$diff_hdrs) #Dist. normal


## Ambas graficas de correlación
library(tidyverse)
VAS_HDRS_SH1 <- VAS_HDRS_SH %>% filter(!rid %in% c( 1, 10))
VAS_HDRS_TX1 <- VAS_HDRS_TX %>% filter(!rid %in% c(20, 7, 14, 3))

V <- VAS_HDRS_SH1 %>% 
  select(rid, group.x.x,diff_hdrs,diff_vas)
V
V1 <- VAS_HDRS_TX1 %>% 
select(rid, group.x.x,diff_hdrs,diff_vas)
V1
V2 <-rbind(V,V1)

library(reshape)
V2 = rename(V2, c(group.x.x = "Grupo"))

V2

hv3 <- ggplot(V2, aes(x=diff_hdrs, y=diff_vas, colour = Grupo)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 9. Delta de puntuaciones totales de HDRS y VAS", subtitle = "Comparación de ambos grupos") + xlab("Delta de HDRS") + ylab("Delta de VAS") + geom_ribbon(stat='smooth', method = "lm", se=TRUE, alpha=0.001, aes(color = NULL, group = Grupo)) + geom_line(stat='smooth', method = "lm", alpha=0.001); hv3 



## Grupo Sham vs Tratamiento de TO a T1

#Indice de correlación grupo Sham
corr <- cor.test(x=VAS_HDRS_SH1$diff_hdrs, y=VAS_HDRS_SH1$diff_vas, method = 'spearman'); corr

#Indice de correlación grupo de tratamiento
corr2 <- cor.test(x=VAS_HDRS_TX1$diff_hdrs, y=VAS_HDRS_TX1$diff_vas, method = 'spearman'); corr2

