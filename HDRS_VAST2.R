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

TMS_HDRST2T <- within(TMS_HDRS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- "TxT2"
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_HDRST2T <- na.omit(TMS_HDRST2T); TMS_HDRST2T

TMS_HDRST2TX <- merge(TMS_HDRST0T, TMS_HDRST2T, by = "rid"); head(TMS_HDRST2TX)

TMS_HDRST2TX$diff_hdrs <- apply(TMS_HDRST2TX[,c('tot_score.x', 'tot_score.y')], 1, function(x) { (x[2]-x[1])} ); TMS_HDRST2TX$diff_hdrs

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

TMS_VAST2T <- within(TMS_VAS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- "TxT2"
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_VAST2T <- na.omit(TMS_VAST2T); TMS_VAST2T

TMS_VAST2TX <- merge(TMS_VAST0T, TMS_VAST2T, by = "rid"); head(TMS_VAST2TX)

TMS_VAST2TX$diff_vas <- apply(TMS_VAST2TX[,c('vas.x', 'vas.y')], 1, function(x) { (x[2]-x[1])} ); TMS_VAST2TX$diff_vas


VAS_HDRS_TX2 <- merge(TMS_HDRST2TX, TMS_VAST2TX, by = "rid")

## 3.  Graficas de grupo de intervención
library(ggplot2)
hv5 <- ggplot(VAS_HDRS_TX2, aes(x=diff_hdrs, y=diff_vas)) + geom_point() + ggtitle("Delta de puntuaciones totales de HDRS y VAS", subtitle = "Grupo de intervención a los 3 meses") + xlab("HDRS") + ylab("VAS") + geom_smooth(method=lm); hv5


## 4.  Prueba de normalidad

qqnorm(VAS_HDRS_TX2$diff_hdrs); qqline(VAS_HDRS_TX2$diff_hdrs, col="red")
qqnorm(VAS_HDRS_TX2$diff_vas); qqline(VAS_HDRS_TX2$diff_vas, col="red")

shapiro.test(VAS_HDRS_TX2$diff_vas) #Dist. normal
shapiro.test(VAS_HDRS_TX2$diff_hdrs) #Dist. normal

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

TMS_HDRST2S <- within(TMS_HDRS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- "ShamT2"
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_HDRST2S <- na.omit(TMS_HDRST2S); TMS_HDRST2S

TMS_HDRSSHAM2 <- merge(TMS_HDRST0S, TMS_HDRST2S, by = "rid"); head(TMS_HDRSSHAM2)

TMS_HDRSSHAM2$diff_hdrs <- apply(TMS_HDRSSHAM2[,c('tot_score.x', 'tot_score.y')], 1, function(x) { (x[2]-x[1])}); TMS_HDRSSHAM2$diff_hdrs

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

TMS_VAST2S <- within(TMS_VAS, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- NA
  Obs[gstage == "Treatment.T0" ] <- NA
  Obs[gstage == "Sham.T1" ] <- NA
  Obs[gstage == "Treatment.T1" ] <- NA
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- "ShamT2"
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_VAST2S <- na.omit(TMS_VAST2S); TMS_VAST2S

TMS_VASSHAM2 <- merge(TMS_VAST0S, TMS_VAST2S, by = "rid"); head(TMS_VASSHAM2)

TMS_VASSHAM2$diff_vas <- apply(TMS_VASSHAM2[,c('vas.x', 'vas.y')], 1, function(x) { (x[2]-x[1])} ); TMS_VASSHAM2$diff_vas

VAS_HDRS_SH2 <- merge(TMS_VASSHAM2, TMS_HDRSSHAM2, by = "rid")

## 2.  Graficas de grupo Sham
library(ggplot2)
hv6 <- ggplot(VAS_HDRS_SH2, aes(x=diff_hdrs, y=diff_vas)) + geom_point() + ggtitle("Delta de puntuaciones totales de HDRS y VAS a los 3 meses", subtitle = "Grupo Sham") + xlab("HDRS") + ylab("VAS") + geom_smooth(method=lm); hv6

## 3.  Prueba de normalidad
qqnorm(VAS_HDRS_SH2$diff_hdrs); qqline(VAS_HDRS_SH2$diff_hdrs, col="red")
qqnorm(VAS_HDRS_SH2$diff_vas); qqline(VAS_HDRS_SH2$diff_vas, col="red")

shapiro.test(VAS_HDRS_SH2$diff_vas) #Dist. normal
shapiro.test(VAS_HDRS_SH2$diff_hdrs) #Dist. normal


## Ambas graficas de correlación
library(tidyverse)
V3 <- VAS_HDRS_SH2 %>% 
  select(rid, group.x.x,diff_hdrs,diff_vas)
mean(V3$diff_hdrs); sd(V3$diff_hdrs)
mean(V3$diff_vas); sd(V3$diff_vas)

V4 <- VAS_HDRS_TX2 %>% 
  select(rid, group.x.x,diff_hdrs,diff_vas)
mean(V4$diff_hdrs); sd(V4$diff_hdrs)
mean(V4$diff_vas); sd(V4$diff_vas)

V5 <-rbind(V3,V4)
mean(V5$diff_hdrs); sd(V5$diff_hdrs)
mean(V5$diff_vas); sd(V5$diff_vas)

V5
hv7 <- ggplot(V5, aes(x=diff_hdrs, y=diff_vas, colour = group.x.x)) + geom_point() +geom_smooth(method=lm) + ggtitle("Figura 8. Delta de puntuaciones totales de HDRS y VAS", subtitle = "Comparación de ambos grupos") + xlab("Delta de HDRS") + ylab("Delta de VAS") + geom_smooth(method=lm); hv7 ## Grupo Sham vs Tratamiento de TO a T2

hv12 <- ggplot(V5, aes(x=diff_hdrs, y=diff_vas)) + geom_point() +geom_smooth(method=lm) + ggtitle("Delta de puntuaciones totales de depresión (HDRS) y craving (VAS) a 3 meses", subtitle = "Conjunto de ambos grupos") + xlab("Delta de HDRS") + ylab("Delta de VAS") + geom_smooth(method=lm); hv12

#Indice de correlación grupo Sham
corr <- cor.test(x=VAS_HDRS_SH2$diff_hdrs, y=VAS_HDRS_SH2$diff_vas, method = 'spearman'); corr

#Indice de correlación grupo de tratamiento
corr2 <- cor.test(x=VAS_HDRS_TX2$diff_hdrs, y=VAS_HDRS_TX2$diff_vas, method = 'spearman'); corr2

#Indice de correlación en ambos grupos
corr3 <- cor.test(x=V5$diff_hdrs, y=V5$diff_vas, method = 'pearson'); corr3
