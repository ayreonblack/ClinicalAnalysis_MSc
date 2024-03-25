### Bases de datos de proyecto TMS ###
### Por Alan Davalos ###

###H3. La disminución de los síntomas depresivos se relaciona directamente a la disminución del craving.
### Prueba de correlación
## Base HDRS y CCQG

TMS_HDRS<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/HDRS.csv", header = TRUE)
TMS_HDRS$group <- factor(TMS_HDRS$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"))
TMS_HDRS$rid <- factor(TMS_HDRS$rid)
TMS_HDRS$stage <- factor(TMS_HDRS$stage)
TMS_HDRS$score_categories <- factor(TMS_HDRS$score_categories)


TMS_CCQG<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/CCQG.csv", header = TRUE)
TMS_CCQG$group <- factor(TMS_CCQG$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"))
TMS_CCQG$rid <- factor(TMS_CCQG$rid)
TMS_CCQG$stage <- factor(TMS_CCQG$stage)

### Tratamiento
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

TMS_HDRSTX$diff_htdrs <- apply(TMS_HDRSTX[,c('tot_score.x', 'tot_score.y')], 1, function(x) { (x[1]-x[2])/x[1] * 100 } ); TMS_HDRSTX$diff_htdrs

# ccqg

TMS_CCQG$gstage <- interaction(TMS_CCQG$group,TMS_CCQG$stage, drop = T); TMS_CCQG$gstage

TMS_CCQGT0T <- within(TMS_CCQG, {
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
TMS_CCQGT0T <- na.omit(TMS_CCQGT0T); TMS_CCQGT0T

TMS_CCQGT1T <- within(TMS_CCQG, {
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
TMS_CCQGT1T <- na.omit(TMS_CCQGT1T); TMS_CCQGT1T

TMS_CCQGTX <- merge(TMS_CCQGT0T, TMS_CCQGT1T, by = "rid"); head(TMS_CCQGTX)

TMS_CCQGTX$diff_ccq <- apply(TMS_CCQGTX[,c('ccq_g.x', 'ccq_g.y')], 1, function(x) { (x[1]-x[2])/x[1] * 100 } ); TMS_CCQGTX$diff_ccq


CCQG_HDRS_TX <- merge(TMS_HDRSTX, TMS_CCQGTX, by = "rid")

## Prueba de correlación, prueba de Hipotesis 3

# Grafica descriptiva
library(ggplot2)
hc <- ggplot(CCQG_HDRS_TX, aes(x=diff_htdrs, y=diff_ccq)) + geom_point() + ggtitle("Puntuaciones totales de HDRS y CCQG") + xlab("HDRS") + ylab("CCQG") + geom_smooth(method=lm); hc

# Prueba de normalidad
hist(CCQG_HDRS_TX$diff_htdrs, probability = T, main = "Distribución de las diferencias proporcionales de HDRS", breaks=10); lines(density(CCQG_HDRS_TX$diff_htdrs, na.rm = T), col=2)

hist(CCQG_HDRS_TX$diff_ccq, probability = T, main = "Distribución de las diferencias proporcionales de CCQG", breaks=10); lines(density(CCQG_HDRS_TX$diff_ccq, na.rm = T), col=2)

qqnorm(CCQG_HDRS_TX$diff_htdrs); qqline(CCQG_HDRS_TX$diff_htdrs, col="red")

shapiro.test(CCQG_HDRS_TX$diff_ccq) #Dist. normal
shapiro.test(CCQG_HDRS_TX$diff_htdrs) #Dist. normal

#Indice de correlación

corr <- cor.test(x=CCQG_HDRS_TX$diff_htdrs, y=CCQG_HDRS_TX$diff_ccq, method = 'pearson'); corr

### Sham ###
## Reordenar variables
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

TMS_HDRSSHAM$diff <- apply(TMS_HDRSSHAM[,c('tot_score.y', 'tot_score.x')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } ); TMS_HDRSSHAM$diff

# ccqg

TMS_CCQG$gstage <- interaction(TMS_CCQG$group,TMS_CCQG$stage, drop = T); TMS_CCQG$gstage

TMS_CCQGT0T <- within(TMS_CCQG, {
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
TMS_CCQGT0S <- na.omit(TMS_CCQGT0S); TMS_CCQGT0S

TMS_CCQGT1S <- within(TMS_CCQG, {
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
TMS_CCQGT1S <- na.omit(TMS_CCQGT1S); TMS_CCQGT1S

TMS_CCQGSHAM <- merge(TMS_CCQGT0S, TMS_CCQGT1S, by = "rid"); head(TMS_CCQGSHAM)

TMS_CCQGSHAM$diff <- apply(TMS_CCQGSHAM[,c('ccq_g.y', 'ccq_g.x')], 1, function(x) { (x[1]-x[2])/x[2] * 100 } ); TMS_CCQGSHAM$diff


CCQG_HDRS <- merge(TMS_CCQGSHAM, TMS_HDRSSHAM, by = "rid")

## Prueba de correlación, prueba de Hipotesis 3
plot(CCQG_HDRS$diff.x, CCQG_HDRS$diff.y, col = "blue", main = "x against y", xlab = "x", ylab = "y")

corr <- cor.test(x=CCQG_HDRS$diff.x, y=CCQG_HDRS$diff.y, method = 'spearman'); corr


# Hacer lo mismo pero con los de tratamiento

