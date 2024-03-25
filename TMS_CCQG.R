### Bases de datos de proyecto TMS
### Por Alan Davalos

### Base CCQG

TMS_CCQG<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/CCQG.csv", header = TRUE)
TMS_CCQG$group <- factor(TMS_CCQG$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"))
TMS_CCQG$rid <- factor(TMS_CCQG$rid)
TMS_CCQG$stage <- factor(TMS_CCQG$stage)

##Descriptivas
table(TMS_CCQG$group,TMS_CCQG$stage)
summary(TMS_CCQG)
library('Hmisc')
describe(TMS_CCQG)

TMS_CCQG$q22

mystats <-function(x, na.omit=TRUE) {
  if(na.omit)
    x <- x[!is.na(x)]
  m <- mean (x)
  n <- length (x)
  s <- sd (x)
  min <- min(x)
  max <- max (x)
  q <- quantile (x, probs=c(0.25, 0.50, 0.75))
  skew <- sum ((x-m)^3/s^3)/n
  kurt <- sum ((x-m)^4/s^4)/n-3
  return(c(n=n, mean=m, stdev=s, min=min, max=max, quantiles=q, kurtosis=kurt, skew=skew))
}

variablescqg <-c("ccq_g") 
stats.datacqg <- sapply(TMS_CCQG[variablescqg], mystats)
stats.datacqg <- t(stats.datacqg)
stats.datacqg <- round(stats.datacqg, digits=2)
stats.datacqg

library(ggplot2)
ggplot(TMS_CCQG, aes(x = rid, y = ccq_g, color = stage)) +
  geom_point(size = 2, alpha = .8) +
  labs(x = "RID", y = "score", 
       title = "CCQ-G")

d <- ggplot(TMS_CCQG, aes(x = stage, y = ccq_g, fill = group))
d + geom_boxplot()

# T0
CCQG0 <- TMS_CCQG[which(TMS_CCQG$stage=="T0"),]

hist(CCQG0$ccq_g, probability = T, main = "Histograma de distribución T0", breaks=15)
lines(density(CCQG0$ccq_g, na.rm = T), col=2)

qqnorm(CCQG0$ccq_g)
qqline(CCQG0$ccq_g, col="red")

shapiro.test(CCQG0$ccq_g)

# T1
CCQG1 <- TMS_CCQG[which(TMS_CCQG$stage=="T1"),]

hist(CCQG1$ccq_g, probability = T, main = "Histograma de distribución T1", breaks=15)
lines(density(CCQG1$ccq_g, na.rm = T), col=2)

qqnorm(CCQG1$ccq_g)
qqline(CCQG1$ccq_g, col="red")

shapiro.test(CCQG1$ccq_g)
# T2
CCQG2 <- TMS_CCQG[which(TMS_CCQG$stage=="T2"),]

hist(CCQG2$ccq_g, probability = T, main = "Histograma de distribución T2", breaks=15)
lines(density(CCQG2$ccq_g, na.rm = T), col=2)

qqnorm(CCQG2$ccq_g)
qqline(CCQG1$ccq_g, col="red")

shapiro.test(CCQG2$ccq_g)

## Contraste de hipótesis
## H1. Disminuye el craving más que el placebo a las 2 semanas de tratamiento.
#Reordenar variables
head(TMS_CCQG)

TMS_CCQG$gstage <- interaction(TMS_CCQG$group,TMS_CCQG$stage, drop = T); TMS_CCQG$gstage

TMS_CCQG <- within(TMS_CCQG, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- "ShamT0"
  Obs[gstage == "Treatment.T0" ] <- "TxPRE"
  Obs[gstage == "Sham.T1" ] <- "ShamPRE"
  Obs[gstage == "Treatment.T1" ] <- "TxPost2wk"
  Obs[gstage == "Sham.T1-4" ] <- NA
  Obs[gstage == "Sham.T2" ] <- NA
  Obs[gstage == "Treatment.T2" ] <- NA
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_CCQG <- na.omit(TMS_CCQG); TMS_CCQG

TMS_CCQG$Obs <- factor(TMS_CCQG$Obs, levels = c("ShamT0", "ShamPRE", "TxPRE", "TxPost2wk"))

## Grafico de medias para ANOVA
library(gplots)
plotmeans(TMS_CCQG$ccq_g ~ TMS_CCQG$Obs, 
          xlab="Stage", ylab= "Total score", main="Grafico de medias con IC 95% - CCQG")

## ANOVA
aov_ccqg <- aov(ccq_g~Obs,TMS_CCQG); aov_ccqg; summary(aov_ccqg)

TukeyHSD(aov_ccqg) #Comparaciones multiples

#Diferencia en ambos grupos, Sham p=.04, Tx p=.09. No mejoría en intervención

#No paramétricas
kruskal.test(ccq_g~Obs,TMS_CCQG)
pairwise.wilcox.test(TMS_CCQG$ccq_g, TMS_CCQG$Obs,
                     p.adjust.method = "BH")

## Descripción Todos los grupos

TMS_CCQGcomplete <- within(TMS_CCQG, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- "ShamT0"
  Obs[gstage == "Treatment.T0" ] <- "TxPRE"
  Obs[gstage == "Sham.T1" ] <- "ShamPRE"
  Obs[gstage == "Treatment.T1" ] <- "TxPost2wk"
  Obs[gstage == "Sham.T1-4" ] <- "ShamPost2wk"
  Obs[gstage == "Sham.T2" ] <- "Sham3Months"
  Obs[gstage == "Treatment.T2" ] <- "Tx3Months"
  Obs[gstage == "Sham.T3" ] <- "Sham6Months"
  Obs[gstage == "Treatment.T3" ] <- "Tx6Months"
  Obs[gstage == "Sham.T4" ] <- "Sham12Months"
  Obs[gstage == "Treatment.T4" ] <- "Tx12Months"
  Obs[gstage == "Sham.T5" ] <- "ShamT5"
})

TMS_CCQGcomplete$Obs <- factor(TMS_CCQGcomplete$Obs, levels = c("ShamT0", "ShamPRE", "ShamPost2wk", "Sham3Months", "Sham6Months", "Sham12Months",  "ShamT5", "TxPRE", "TxPost2wk", "Tx3Months", "Tx6Months", "Tx12Months"))

library(gplots)
plotmeans(TMS_CCQGcomplete$ccq_g ~ TMS_CCQGcomplete$Obs, 
          xlab="Stage", ylab= "Total score", main="Grafico de medias con IC 95% - CCQG todos los grupos")
