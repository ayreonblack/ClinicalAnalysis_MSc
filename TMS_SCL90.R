### Bases de datos de proyecto TMS
### Por Alan Davalos

### Base SCL-90

TMS_SCL90<- read.csv("/home/alan/Documentos/Maestría/Protocolo/Addimex bases/Addimex_clinical_Feb20/csv/SCL-90-R.csv", header = TRUE)
TMS_SCL90$group <- factor(TMS_SCL90$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"))
TMS_SCL90$rid <- factor(TMS_SCL90$rid)
TMS_SCL90$stage <- factor(TMS_SCL90$stage)

TMS_SCL90$dep <- as.numeric(TMS_SCL90$dep)
TMS_SCL90$anx <- as.numeric(TMS_SCL90$anx)
##Descriptivas
table(TMS_SCL90$group,TMS_SCL90$stage)
summary(TMS_SCL90)

TMS_SCL90$q58

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

variablescl <-c("dep", "gsi") 
stats.datascl <- sapply(TMS_SCL90[variablescl], mystats)
stats.datascl <- t(stats.datascl)
stats.datascl <- round(stats.datascl, digits=2)
stats.datascl


library(ggplot2)
ggplot(TMS_SCL90, aes(x = rid, y = dep, color = stage)) +
  geom_point(size = 2, alpha = .8) +
  labs(x = "RID", y = "score", 
       title = "SCL-90 Depression Sub Scale")

c <- ggplot(TMS_SCL90, aes(x = stage, y = dep, fill = group))
c + geom_boxplot()

table(TMS_SCL90$group,TMS_SCL90$stage)

# T0
SCL0 <- TMS_SCL90[which(TMS_SCL90$stage=="T0"),]

hist(SCL0$dep, probability = T, main = "Histograma de distribución T0", breaks=15)
lines(density(SCL0$dep, na.rm = T), col=2)

qqnorm(SCL0$dep)
qqline(SCL0$dep, col="red")

shapiro.test(SCL0$dep)

# T1
SCL1 <- TMS_SCL90[which(TMS_SCL90$stage=="T1"),]

hist(SCL1$dep, probability = T, main = "Histograma de distribución T1", breaks=15)
lines(density(SCL1$dep, na.rm = T), col=2)

qqnorm(SCL1$dep)
qqline(SCL1$dep, col="red")

shapiro.test(SCL1$dep)

# T2
SCL2 <- TMS_SCL90[which(TMS_SCL90$stage=="T2"),]

hist(SCL2$dep, probability = T, main = "Histograma de distribución T1", breaks=15)
lines(density(SCL2$dep, na.rm = T), col=2)

qqnorm(SCL2$dep)
qqline(SCL2$dep, col="red")

shapiro.test(SCL2$dep)

## Contraste de hipótesis
## Hipótesis 2. Disminuye los síntomas depresivos más que el placebo las 2 semanas de tratamiento
#Reordenar variables
head(TMS_SCL90)
TMS_SCL901 <- TMS_SCL90; TMS_SCL901$gstage <- interaction(TMS_SCL901$group,TMS_SCL901$stage, drop = T); TMS_SCL901$gstage

TMS_SCL901 <- within(TMS_SCL901, {
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
TMS_SCL901 <- na.omit(TMS_SCL901); TMS_SCL901

TMS_SCL901$Obs <- factor(TMS_SCL901$Obs, levels = c("ShamT0", "ShamPRE", "TxPRE", "TxPost2wk"))

## Grafico de medias para ANOVA
library(gplots)
plotmeans(TMS_SCL901$dep ~ TMS_SCL901$Obs, 
          xlab="Stage", ylab= "puntaje subescala de depresión", main="Grafico de medias con IC 95% - SCL90-DEP H2")

## ANOVA
aov_scl901 <- aov(dep~Obs,TMS_SCL901); aov_scl901; summary(aov_scl901)

#Comparaciones multiples
TukeyHSD(aov_scl901)

#Diferencia en ambos grupos, Sham p=.039, Tx p=.029. No mejoría importante en intervención

## Hipotesis 2a
#Reordenar variables
head(TMS_SCL90)

TMS_SCL90$gstage <- interaction(TMS_SCL90$group,TMS_SCL90$stage, drop = T); TMS_SCL90$gstage

TMS_SCL90 <- within(TMS_SCL90, {
  Obs <- NA
  Obs[gstage == "Sham.T0" ] <- "ShamT0"
  Obs[gstage == "Treatment.T0" ] <- "TxPRE"
  Obs[gstage == "Sham.T1" ] <- "ShamPRE"
  Obs[gstage == "Treatment.T1" ] <- "TxPost2wk"
  Obs[gstage == "Sham.T1-4" ] <- "ShamPost2wk"
  Obs[gstage == "Sham.T2" ] <- "Sham3Months"
  Obs[gstage == "Treatment.T2" ] <- "Tx3Months"
  Obs[gstage == "Sham.T3" ] <- NA
  Obs[gstage == "Treatment.T3" ] <- NA
  Obs[gstage == "Sham.T4" ] <- NA
  Obs[gstage == "Treatment.T4" ] <- NA
  Obs[gstage == "Sham.T5" ] <- NA
})
TMS_SCL90 <- na.omit(TMS_SCL90)
TMS_SCL90

TMS_SCL90$Obs <- factor(TMS_SCL90$Obs, levels = c("ShamT0", "ShamPRE", "ShamPost2wk", "Sham3Months", "TxPRE", "TxPost2wk", "Tx3Months"))

## Grafico de medias para ANOVA
library(gplots)
plotmeans(TMS_SCL90$dep ~ TMS_SCL90$Obs, 
          xlab="Stage", ylab= "Puntaje sub escala de depresión", main="Grafico de medias con IC 95% - SCL90")

## ANOVA
library(dplyr)
scl0 <- c("T0", "T1")
TMS_SCL9001<- filter(TMS_SCL90, stage %in% scl0) 

aov_scl <- aov(dep~stage*group,TMS_SCL9001); aov_scl; summary(aov_scl)

#Comparaciones multiples
TukeyHSD(aov_scl)

#Se mantiene mejoría en ambos grupos