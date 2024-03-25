### Bases de datos de proyecto TMS
### Por Alan Davalos

### Base CCQN

TMS_CCQN<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/CCQN.csv", header = TRUE)
TMS_CCQN$group <- factor(TMS_CCQN$group,
                                levels = c(1, 2),
                                labels = c("Sham", "Treatment"))
TMS_CCQN$rid <- factor(TMS_CCQN$rid)
TMS_CCQN$stage <- factor(TMS_CCQN$stage)

##Descriptivas
table(TMS_CCQN$group,TMS_CCQN$stage)
table(TMS_CCQN$rid,TMS_CCQN$stage)
summary(TMS_CCQN)
library('Hmisc')
describe(TMS_CCQN)

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

variablescqn <-c("ccq_n") 
stats.datacqn <- sapply(TMS_CCQN[variablescqn], mystats)
stats.datacqn <- t(stats.datacqn)
stats.datacqn <- round(stats.datacqn, digits=2)
stats.datacqn

library(ggplot2)
ggplot(TMS_CCQN, aes(x = rid, y = ccq_n, color = stage)) +
  geom_point(size = 2, alpha = .8) +
  labs(x = "RID", y = "score", 
       title = "CCQ-N")

e <- ggplot(TMS_CCQN, aes(x = stage, y = ccq_n, fill = group)); e + geom_boxplot()

# T0
CCQN0 <- TMS_CCQN[which(TMS_CCQN$stage=="T0"),]

hist(CCQN0$ccq_n, probability = T, main = "Histograma de distribución T0", breaks=15); lines(density(CCQN0$ccq_n, na.rm = T), col=2)

qqnorm(CCQN0$ccq_n); qqline(CCQN0$ccq_n, col="red")

shapiro.test(CCQN0$ccq_n)

# T1
CCQN1 <- TMS_CCQN[which(TMS_CCQN$stage=="T1"),]

hist(CCQN1$ccq_n, probability = T, main = "Histograma de distribución T1", breaks=15); lines(density(CCQN1$ccq_n, na.rm = T), col=2)

qqnorm(CCQN1$ccq_n); qqline(CCQN1$ccq_n, col="red")

shapiro.test(CCQN1$ccq_n)

# T2
CCQN2 <- TMS_CCQN[which(TMS_CCQN$stage=="T2"),]

hist(CCQN2$ccq_n, probability = T, main = "Histograma de distribución T2", breaks=15); lines(density(CCQN2$ccq_n, na.rm = T), col=2)

qqnorm(CCQN2$ccq_n); qqline(CCQN2$ccq_n, col="red")

shapiro.test(CCQN2$ccq_n)

## Contraste de hipótesis
## H1. Disminuye el craving más que el placebo a las 2 semanas de tratamiento.
#Reordenar variables
head(TMS_CCQN)

TMS_CCQN$gstage <- interaction(TMS_CCQN$group,TMS_CCQN$stage, drop = T); TMS_CCQN$gstage

TMS_CCQN1 <- within(TMS_CCQN, {
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
TMS_CCQN1 <- na.omit(TMS_CCQN1); TMS_CCQN1

TMS_CCQN1$Obs <- factor(TMS_CCQN1$Obs, levels = c("ShamT0", "ShamPRE", "TxPRE", "TxPost2wk"))

## Grafico de medias para ANOVA
library(gplots)
plotmeans(TMS_CCQN1$ccq_n ~ TMS_CCQN1$Obs, 
          xlab="Stage", ylab= "Total score", main="Grafico de medias con IC 95% - CCQN")

## ANOVA
aov_ccqn <- aov(ccq_n~Obs,TMS_CCQN1); aov_ccqn; summary(aov_ccqn)

TukeyHSD(aov_ccqn)#Comparaciones multiples

# No hay diferencia significativa entre grupos

#No paramétricas
kruskal.test(ccq_n~Obs,TMS_CCQN1)
pairwise.wilcox.test(TMS_CCQN1$ccq_n, TMS_CCQN1$Obs,
                     p.adjust.method = "BH")

## Descripción Todos los grupos

TMS_CCQNcomplete <- within(TMS_CCQN, {
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

TMS_CCQNcomplete$Obs <- factor(TMS_CCQNcomplete$Obs, levels = c("ShamT0", "ShamPRE", "ShamPost2wk", "Sham3Months", "Sham6Months", "Sham12Months",  "ShamT5", "TxPRE", "TxPost2wk", "Tx3Months", "Tx6Months", "Tx12Months"))

library(gplots)
plotmeans(TMS_CCQNcomplete$ccq_n ~ TMS_CCQNcomplete$Obs, 
          xlab="Stage", ylab= "Total score", main="Grafico de medias con IC 95% - CCQG todos los grupos")

