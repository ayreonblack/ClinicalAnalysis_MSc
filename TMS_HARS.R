### Bases de datos de proyecto TMS - 02/10/18
### Por Alan Davalos

### Base HARS

TMS_HARS<- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/HARS.csv", header = TRUE)
TMS_HARS$group <- factor(TMS_HARS$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"))
TMS_HARS$rid <- factor(TMS_HARS$rid)
TMS_HARS$date <- factor(TMS_HARS$date)
TMS_HARS$stage <- factor(TMS_HARS$stage)
TMS_HARS$hars_categories <- factor(TMS_HARS$hars_categories,
                         levels = c(1, 2, 3),
                         labels = c("mild", "moderate", "severe"))
##Descriptivas
table(TMS_HARS$group,TMS_HARS$stage)
summary(TMS_HARS)

TMS_HARS$hars_categories

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

variablehars <-c("hars_tot") 
stats.datahars <- sapply(TMS_HARS[variablehars], mystats)
stats.datahars <- t(stats.datahars)
stats.datahars <- round(stats.datahars, digits=2)
stats.datahars

##Graficas cada estadio por separado
library(ggplot2)
ggplot(TMS_HARS, aes(x = rid, y = hars_tot, color = stage)) +
  geom_point(size = 2, alpha = .8) +
  labs(x = "RID", y = "score", 
       title = "Hamilton Anxiety Rating Scale") 

a <- ggplot(TMS_HARS, aes(x = stage, y = hars_tot, fill = group))
a + geom_boxplot()

# T0
HARST0 <- TMS_HARS[which(TMS_HARS$stage=="T0"),]

hist(HARST0$hars_tot, probability = T, main = "Histograma de distribución T0", breaks=30); lines(density(HARST0$hars_tot, na.rm = T), col=2)

qqnorm(HARST0$hars_tot); qqline(HARST0$hars_tot, col="red")

shapiro.test(HARST0$hars_tot)

summary(HARST0$hars_tot)
sd(HARST0$hars_tot)

HARST0$Obs[HARST0$hars_tot<8] <- "Asintomatico"
HARST0$Obs[HARST0$hars_tot>= 8 & HARST0$hars_tot<15] <- "Ligera"
HARST0$Obs[HARST0$hars_tot>= 15 & HARST0$hars_tot<24] <- "Moderada"
HARST0$Obs[HARST0$hars_tot>=24] <- "grave"
tail(HARST0[c(19,21)])

table(HARST0$group, HARST0$Obs)

#Cada grupo por separado
HARST0sh <- HARST0[which(HARST0$group=="Sham"),]
HARST0tx <- HARST0[which(HARST0$group=="Treatment"),]

summary(HARST0sh$hars_tot)
sd(HARST0sh$hars_tot)

summary(HARST0tx$hars_tot)
sd(HARST0tx$hars_tot)

# T1
HARST1 <- TMS_HARS[which(TMS_HARS$stage=="T1"),]

hist(HARST1$hars_tot, probability = T, main = "Histograma de distribución T1", breaks=30); lines(density(HARST1$hars_tot, na.rm = T), col=2)

qqnorm(HARST1$hars_tot); qqline(HARST1$hars_tot, col="red")

shapiro.test(HARST1$hars_tot)

#Cada variable por separado
HARST1sh <- HARST1[which(HDRST1$group=="Sham"),]
HARST1tx <- HARST1[which(HDRST1$group=="Treatment"),]

summary(HARST1$hars_tot)
sd(HARST1$hars_tot)

summary(HARST1sh$hars_tot)
sd(HARST1sh$hars_tot)
summary(HARST1tx$hars_tot)
sd(HARST1tx$hars_tot)

# T1-4
HARST14 <- TMS_HARS[which(TMS_HARS$stage=="T1-4"),]

summary(HARST14$hars_tot)
sd(HARST14$hars_tot)

# T2
HARST2 <- TMS_HARS[which(TMS_HARS$stage=="T2"),]

hist(HARST2$hars_tot, probability = T, main = "Histograma de distribución T2", breaks=30); lines(density(HARST2$hars_tot, na.rm = T), col=2)

qqnorm(HARST2$hars_tot); qqline(HARST2$hars_tot, col="red")

shapiro.test(HARST2$hars_tot)

#Cada variable por separado
HARST2sh <- HARST2[which(HARST2$group=="Sham"),]
HARST2tx <- HARST2[which(HARST2$group=="Treatment"),]

summary(HARST2$hars_tot)
sd(HARST2$hars_tot)

summary(HARST2sh$hars_tot)
sd(HARST2sh$hars_tot)
summary(HARST2tx$hars_tot)
sd(HARST2tx$hars_tot)

### Gráficos comparativos

##Comparación T0 y T1
# Filtrar variables
library(dplyr)
library(ggplot2)

hars0 <- c("T0", "T1")
TMS_HARS01<- filter(TMS_HARS, stage %in% hars0) 

# Grafico de boxplot
b <- ggplot(TMS_HARS01, aes(x = stage, y = hars_tot, fill = group)) 
b + geom_boxplot() +  labs(title ="Puntaje de Hamilton de Ansiedad entre grupos", x = 'Estadio', y = 'Puntaje total') 

# Gráfico de medias y DE
library(plyr)
xsumma <- ddply(TMS_HARS01, .(stage, group), summarise, m = mean(hars_tot), s = sd(hars_tot))

ggplot(xsumma, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Figura 3. Media y DE de escala de Hamilton de Ansiedad", x = 'Estadio', y = 'Puntaje total', subtitle="Comparación T0 y T1 entre grupos") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue'))

## Comportamiento de grupo Sham
# Filtrar variables
library(dplyr)

Sham <- c("Sham")
TMS_HARSsham<- filter(TMS_HARS, group %in% Sham) 

xsummssa <- ddply(TMS_HARSsham, .(stage, group), summarise, m = mean(hars_tot), s = sd(hars_tot))

ggplot(xsummssa, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Media y DE de escala de Hamilton de Ansiedad", x = 'Estadio', y = 'Puntaje total', subtitle="Comportamiento de grupo Sham") + scale_colour_manual(breaks = c('Sham'), values = c('darkgoldenrod3'))

## Comportamiento de ambos grupos tras el tratamiento
library(tidyverse)
TMS_HARS <- as_tibble(TMS_HARS)

datos2 <- TMS_HARS %>% 
  select(rid, group, stage, hars_tot)

sham2 <-
  datos2 %>% 
  filter(group == "Sham", stage != "T0") %>% 
  mutate(
    stage = case_when(
      stage == "T1" ~ "T0",
      stage == "T1-4" ~ "T1",
      stage == "T2"~ "T2")
  ) 

sham2 <- na.omit(sham2)

treatment2 <- datos2 %>% 
  filter(group == "Treatment") %>% 
  mutate(
    stage = case_when(
      stage == "T0" ~ "T0",
      stage == "T1" ~ "T1",
      stage == "T2"~ "T2")
  ) 
treatment2 <- na.omit(treatment2)

harsboth <- sham2 %>% 
  rbind(treatment2)

harsboth <- harsboth %>% 
  mutate(
    stage = factor(stage,
                   levels = c("T0", "T1",
                              "T2", "T3")))

xsummt3 <- ddply(harsboth, .(stage, group), summarise, m = mean(hars_tot), s = sd(hars_tot))

hamalongit <- ggplot(xsummt3, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Media y DE de escala de Hamilton de Ansiedad", x = 'Estadio', y = 'Puntaje total', subtitle="Comportamiento antes y despues de la intervención") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue')); hamalongit

## Comportamiento a los 3 meses incluyendo al T1-4

datos5 <- TMS_HARS %>% 
  select(rid, group, stage, hars_tot)

sham5 <-
  datos5 %>% 
  filter(group == "Sham", stage != "T3",  stage != "T4",  stage != "T5")

sham5 <- na.omit(sham5)

treatment5 <- datos5 %>% 
  filter(group == "Treatment", stage != "T3",  stage != "T4",  stage != "T5") 

treatment5 <- na.omit(treatment5)

harsboth5 <- sham5 %>% 
  rbind(treatment5)

harsboth5 <- harsboth5 %>% 
  mutate(
    stage = factor(stage,
                   levels = c("T0", "T1", "T1-4",
                              "T2")))

xsummt5 <- ddply(harsboth5, .(stage, group), summarise, m = mean(hars_tot), s = sd(hars_tot))

hamalongit5 <- ggplot(xsummt5, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Figura 7. Media y DE de escala de Hamilton de Ansiedad", x = 'Estadio', y = 'Puntaje total', subtitle="Seguimiento a 2 semanas y 3 meses de la intervención") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue')); hamalongit5

## Comportamiento a los 3 meses sin el T1-4

datos7 <- TMS_HARS %>% 
  select(rid, group, stage, hars_tot)

sham7 <-
  datos7 %>% 
  filter(group == "Sham", stage != "T1-4",stage != "T3",  stage != "T4",  stage != "T5")

sham7 <- na.omit(sham7)

treatment7 <- datos7 %>% 
  filter(group == "Treatment", stage != "T3",  stage != "T4",  stage != "T5") 

treatment7 <- na.omit(treatment7)

harsboth7 <- sham7 %>% 
  rbind(treatment7)

harsboth7 <- harsboth7 %>% 
  mutate(
    stage = factor(stage,
                   levels = c("T0", "T1",
                              "T2")))

xsummt7 <- ddply(harsboth7, .(stage, group), summarise, m = mean(hars_tot), s = sd(hars_tot))

hamalongit7 <- ggplot(xsummt7, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Figura 8. Media y DE de escala de Hamilton de Ansiedad", x = 'Estadio', y = 'Puntaje total', subtitle="Seguimiento a 2 semanas y 3 meses de la intervención") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue')); hamalongit7

### Contraste de Hipótesis 2. Disminuye los síntomas depresivos más que el placebo las 2 semanas de tratamiento
## ANOVA 2x2
aov_hars1 <- aov(hars_tot~stage*group,TMS_HARS01); aov_hars1; summary(aov_hars1)

TukeyHSD(aov_hars1) #Contraste entre grupos


test <- TMS_HARS01 %>% filter(!rid %in% c(5, 9, 14, 28, 29))
afex::aov_car(hars_tot ~ group * stage + Error(rid/stage), data = test)

### Contraste de Hipótesis 2a. Se Mantiene la disminución en los síntomas depresivos hasta los 3 meses de tratamiento.
## ANOVA Con todos los grupos
aov_hars2 <- aov(hars_tot~stage*group,harsboth7); aov_hars2; summary(aov_hars2)

TukeyHSD(aov_hars2) #Contraste entre grupos

test3 <- harsboth7 %>% filter(!rid %in% c(5, 9, 14, 28, 29))
aovhars2.1 <- afex::aov_car(hars_tot ~ group * stage + Error(rid/stage), data = test3); aovhars2.1

library(emmeans)
m3 <- emmeans(aovhars2.1, ~ group*stage); m3
pairs(m3)

## ANOVA de medidas repetidas
library(afex)
aov_hars3 <- aov_car(hars_tot ~ group * stage + Error(rid/stage), data = harsboth7); aov_hars3
summary(aov_hars3)


