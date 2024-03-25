### Bases de datos de proyecto TMS - 02/10/18
### Por Alan Davalos

### Base HDRS

TMS_HDRS<- read.csv("/home/alan/Documentos/Maestría/Protocolo/Addimex bases/Addimex_clinical_Feb20/csv/HDRS.csv", header = TRUE)
TMS_HDRS$group <- factor(TMS_HDRS$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"))
TMS_HDRS$rid <- factor(TMS_HDRS$rid)
TMS_HDRS$stage <- factor(TMS_HDRS$stage)
TMS_HDRS$score_categories <- factor(TMS_HDRS$score_categories,
                                    levels = c(1, 2, 3, 4, 5),
                                    labels = c("normal", "minor depression", "less than major depressive", "major depressive", "more than major depressive"))

##Descriptivas
table(TMS_HDRS$group,TMS_HDRS$stage)
summary(TMS_HDRS)



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

variablehdrs <-c("tot_score") 
stats.datahdrs <- sapply(TMS_HDRS[variablehdrs], mystats)
stats.datahdrs <- t(stats.datahdrs)
stats.datahdrs <- round(stats.datahdrs, digits=2)
stats.datahdrs

##Graficas cada estadio por separado
library(ggplot2)
ggplot(TMS_HDRS, aes(x = rid, y = tot_score, color = stage)) +
  geom_point(size = 2, alpha = .8) +
  labs(x = "RID", y = "score", 
       title = "Hamilton Depression Rating Scale") 

a <- ggplot(TMS_HDRS, aes(x = stage, y = tot_score, fill = group))
a + geom_boxplot()

# T0
HDRST0 <- TMS_HDRS[which(TMS_HDRS$stage=="T0"),]

hist(HDRST0$tot_score, probability = T, main = "Histograma de distribución T0", breaks=30); lines(density(HDRST0$tot_score, na.rm = T), col=2)

qqnorm(HDRST0$tot_score); qqline(HDRST0$tot_score, col="red")
shapiro.test(HDRST0$tot_score)

summary(HDRST0$tot_score)
sd(HDRST0$tot_score)

HDRST0$Obs[HDRST0$tot_score<8] <- "Asintomatico"
HDRST0$Obs[HDRST0$tot_score>= 8 & HDRST0$tot_score<14] <- "Ligera"
HDRST0$Obs[HDRST0$tot_score>= 14 & HDRST0$tot_score<19] <- "Moderada"
HDRST0$Obs[HDRST0$tot_score>= 19 & HDRST0$tot_score<23] <- "Grave"
HDRST0$Obs[HDRST0$tot_score>=23] <- "Muy grave"
tail(HDRST0[c(22,24)])

table(HDRST0$group, HDRST0$Obs)

#Cada grupo por separado
HDRST0sh <- HDRST0[which(HDRST0$group=="Sham"),]
HDRST0tx <- HDRST0[which(HDRST0$group=="Treatment"),]

summary(HDRST0sh$tot_score)
sd(HDRST0sh$tot_score)

summary(HDRST0tx$tot_score)
sd(HDRST0tx$tot_score)

# T1
HDRST1 <- TMS_HDRS[which(TMS_HDRS$stage=="T1"),]

hist(HDRST1$tot_score, probability = T, main = "Histograma de distribución T1", breaks=30); lines(density(HDRST1$tot_score, na.rm = T), col=2)

qqnorm(HDRST1$tot_score); qqline(HDRST1$tot_score, col="red")

shapiro.test(HDRST1$tot_score)

#Cada variable por separado
HDRS1sh <- HDRST1[which(HDRST1$group=="Sham"),]
HDRS1tx <- HDRST1[which(HDRST1$group=="Treatment"),]

summary(HDRST1$tot_score)
sd(HDRST1$tot_score)

summary(HDRS1sh$tot_score)
sd(HDRS1sh$tot_score)
summary(HDRS1tx$tot_score)
sd(HDRS1tx$tot_score)

# T1-4
HDRST14 <- TMS_HDRS[which(TMS_HDRS$stage=="T1-4"),]

summary(HDRST14$tot_score)
sd(HDRST14$tot_score)


# T2
HDRST2 <- TMS_HDRS[which(TMS_HDRS$stage=="T2"),]

hist(HDRST2$tot_score, probability = T, main = "Histograma de distribución T2", breaks=30); lines(density(HDRST2$tot_score, na.rm = T), col=2)

qqnorm(HDRST2$tot_score); qqline(HDRST2$tot_score, col="red")

shapiro.test(HDRST2$tot_score)

#Cada variable por separado
HDRST2sh <- HDRST2[which(HDRST2$group=="Sham"),]
HDRST2tx <- HDRST2[which(HDRST2$group=="Treatment"),]

summary(HDRST2$tot_score)
sd(HDRST2$tot_score)

summary(HDRST2sh$tot_score)
sd(HDRST2sh$tot_score)
summary(HDRST2tx$tot_score)
sd(HDRST2tx$tot_score)

### Gráficos comparativos

##Comparación T0 y T1
# Filtrar variables
library(dplyr)
library(ggplot2)

hdrs0 <- c("T0", "T1")
TMS_HDRS01<- filter(TMS_HDRS, stage %in% hdrs0) 

# Grafico de boxplot
b <- ggplot(TMS_HDRS01, aes(x = stage, y = tot_score, fill = group)) 
b + geom_boxplot() +  labs(title ="Puntaje de Hamilton de Depresión entre grupos", x = 'Estadio', y = 'Puntaje total') 

# Gráfico de medias y DE
library(plyr)
xsumm <- ddply(TMS_HDRS01, .(stage, group), summarise, m = mean(tot_score), s = sd(tot_score))

ggplot(xsumm, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Figura 4. Media y DE de escala de Hamilton de Depresión", x = 'Estadio', y = 'Puntaje total', subtitle="Comparación T0 y T1 entre grupos") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue'))

## Comportamiento de grupo Sham
# Filtrar variables
library(dplyr)

Sham <- c("Sham")
TMS_HDRSsham<- filter(TMS_HDRS, group %in% Sham) 

xsumms <- ddply(TMS_HDRSsham, .(stage, group), summarise, m = mean(tot_score), s = sd(tot_score))

ggplot(xsumms, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Media y DE de escala de Hamilton de Depresión", x = 'Estadio', y = 'Puntaje total', subtitle="Comportamiento de grupo Sham") + scale_colour_manual(breaks = c('Sham'), values = c('darkgoldenrod3'))

## Comportamiento de ambos grupos tras el tratamiento
library(tidyverse)
TMS_HDRS <- as_tibble(TMS_HDRS)

datos <- TMS_HDRS %>% 
  select(rid, group, stage, tot_score)

sham <-
  datos %>% 
  filter(group == "Sham", stage != "T0") %>% 
  mutate(
    stage = case_when(
      stage == "T1" ~ "pre EMTr",
      stage == "T1-4" ~ "2 semanas",
      stage == "T2"~ "3 meses")
  ) 

sham <- na.omit(sham)

treatment <- datos %>% 
  filter(group == "Treatment") %>% 
  mutate(
    stage = case_when(
      stage == "T0" ~ "pre EMTr",
      stage == "T1" ~ "2 semanas",
      stage == "T2"~ "3 meses")
  ) 
treatment <- na.omit(treatment)
 
hdrsboth <- sham %>% 
  rbind(treatment)

hdrsboth <- hdrsboth %>% 
  mutate(
    stage = factor(stage,
                   levels = c("pre EMTr", "2 semanas",
                              "3 meses")))

xsummt2 <- ddply(hdrsboth, .(stage, group), summarise, m = mean(tot_score), s = sd(tot_score))

hamlongit2 <- ggplot(xsummt2, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Figura 6. Media y DE de escala de Hamilton de Depresión", x = 'Estadio', y = 'Puntaje total', subtitle="Seguimiento a 2 semanas y 3 meses de la intervención") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue')); hamlongit2

## Comportamiento global incluyendo al T1-4

TMS_HDRS <- as_tibble(TMS_HDRS)

datos4 <- TMS_HDRS %>% 
  select(rid, group, stage, tot_score)

sham4 <-
  datos4 %>% 
  filter(group == "Sham", stage != "T3", stage != "T4", stage != "T5") 

sham4 <- na.omit(sham4)

treatment4 <- datos4 %>% 
  filter(group == "Treatment", stage != "T3", stage != "T4", stage != "T5")  

treatment4 <- na.omit(treatment4)

hdrsboth4 <- sham4 %>% 
  rbind(treatment4)

hdrsboth4 <- hdrsboth4 %>% 
  mutate(
    stage = factor(stage,
                   levels = c("T0", "T1", "T1-4",
                              "T2")))

xsummt4 <- ddply(hdrsboth4, .(stage, group), summarise, m = mean(tot_score), s = sd(tot_score))

hamlongit4 <- ggplot(xsummt4, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Figura 6. Media y DE de escala de Hamilton de Depresión", x = 'Estadio', y = 'Puntaje total', subtitle="Seguimiento a 2 semanas y 3 meses de la intervención") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue')); hamlongit4

## Comportamiento global sin incluir al T1-4

TMS_HDRS <- as_tibble(TMS_HDRS)

datos6 <- TMS_HDRS %>% 
  select(rid, group, stage, tot_score)

sham6 <-
  datos6 %>% 
  filter(group == "Sham", stage != "T1-4", stage != "T3", stage != "T4", stage != "T5") 

sham6 <- na.omit(sham6)

treatment6 <- datos6 %>% 
  filter(group == "Treatment", stage != "T1-4", stage != "T3", stage != "T4", stage != "T5")  

treatment6 <- na.omit(treatment6)

hdrsboth6 <- sham6 %>% 
  rbind(treatment6)

hdrsboth6 <- hdrsboth6 %>% 
  mutate(
    stage = factor(stage,
                   levels = c("T0", "T1",
                              "T2")))

xsummt6 <- ddply(hdrsboth6, .(stage, group), summarise, m = mean(tot_score), s = sd(tot_score))

hamlongit6 <- ggplot(xsummt6, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Figura 7. Media y DE de escala de Hamilton de Depresión", x = 'Estadio', y = 'Puntaje total', subtitle="Seguimiento a 2 semanas y 3 meses de la intervención") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue')); hamlongit6

## Comportamiento global en ambos grupos
library(tidyverse)
TMS_HDRS <- as_tibble(TMS_HDRS)

datos3 <- TMS_HDRS %>% 
  select(rid, group, stage, tot_score)

xsummt3 <- ddply(datos3, .(stage, group), summarise, m = mean(tot_score), s = sd(tot_score))

hamlongit3 <- ggplot(xsummt3, aes(x = stage, y = m)) +
  geom_errorbar(aes(ymin = m - s, ymax = m + s, colour = group),
                position = position_dodge(width = 0.3),
                size = 1, width = 0.4) +
  geom_line(aes(colour=group, group=group)) + 
  geom_point(aes(colour=group),              
             size=3)   +
  labs(title ="Media y DE de escala de Hamilton de Depresión", x = 'Estadio', y = 'Puntaje total', subtitle="Comportamiento global") + scale_colour_manual(breaks = c('Sham', 'Treatment'), values = c('darkgoldenrod3', 'midnightblue')); hamlongit3


### Contraste de Hipótesis 2. Disminuye los síntomas depresivos más que el placebo las 2 semanas de tratamiento
## ANOVA 2x2
aov_hdrs1 <- aov(tot_score~stage*group,TMS_HDRS01); aov_hdrs1; summary(aov_hdrs1)

TukeyHSD(aov_hdrs1) #Contraste entre grupos

test <- TMS_HDRS01 %>% filter(!rid %in% c(5, 9, 14, 28, 29))
afex::aov_car(tot_score ~ group * stage + Error(rid/stage), data = test)

### Contraste de Hipótesis 2a. Se Mantiene la disminución en los síntomas depresivos hasta los 3 meses de tratamiento.
## ANOVA Con todos los grupos
aov_hdrs2 <- aov(tot_score~stage*group, hdrsboth6); aov_hdrs2; summary(aov_hdrs2)

TukeyHSD(aov_hdrs2) #Contraste entre grupos

test2 <- hdrsboth6 %>% filter(!rid %in% c(5, 9, 14, 28, 29))
aovhdrs2.1 <- afex::aov_car(tot_score ~ group * stage + Error(rid/stage), data = test2); aovhdrs2.1

library(emmeans)
m2 <- emmeans(aovhdrs2.1, ~ group*stage); m2
pairs(m2)

## ANOVA de medidas repetidas
library(afex)
aov_hdrs3 <- aov_car(tot_score ~ group * stage + Error(rid/stage), data = hdrsboth6); aov_hdrs3
summary(aov_hdrs3)

## Comparación solo con ciertos items

