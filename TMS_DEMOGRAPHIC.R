## TMS datos demográficos ##
## Por Alan Dávalos ##

## Cargar bases y acomodar variables


TMS_DEMO <- read.csv("/home/alan/Documentos/Maestría/Protocolo/Addimex bases/Addimex_clinical_Feb20/csv/DEMOGRAPHIC.csv", header = T)
TMS_DEMO$rid <- factor(TMS_DEMO$rid)
TMS_DEMO$group <- factor(TMS_DEMO$group,
                                 levels = c(1, 2),
                                 labels = c("Sham", "Treatment"
                                 ))
TMS_DEMO$date <- factor(TMS_DEMO$date)
TMS_DEMO$stage <- factor(TMS_DEMO$stage)
TMS_DEMO$q1_sex <- factor(TMS_DEMO$q1_sex,
                               levels = c(1, 2),
                               labels = c("Male", "Female"
                               ))
TMS_DEMO$q4_child <- factor(TMS_DEMO$q4_child,
                          levels = c(1, 2),
                          labels = c("Yes", "No"
                          ))
TMS_DEMO$q5_civ <- factor(TMS_DEMO$q5_civ,
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("single", "married", "divorced", "separated", "widowed"
                                ))

TMS_DEMO$q6_employeedays <- factor(TMS_DEMO$q6_employeedays,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c("full time", "half time", "free lance", "scholarized", "not scholarized", "reteired", "housewife",  "unemployee"
                          ))
TMS_DEMO$q6_employeeyr <- factor(TMS_DEMO$q6_employeeyr,
                                   levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                                   labels = c("full time", "half time", "free lance", "scholarized", "not scholarized", "reteired", "housewife",  "unemployee"
                                   ))
TMS_DEMO$q6_sustance <- factor(TMS_DEMO$q6_sustance,
                            levels = c(1, 2),
                            labels = c("crack", "cocaine"
                            ))
TMS_DEMO$q7_tconsume <- as.numeric(TMS_DEMO$q7_tconsume)

library(tidyverse)
TMS_DEMO <- TMS_DEMO %>% 
  separate(col = q8_profcon, into = c("q8.1", "q8.2", "q8.3", "q8.4", "q8.5", "q8.6")) 

TMS_DEMO$q8.1 <- factor(TMS_DEMO$q8.1,
                                       levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                       labels = c("none", "general practice physician", "especialist other that psychiatrist", "psychiatrist", "psychologist", "social worker", "teacher/school counselor", "spiritual/religious counselor", "homeopathy/alternative medicine practitioner", "Other"
                                       ))
TMS_DEMO$q8.2 <- factor(TMS_DEMO$q8.2,
                        levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("none", "general practice physician", "especialist other that psychiatrist", "psychiatrist", "psychologist", "social worker", "teacher/school counselor", "spiritual/religious counselor", "homeopathy/alternative medicine practitioner", "Other"
                        ))
TMS_DEMO$q8.3 <- factor(TMS_DEMO$q8.3,
                        levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("none", "general practice physician", "especialist other that psychiatrist", "psychiatrist", "psychologist", "social worker", "teacher/school counselor", "spiritual/religious counselor", "homeopathy/alternative medicine practitioner", "Other"
                        ))
TMS_DEMO$q8.4 <- factor(TMS_DEMO$q8.4,
                        levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("none", "general practice physician", "especialist other that psychiatrist", "psychiatrist", "psychologist", "social worker", "teacher/school counselor", "spiritual/religious counselor", "homeopathy/alternative medicine practitioner", "Other"
                        ))
TMS_DEMO$q8.5 <- factor(TMS_DEMO$q8.5,
                        levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("none", "general practice physician", "especialist other that psychiatrist", "psychiatrist", "psychologist", "social worker", "teacher/school counselor", "spiritual/religious counselor", "homeopathy/alternative medicine practitioner", "Other"
                        ))
TMS_DEMO$q8.6 <- factor(TMS_DEMO$q8.6,
                        levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                        labels = c("none", "general practice physician", "especialist other that psychiatrist", "psychiatrist", "psychologist", "social worker", "teacher/school counselor", "spiritual/religious counselor", "homeopathy/alternative medicine practitioner", "Other"
                        ))
TMS_DEMO$q12_engbe <- factor(TMS_DEMO$q12_engbe,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("never", "ocasional", "monthly", "one weekly", "one daily" 
                                            ))

##Descriptivas
table(TMS_DEMO$group,TMS_DEMO$stage)
summary(TMS_DEMO)

library('Hmisc')
describe(TMS_DEMO)

TMS_DEMO$q8.1

library(DataExplorer)
plot_missing(TMS_DEMO)
plot_bar(TMS_DEMO)

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

## Gráficas
library(ggplot2)
library(tidyverse)

#Grupo
TMS_DEMO %>% 
  filter(!is.na(group)) %>% 
  ggplot(aes(x = factor(1), fill = group)) + geom_bar() +
  coord_polar("y") + labs(x="Sujetos") + labs(y="") + ggtitle("Fig 1.  Sujetos por grupo")

#Sexo
TMS_DEMO %>% 
  filter(!is.na(q1_sex)) %>% 
  ggplot(aes(x = factor(1), fill = q1_sex)) + geom_bar() +
  coord_polar("y") + labs(x="Sujetos") + labs(y="") + ggtitle("Fig 2. Sexo de los participantes")


## Edad
e  <- ggplot( TMS_DEMO, aes(x= q1_age))
e + geom_bar ( colour= "black", fill= "goldenrod3") + theme_minimal(12) + labs(x="Edad") + labs(y="Sujetos") + ggtitle("Fig 3. Edad de los sujetos del estudio") 
sd(TMS_DEMO$q1_age)

# Edad de grupo de tratamiento
TMS_DEMOedadtx <- TMS_DEMO[which(TMS_DEMO$group=="Treatment"),]
TMS_DEMOedadtx$group

variableedad1 <-c("q1_age") 
stats.dataedad1 <- sapply(TMS_DEMOedadtx[variableedad1], mystats)
stats.dataedad1 <- t(stats.dataedad1)
stats.dataedad1 <- round(stats.dataedad1, digits=2)
stats.dataedad1

# Edad de grupo sham
TMS_DEMOedadsh <- TMS_DEMO[which(TMS_DEMO$group=="Sham"),]
TMS_DEMOedadsh$group

variableedad2 <-c("q1_age") 
stats.dataedad2 <- sapply(TMS_DEMOedadsh[variableedad2], mystats)
stats.dataedad2 <- t(stats.dataedad2)
stats.dataedad2 <- round(stats.dataedad2, digits=2)
stats.dataedad2

#Estado civil

TMS_DEMO %>% 
  filter(!is.na(q5_civ)) %>% 
  ggplot(aes(x = factor(1), fill = q5_civ)) + geom_bar() +
  coord_polar("y") + labs(x="Sujetos") + labs(y="") + ggtitle("Estado civil") 

g  <- ggplot( TMS_DEMO, aes(x= q5_civ))
g + geom_bar ( colour= "black", fill= "goldenrod3") + theme_minimal(12) + labs(x="Estado civil") + labs(y="Sujetos") + ggtitle("Fig 4. Estado civil de los participantes") 


#Situación laboral

TMS_DEMO %>% 
  filter(!is.na(q6_employeeyr)) %>% 
  ggplot(aes(x = q6_employeeyr)) + geom_bar(color="black", fill="darkolivegreen4") + labs(title = "Fig 5. Situación laboral en los últimos3 años", x = "Situación laboral", y = "Número de sujetos")

## Escolaridad
#grupo intervención
TMS_DEMOedadsh$q2_edyears

edyear <-c("q2_edyears") 
stats.dataed <- sapply(TMS_DEMOedadsh[edyear], mystats)
stats.dataed <- t(stats.dataed)
stats.dataed <- round(stats.dataed, digits=2)
stats.dataed

#grupo sham
TMS_DEMOedadtx$q2_edyears

edyear <-c("q2_edyears") 
stats.dataed <- sapply(TMS_DEMOedadtx[edyear], mystats)
stats.dataed <- t(stats.dataed)
stats.dataed <- round(stats.dataed, digits=2)
stats.dataed

## Años de consumo de cocaína
consum_sex2 <- boxplot(TMS_DEMO$q7_yrstart ~ TMS_DEMO$q1_sex, pch=21, notch=F,
                      col = c("firebrick4", "firebrick3"), bg = "slategray2",
                      ylab = "Edad de inicio",
                      xlab = "Sexo",
                      main = "Fig 6. Años de consumo de cocaína", font.main=2)

# grupo sham
TMS_DEMOedadsh <- TMS_DEMO[which(TMS_DEMO$group=="Sham"),]
TMS_DEMOedadsh$group

edadin <-c("q7_yrstart") 
stats.datain <- sapply(TMS_DEMOedadsh[edadin], mystats)
stats.datain <- t(stats.datain)
stats.datain <- round(stats.datain, digits=2)
stats.datain

#grupo intervención
TMS_DEMOedadtx <- TMS_DEMO[which(TMS_DEMO$group=="Treatment"),]
TMS_DEMOedadtx$group

edadin2 <-c("q7_yrstart") 
stats.datain <- sapply(TMS_DEMOedadtx[edadin2], mystats)
stats.datain <- t(stats.datain)
stats.datain <- round(stats.datain, digits=2)
stats.datain

## Años de consumo de cocaína
consum_sex <- boxplot(TMS_DEMO$q7_tconsume ~ TMS_DEMO$q1_sex, pch=21, notch=F,
                      col = c("firebrick4", "firebrick3"), bg = "slategray2",
                      ylab = "Años de consumo",
                      xlab = "Sexo",
                      main = "Fig 6. Años de consumo de cocaína", font.main=2)

# grupo sham
TMS_DEMOedadsh <- TMS_DEMO[which(TMS_DEMO$group=="Sham"),]
TMS_DEMOedadsh$group

consumo <-c("q7_tconsume") 
stats.datacon <- sapply(TMS_DEMOedadsh[consumo], mystats)
stats.datacon <- t(stats.datacon)
stats.datacon <- round(stats.datacon, digits=2)
stats.datacon

#grupo intervención
TMS_DEMOedadtx <- TMS_DEMO[which(TMS_DEMO$group=="Treatment"),]
TMS_DEMOedadtx$group

consumo2 <-c("q7_tconsume") 
stats.datacon <- sapply(TMS_DEMOedadtx[consumo2], mystats)
stats.datacon <- t(stats.datacon)
stats.datacon <- round(stats.datacon, digits=2)
stats.datacon

## Ingresos mensuales
# grupo sham
TMS_DEMOedadsh$q6_month <- as.numeric(TMS_DEMOedadsh$q6_month)
TMS_DEMOedadsh$q6_month

ingreso <-c("q6_month") 
stats.dataing <- sapply(TMS_DEMOedadsh[ingreso], mystats)
stats.dataing <- t(stats.dataing)
stats.dataing <- round(stats.dataing, digits=2)
stats.dataing
median(TMS_DEMOedadsh$q6_month)

# grupo intervención
ingreso1 <-c("q6_month") 
stats.dataing <- sapply(TMS_DEMOedadtx[ingreso], mystats)
stats.dataing <- t(stats.dataing)
stats.dataing <- round(stats.dataing, digits=2)
stats.dataing
median(TMS_DEMOedadtx$q6_month)
TMS_DEMOedadtx$q6_month

## Tratamiento ambulatorio
# grupo sham
ambul <-c("q9_amtreat") 
stats.dataam <- sapply(TMS_DEMOedadsh[ambul], mystats)
stats.dataam <- t(stats.dataam)
stats.dataam <- round(stats.dataam, digits=2)
stats.dataam
median(TMS_DEMOedadsh$q9_amtreat)

# grupo intervención
ambul1 <-c("q9_amtreat") 
stats.dataing <- sapply(TMS_DEMOedadtx[ambul1], mystats)
stats.dataing <- t(stats.dataing)
stats.dataing <- round(stats.dataing, digits=2)
stats.dataing
median(TMS_DEMOedadtx$q9_amtreat)

## Tratamiento hospitalario
# grupo sham
hosp <-c("q10_hosp") 
stats.datahosp <- sapply(TMS_DEMOedadsh[hosp], mystats)
stats.datahosp <- t(stats.datahosp)
stats.datahosp <- round(stats.datahosp, digits=2)
stats.datahosp
median(TMS_DEMOedadsh$q10_hosp)
TMS_DEMOedadtx$q10_hosp

# grupo intervención
hosp2 <-c("q10_hosp") 
stats.datahosp2 <- sapply(TMS_DEMOedadtx[hosp2], mystats)
stats.datahosp2 <- t(stats.datahosp2)
stats.datahosp2 <- round(stats.datahosp2, digits=2)
stats.datahosp2
median(TMS_DEMOedadtx$q10_hosp)

## Grupos de autoayuda
# grupo sham
autoa <-c("q11_grptime") 
stats.dataauto <- sapply(TMS_DEMOedadsh[autoa], mystats)
stats.dataauto <- t(stats.dataauto)
stats.dataauto <- round(stats.dataauto, digits=2)
stats.dataauto
median(TMS_DEMOedadsh$q11_grptime)
TMS_DEMOedadsh$q11_grptime

# grupo intervención
autoa2 <-c("q11_grptime") 
stats.dataauto2 <- sapply(TMS_DEMOedadtx[autoa2], mystats)
stats.dataauto2 <- t(stats.dataauto2)
stats.dataauto2 <- round(stats.dataauto2, digits=2)
stats.dataauto2
median(TMS_DEMOedadtx$q11_grptime)
TMS_DEMOedadtx$q11_grptime


library(moonBook)


require(moonBook)
require(snakecase)

mytable(group~q9_amtreat+q10_hosp+q11_grptime+q1_sex, data=TMS_DEMO)

