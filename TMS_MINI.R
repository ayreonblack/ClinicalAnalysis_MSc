### Base de datos de protocolo TMS y cocaína
##  Instrumento MINI PLUS
## Por Alan Dávalos

library(tidyverse)
library(plyr)
TMS_MINI <- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/MINI.csv", header = T)
TMS_MINI$rid <- factor(TMS_MINI$rid)
TMS_MINI$group <- factor(TMS_MINI$group,
                                levels = c(1, 2),
                                labels = c("Sham", "Treatment"))

TMS_MINI <- rename(TMS_MINI, c("aactc"="actualdepression"))
TMS_MINI$actualdepression <- factor(TMS_MINI$actualdepression,
                                          levels = c(0, 1),
                                          labels = c("No", "Yes"
                                          ))
TMS_MINI <- rename(TMS_MINI, c("apasr"="pastdepression"))
TMS_MINI$pastdepression <- factor(TMS_MINI$pastdepression,
                         levels = c(0, 1),
                         labels = c("No", "Yes"
                         ))
TMS_MINI <- rename(TMS_MINI, c("asubc"="submoodcurrent"))
TMS_MINI$submoodcurrent <- factor(TMS_MINI$submoodcurrent,
                                  levels = c(0, 1),
                                  labels = c("No", "Yes"
                                  ))
TMS_MINI <- rename(TMS_MINI, c("asubr"="submoodrecurrent"))
TMS_MINI$submoodrecurrent <- factor(TMS_MINI$submoodrecurrent,
                                  levels = c(0, 1),
                                  labels = c("No", "Yes"
                                  ))
TMS_MINI <- rename(TMS_MINI, c("amel"="melancholiccurrent"))
TMS_MINI$melancholiccurrent <- factor(TMS_MINI$melancholiccurrent,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"
                                    ))
TMS_MINI <- rename(TMS_MINI, c("bact"="dysthimiacurrent"))
TMS_MINI$dysthimiacurrent <- factor(TMS_MINI$dysthimiacurrent,
                                      levels = c(0, 1),
                                      labels = c("No", "Yes"
                                      ))
TMS_MINI <- rename(TMS_MINI, c("bpas"="dysthimiapast"))
TMS_MINI$dysthimiapast <- factor(TMS_MINI$dysthimiapast,
                                    levels = c(0, 1),
                                    labels = c("No", "Yes"
                                    ))




colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.a1"] <- "stimulants_answer1"
connectome_MINI$stimulants_answer1 <- factor(connectome_MINI$stimulants_answer1,
                                             levels = c(1, 2, 3, 4, 5, 6),
                                             labels = c("amphetamines", "speed", "crystal", "dexedrine", "ritalin", "slimming pills"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.a2"] <- "stimulants_answer2"
connectome_MINI$stimulants_answer2 <- factor(connectome_MINI$stimulants_answer2,
                                             levels = c(1, 2, 3, 4, 5, 6),
                                             labels = c("amphetamines", "speed", "crystal", "dexedrine", "ritalin", "slimming pills"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.b1"] <- "cocaine_answer1"
connectome_MINI$cocaine_answer1 <- factor(connectome_MINI$cocaine_answer1,
                                          levels = c(1, 2, 3, 4),
                                          labels = c("nasal", "IV", "crack", "speedball"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.b2"] <- "cocaine_answer2"
connectome_MINI$cocaine_answer2 <- factor(connectome_MINI$cocaine_answer2,
                                          levels = c(1, 2, 3, 4),
                                          labels = c("nasal", "IV", "crack", "speedball"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.c"] <- "opioids"
connectome_MINI$opioids <- factor(connectome_MINI$opioids,
                                  levels = c(1, 2, 3, 4),
                                  labels = c("heroine", "morphine", "dilaudid", "opium; 5 = demerol; 6 = methadone; 7 = codeine; 8 = percodan; 9 = darvon"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.d1"] <- "hallucinogens_answer1"
connectome_MINI$hallucinogens_answer1 <- factor(hallucinogens_answer1,
                                                levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                                                labels = c("LSD", "mescaline", "peyote", "PCP", "psilocybin", "STP", "mushrooms", "extasy"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.d2"] <- "hallucinogens_answer2"
connectome_MINI$hallucinogens_answer2 <- factor(connectome_MINI$hallucinogens_answer2,
                                                levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                                                labels = c("LSD", "mescaline", "peyote", "PCP", "psilocybin", "STP", "mushrooms", "extasy"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.d3"] <- "hallucinogens_answer3"
connectome_MINI$hallucinogens_answer3 <- factor(connectome_MINI$hallucinogens_answer3,
                                                levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                                                labels = c("LSD", "mescaline", "peyote", "PCP", "psilocybin", "STP", "mushrooms", "extasy"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.e"] <- "inhalants"
connectome_MINI$inhalants <- factor(connectome_MINI$inhalants,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("glue/solvents", "ether", "nitrous oxide", "poppers"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.f"] <- "cannabis"
connectome_MINI$cannabis <- factor(connectome_MINI$cannabis,
                                   levels = c(1, 2, 3),
                                   labels = c("hashish", "THC pure", "weed"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="Mini.L1.g"] <- "tranquilizers"
connectome_MINI$tranquilizers <- factor(connectome_MINI$tranquilizers,
                                        levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                        labels = c("qualude", "seconal", "valium", "xanax", "librium", "ativan", "dalmane", "halcion", "barbiturates"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="MINI.L2a"] <- "impact_drugs1"
connectome_MINI$impact_drugs1 <- factor(connectome_MINI$impact_drugs1,
                                        levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                                        labels = c("stimulants", "cocaine", "opioids", "hallucinogens", "inhalants", "cannabis", "tranquilizers", "others"))

colnames(connectome_MINI)[colnames(connectome_MINI)=="MINI.L2b"] <- "impact_drugs2"
connectome_MINI$impact_drugs2 <- factor(connectome_MINI$impact_drugs2,
                                        levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                                        labels = c("stimulants", "cocaine", "opioids", "hallucinogens", "inhalants", "cannabis", "tranquilizers", "others"))

connectome_MINI$MINI.M16.b <- ordered(connectome_MINI$MINI.M16.b,
                                      levels = c(1, 2, 3, 4),
                                      labels = c("No", "mild", "moderate", "high"))

connectome_MINI$MINI.M17 <- ordered(connectome_MINI$MINI.M17,                                                                              levels = c(1, 2, 3),
                                    labels = c("1-30 days", "1-6 months", "more than 6 months"))

connectome_MINI$MINI.M23 <- factor(connectome_MINI$MINI.M23,                                                                               levels = c(1, 2, 3),
                                   labels = c("mood", "thoughts", "equal"))

class(TMS_MINI)
dim(TMS_MINI)

summary(TMS_MINI)
library('Hmisc')
describe(TMS_MINI)
library(DataExplorer)
plot_bar(TMS_MINI)