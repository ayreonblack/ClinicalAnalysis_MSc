## Bases de protocolo TMS-Cocaína
## Base ASIP
## Por Alan Dávalos


TMS_ASIP <- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/TMS_ASIP.csv", header = T)

# factores
library(tidyverse)
TMS_ASIP$rid <- factor(TMS_ASIP$rid)
TMS_ASIP$group <- factor(TMS_ASIP$group,
                                levels = c(1, 2),
                                labels = c("Sham", "Treatment"))
connectome_ASIP$ASIP.G10 <- factor(connectome_ASIP$ASIP.G10,
                                   levels = c(1, 2),
                                   labels = c("Male", "Female"))
connectome_ASIP$ASIP.G17 <- factor(connectome_ASIP$ASIP.G17,
                                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                   labels = c("White", "Black", "American Indian", "Alaskan Native", "Asian", "Hispanic Mexican", "Hispanic Puerto Rican", "Hispanic Cuban", "Other Hispanic"))
connectome_ASIP$ASIP.G18 <- factor(connectome_ASIP$ASIP.G18,
                                   levels = c(1, 2, 3, 4, 5, 6),
                                   labels = c("protestant;", "catholic", "jewish", "islamic", "other", "none"))
connectome_ASIP$ASIP.G19 <- factor(connectome_ASIP$ASIP.G19,
                                   levels = c(1, 2, 3, 4, 5, 6),
                                   labels = c("no", "jail", "alcohol or drug treatment", "medical treatment", "psychiatric treatment", "other"))
connectome_ASIP[c(11, 13, 15, 20, 21, 24, 25, 30, 44, 45, 98, 99, 100, 101, 121, 127, 128, 133, 134, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147,148, 149, 150, 151, 152, 153, 154, 155, 156,157, 158, 165, 166, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 190, 191)] <- map_df(connectome_ASIP[c(11, 13, 15, 20, 21, 24, 25, 30, 44, 45, 98, 99, 100, 101, 121, 127, 128, 133, 134, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147,148, 149, 150, 151, 152, 153, 154, 155, 156,157, 158, 165, 166, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 190, 191)], function(x){
  factor(x, levels = c(0, 1), labels = c("no", "yes"))
})
connectome_ASIP[c(18, 19, 41, 42, 94, 95, 96, 97, 125, 126, 161, 162, 163, 164, 188, 189)] <- map_df(connectome_ASIP[c(18, 19, 41, 42, 94, 95, 96, 97, 125, 126, 161, 162, 163, 164, 188, 189)], function(x){
  factor(x, levels = c(0, 1, 2, 3, 4), labels = c("0", "1", "2", "3", "4"))
})
connectome_ASIP$ASIP.E7a <- factor(connectome_ASIP$ASIP.E7a,
                                   levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                   labels = c("major professional, ejecutive", "manager, nurse, pharmacist, teacher", "administrator, small business owner", "clerical, sales, technicians", "skilled manual, electrician", "semi-skilled, aide, driver, waiter", "unskilled, unenployed", "housework", "student, no-occupation, disabled"))
connectome_ASIP$ASIP.E10 <- factor(connectome_ASIP$ASIP.E10,
                                   levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                                   labels = c("fulltime job (35hrs per week)", "halftime-regular job", "half time-informal job", "student", "military service", "disabled/retired", "unemployed", "in controlled environment"))


connectome_ASIP[c(48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81)] <- map_df(connectome_ASIP[c(48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81)], function(x){
  factor(x, levels = c(1, 2, 3, 4, 5, 6), labels = c("0", "Oral", "Nasal", "smoking", "non IV injection", "IV injection"))
})

connectome_ASIP[c("ASIP.F3", "ASIP.F6", "ASIP.F10")] <- map_df(connectome_ASIP[c("ASIP.F3", "ASIP.F6", "ASIP.F10")], function(x){
  factor(x, levels = c(1, 2, 3), labels = c("no", "indifferent", "yes"))
})

connectome_ASIP$ASIP.F1 <- factor(connectome_ASIP$ASIP.F1,
                                  levels = c(1, 2, 3, 4, 5, 6),
                                  labels = c("married", "Remaried", "widowed", "separated", "divorced", "never married"))
connectome_ASIP$ASIP.F4 <- factor(connectome_ASIP$ASIP.F4,
                                  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                                  labels = c("with sexual partner and children", "with sexual partner alone", "with children alone", "with parents", "with family", "with friends", "alone", "controlled environment", "no stable arrangments"))
connectome_ASIP$ASIP.F9 <- factor(connectome_ASIP$ASIP.F9,
                                  levels = c(1, 2, 3),
                                  labels = c("family", "friends", "alone"))

##Descriptivas
table(TMS_ASIP$group,TMS_ASIP$stage)
table(TMS_ASIP$rid,TMS_ASIP$group)
summary(TMS_ASIP)


library('Hmisc')
describe(connectome_ASIP)
library(DataExplorer)
plot_missing(connectome_ASIP)
plot_bar(connectome_ASIP)
