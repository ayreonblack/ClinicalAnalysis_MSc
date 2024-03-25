## TMS tabaco ##
## Por Alan Dávalos ##

## Cargar bases y acomodar variables


TMS_tob <- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/Tobacco.csv", header = T)
TMS_tob$rid <- factor(TMS_tob$rid)
TMS_tob$group <- factor(TMS_tob$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"
                         ))
TMS_tob$consumption <- factor(TMS_tob$consumption,
                        levels = c(0, 1),
                        labels = c("no", "yes"
                        ))
##Descriptivas
table(TMS_tob$group,TMS_tob$stage)
summary(TMS_tob)

TMS_tob$rid
