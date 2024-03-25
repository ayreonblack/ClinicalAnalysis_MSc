## TMS escala AMAI socioeconómicos ##
## Por Alan Dávalos ##

## Cargar bases y acomodar variables


TMS_AMAI <- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/AMAI.csv", header = T)
TMS_AMAI$rid <- factor(TMS_AMAI$rid)
TMS_AMAI$group <- factor(TMS_AMAI$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"
                         ))

#Descriptivas
summary(TMS_AMAI)
library('Hmisc')
describe(TMS_AMAI)
