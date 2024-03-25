### Bases de datos de proyecto TMS
### Por Alan Davalos

### Base BIS

TMS_BIS <- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/BIS.csv", header = TRUE)
TMS_BIS$group <- factor(TMS_BIS$group,
                               levels = c(1, 2),
                               labels = c("Sham", "Treatment"))
TMS_BIS$rid <- factor(TMS_BIS$rid)



##Descriptivas
table(TMS_BIS$group,TMS_BIS$stage)
summary(TMS_BIS)
library('Hmisc')
describe(TMS_BIS)




table(connectome_BIS$group)

class(connectome_BIS)
dim(connectome_BIS)
names(connectome_BIS)
summary(connectome_BIS)
str(connectome_BIS)
library('Hmisc')
describe(connectome_BIS)

library(DataExplorer)
plot_missing(connectome_BIS)
plot_histogram(connectome_BIS)
plot_density(connectome_BIS)

ggplot(connectome_BIS, aes(x = rid, y = tot_score)) +
  geom_point(col = "blue", size = 2, alpha = .8) +
  labs(x = "RID", y = "score", 
       title = "BIS") +
  geom_smooth(aes(x = rid, y = tot_score))
