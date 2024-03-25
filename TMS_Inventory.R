## TMS Inventario ##
## Por Alan Dávalos ##

## Cargar bases y acomodar variables

library(tidyverse)
TMS_INV <- read.csv("C:/Users/Alan/Desktop/Maestría/TMS/Bases_r/TMS_Inventory.csv", header = T)
TMS_INV$rid <- factor(TMS_INV$rid)
TMS_INV$group <- factor(TMS_INV$group,
                         levels = c(1, 2),
                         labels = c("Sham", "Treatment"
                         ))
TMS_INV$date <- factor(TMS_INV$date)
TMS_INV$stage <- factor(TMS_INV$stage)
TMS_INV$othq3_a <- factor(TMS_INV$othq3_a,
                          levels = c(1, 2),
                          labels = c("yes", "no"
                          ))
TMS_INV <- TMS_INV %>% 
  separate(col = othq3_b, into = c("othq3_b.1", "othq3_b.2")) 

TMS_INV$othq3_b.1 <- factor(TMS_INV$othq3_b.1,
                        levels = c(1, 2),
                        labels = c("psychologic", "pharmacologic"
                        ))
TMS_INV$othq3_b.2 <- factor(TMS_INV$othq3_b.2,
                            levels = c(1, 2),
                            labels = c("psychologic", "pharmacologic"
                            ))
TMS_INV$othq3_d <- factor(TMS_INV$othq3_d,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                          labels = c("antidepressant", "gabapentin", "topiramate", "pregabaline", "bupropion",  "fluoxetine", "citalopram", "paroxeine", "sertraline", "Hidroxizina", "Clonazepam", "escitalopram", "Others"
                          ))
TMS_INV$othq3_f <- factor(TMS_INV$othq3_f,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                          labels = c("antidepressant", "gabapentin", "topiramate", "pregabaline", "bupropion",  "fluoxetine", "citalopram", "paroxeine", "sertraline", "Hidroxizina", "Clonazepam", "escitalopram", "Others"
                          ))
TMS_INV$othq3_h <- factor(TMS_INV$othq3_h,
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                          labels = c("antidepressant", "gabapentin", "topiramate", "pregabaline", "bupropion",  "fluoxetine", "citalopram", "paroxeine", "sertraline", "Hidroxizina", "Clonazepam", "escitalopram", "Others"
                          ))
TMS_INV$othq3_j <- factor(TMS_INV$othq3_j,
                          levels = c(1, 2, 3),
                          labels = c("daily", "when needed", "other"
                          ))
TMS_INV$alchq1 <- factor(TMS_INV$alchq1,
                            levels = c(1, 2),
                            labels = c("positive", "negative"
                            ))
TMS_INV$ut_amp <- factor(TMS_INV$ut_amp,
                         levels = c(1, 2),
                         labels = c("positive", "negative"
                         ))
TMS_INV$ut_bzd <- factor(TMS_INV$ut_bzd,
                         levels = c(1, 2),
                         labels = c("positive", "negative"
                         ))
TMS_INV$ut_coc <- factor(TMS_INV$ut_coc,
                         levels = c(1, 2),
                         labels = c("positive", "negative"
                         ))
TMS_INV$ut_met <- factor(TMS_INV$ut_met,
                         levels = c(1, 2),
                         labels = c("positive", "negative"
                         ))
TMS_INV$ut_mor <- factor(TMS_INV$ut_mor,
                         levels = c(1, 2),
                         labels = c("positive", "negative"
                         ))
TMS_INV$ut_thc <- factor(TMS_INV$ut_thc,
                         levels = c(1, 2),
                         labels = c("positive", "negative"
                         ))
TMS_INV$auto1 <- factor(TMS_INV$auto1,
                         levels = c(1, 2),
                         labels = c("yes", "no"
                         ))
TMS_INV$auto4 <- factor(TMS_INV$auto4,
                        levels = c(1, 2),
                        labels = c("reincidence", "relapse"
                        ))
TMS_INV$auto5 <- factor(TMS_INV$auto5,
                        levels = c(1, 2, 3, 4, 5, 6, 7),
                        labels = c("better", "moderetly better", "slightly better", "no changes", "slightly worst", "moderetly worst", "worst"
                        ))



##Descriptivas
table(TMS_INV$group,TMS_INV$stage)
table(TMS_INV$rid,TMS_INV$group)
summary(TMS_INV)

INV0 <- TMS_INV[which(TMS_INV$stage=="T0"),]
summary(INV0$othq3_d)
summary(INV0$othq3_f)
summary(INV0$othq3_h)

INV0sh <- INV0[which(INV0$group=="Sham"),]
summary(INV0sh$othq3_d)
summary(INV0sh$othq3_f)
summary(INV0sh$othq3_h)

INV0tx <- INV0[which(INV0$group=="Treatment"),]
summary(INV0tx$othq3_d)
summary(INV0tx$othq3_f)
summary(INV0tx$othq3_h)

INV1 <- TMS_INV[which(TMS_INV$stage=="T1"),]

summary(INV1$othq3_d)
summary(INV1$othq3_f)
summary(INV1$othq3_h)
INV2 <- TMS_INV[which(TMS_INV$stage=="T2"),]
summary(INV2$othq3_d)

summary(TMS_INV)

# Farmacos de acuerdo a deltas en puntaje de VAS
#grupo sham
TMS_VASSHAM$diff_vas
TMS_INVT1 <- TMS_INV[which(TMS_INV$stage=="T1"),]

VAS_INVSH <- merge(TMS_VASSHAM, TMS_INVT1, by = "rid")

table(VAS_INVSH$othq3_d, VAS_INVSH$diff_vas)
table(VAS_INVSH$othq3_f, VAS_INVSH$diff_vas)
table(VAS_INVSH$othq3_h, VAS_INVSH$diff_vas)

# grupo real
TMS_VASTX$diff_vas
VAS_INVTX <- merge(TMS_VASTX, TMS_INVT1, by = "rid")
table(VAS_INVTX$othq3_d, VAS_INVTX$diff_vas)
table(VAS_INVTX$othq3_f, VAS_INVTX$diff_vas)
table(VAS_INVTX$othq3_h, VAS_INVTX$diff_vas)

# Farmacos de acuerdo a deltas en puntaje de HDRS
#grupo sham
TMS_HDRSSHAM$diff_hdrs
TMS_INVT1 <- TMS_INV[which(TMS_INV$stage=="T1"),]

HDRS_INVSH <- merge(TMS_HDRSSHAM, TMS_INVT1, by = "rid")

table(HDRS_INVSH$othq3_d, HDRS_INVSH$diff_hdrs)
table(HDRS_INVSH$othq3_f, HDRS_INVSH$diff_hdrs)
table(HDRS_INVSH$othq3_h, HDRS_INVSH$diff_hdrs)

# grupo real
TMS_HDRSTX$diff_hdrs
HDRS_INVTX <- merge(TMS_HDRSTX, TMS_INVT1, by = "rid")
table(HDRS_INVTX$othq3_d, HDRS_INVTX$diff_hdrs)
table(HDRS_INVTX$othq3_f, HDRS_INVTX$diff_hdrs)
table(HDRS_INVTX$othq3_h, HDRS_INVTX$diff_hdrs)


# Farmacos de acuerdo a deltas en puntaje de HARS
TMS_HARSSHAM$diff_hars

HARS_INVSH <- merge(TMS_HARSSHAM, TMS_INVT1, by = "rid")
table(HARS_INVSH$othq3_d, HARS_INVSH$diff_hars)
table(HARS_INVSH$othq3_f, HARS_INVSH$diff_hars)
table(HARS_INVSH$othq3_h, HARS_INVSH$diff_hars)



