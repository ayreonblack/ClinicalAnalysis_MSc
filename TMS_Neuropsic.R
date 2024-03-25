### Bases de datos de proyecto TMS
### Por Alan Davalos
### Bases de datos neuropsicológicos

### Load Packages
library(pacman)
p_load(moonBook, ggplot2, dplyr)

### General Data
TMS_Neuropsiq<- read.csv("/home/alan/Documentos/Maestría/Protocolo/TMS_NEURODATA_2020.csv", header = TRUE)
TMS_Neuropsiq$group <- factor(TMS_Neuropsiq$group,
                          levels = c(1, 2),
                          labels = c("Sham", "Treatment"))
TMS_Neuropsiq$rid <- factor(TMS_Neuropsiq$rid)
TMS_Neuropsiq$stage <- factor(TMS_Neuropsiq$stage)
TMS_Neuropsiq<-TMS_Neuropsiq[!(TMS_Neuropsiq$stage=="T5"),]

table(TMS_Neuropsiq$group, TMS_Neuropsiq$stage)

TMSNPsiq0 <- TMS_Neuropsiq[which(TMS_Neuropsiq$stage=="T0"),]
TMSNPsiq1 <- TMS_Neuropsiq[which(TMS_Neuropsiq$stage=="T1"),]
TMSNPsiq1_4 <- TMS_Neuropsiq[which(TMS_Neuropsiq$stage=="T1-4"),]
TMSNPsiq2 <- TMS_Neuropsiq[which(TMS_Neuropsiq$stage=="T2"),]
TMSNPsiq3 <- TMS_Neuropsiq[which(TMS_Neuropsiq$stage=="T3"),]
TMSNPsiq4 <- TMS_Neuropsiq[which(TMS_Neuropsiq$stage=="T4"),]

table(TMSNPsiq1$group, TMSNPsiq1$stage)
table(TMSNPsiq1_4$group, TMSNPsiq1_4$stage)
table(TMSNPsiq2$group, TMSNPsiq2$stage)
table(TMSNPsiq3$group, TMSNPsiq3$stage)


### Frontal System Behaviors Scale

## before drug abuse (only in T0)
TMS_Neuropsiq$ApathyB 
TMS_Neuropsiq$DesinB
TMS_Neuropsiq$ExDysB

mytable(group~ApathyB+DesinB+ExDysB, data=TMS_Neuropsiq)
mytable(group~ApathyB+DesinB+ExDysB, data=TMSNPsiq0)

mytable(stage~ApathyB+DesinB+ExDysB, data=TMS_Neuropsiq)

## Currently, in the last week (for T0: in the last month)
TMS_FrontalScale$ApathyC
TMS_FrontalScale$DesinC
TMS_FrontalScale$ExDysC

mytable(group~ApathyC+DesinC+ExDysC, data=TMS_FrontalScale)
mytable(group~ApathyC+DesinC+ExDysC, data=TMSNPsiq0)
mytable(group~ApathyC+DesinC+ExDysC, data=TMSNPsiq1)
mytable(group~ApathyC+DesinC+ExDysC, data=TMSNPsiq2)
mytable(group~ApathyC+DesinC+ExDysC, data=TMSNPsiq3)

mytable(stage~ApathyC+DesinC+ExDysC, data=TMS_Neuropsiq)

aov_ApathyC <- aov(ApathyC~stage*group,TMS_Neuropsiq); aov_ApathyC; summary(aov_ApathyC)
TukeyHSD(aov_ApathyC)

### ADHD Scale (only past 6 months)
TMS_Neuropsiq$tdah_inat
TMS_Neuropsiq$tdah_imp_hip

mytable(group~tdah_inat+tdah_imp_hip, data=TMSNPsiq0)
mytable(group~tdah_inat+tdah_imp_hip, data=TMSNPsiq2)
mytable(group~tdah_inat+tdah_imp_hip, data=TMSNPsiq3)

mytable(stage~tdah_inat+tdah_imp_hip, data=TMS_Neuropsiq)
aov_ApathyC <- aov(tdah_imp_hip~stage*group,TMS_Neuropsiq); aov_ApathyC; summary(aov_ApathyC)
### Plutchik Impulsivity scale

TMS_Neuropsiq$Isauto ## autocontrol items
TMS_Neuropsiq$Isplan ## planning items
TMS_Neuropsiq$Isphysb ## physioloical Behaviour	
TMS_Neuropsiq$Issact ## sponaneous acting	
TMS_Neuropsiq$Istot ## total items

mytable(group~Isauto+Isplan+Isphysb+Issact+Istot, data=TMSNPsiq0)
mytable(group~Isauto+Isplan+Isphysb+Issact+Istot, data=TMSNPsiq2)

qqnorm(TMSNPsiq1$Istot); qqline(TMSNPsiq1$Istot, col="red")
shapiro.test(HDRST0$tot_score)

mytable(stage~Isauto+Isplan+Isphysb+Issact+Istot, data=TMS_Neuropsiq)

aov_Plutchik <- aov(Isauto~stage*group,TMS_Neuropsiq); aov_Plutchik; summary(aov_Plutchik)
TukeyHSD(aov_Plutchik)

## Solo salió significativo el de autocontrol de t0 a t2, sin diferencia entre grupos (aunque no mide t1)
## hubo diferencia en la fase abierta en auatocontrol (p=0.003) en  sponaneous acting (p=0.02) y en el puntaje total (p=0.02)

### Labyrinth
## Items of time: lab_t1	lab_t2	lab_t3	lab_t4	lab_t5	lab_ttotal

mytable(group~lab_t1+lab_t2+lab_t3+lab_t4+lab_t5+lab_ttotal, data=TMSNPsiq0)
mytable(group~lab_t1+lab_t2+lab_t3+lab_t4+lab_t5+lab_ttotal, data=TMSNPsiq1)
mytable(group~lab_t1+lab_t2+lab_t3+lab_t4+lab_t5+lab_ttotal, data=TMSNPsiq2)

qqnorm(TMSNPsiq1$lab_ttotal); qqline(TMSNPsiq1$lab_ttotal, col="red")
shapiro.test(TMSNPsiq1$lab_ttotal)

mytable(group~lab_wall1+lab_wall2+lab_wall3+lab_wall4+lab_wall5+lab_wall_total, data=TMSNPsiq0)
mytable(group~lab_wall1+lab_wall2+lab_wall3+lab_wall4+lab_wall5+lab_wall_total, data=TMSNPsiq1)
mytable(group~lab_wall1+lab_wall2+lab_wall3+lab_wall4+lab_wall5+lab_wall_total, data=TMSNPsiq2)

mytable(group~lab_cross1+lab_cross2+lab_cross3+lab_cross4+lab_cross5+lab_cross_total, data=TMSNPsiq0)
mytable(group~lab_cross1+lab_cross2+lab_cross3+lab_cross4+lab_cross5+lab_cross_total, data=TMSNPsiq1)
mytable(group~lab_cross1+lab_cross2+lab_cross3+lab_cross4+lab_cross5+lab_cross_total, data=TMSNPsiq2)

mytable(group~lab_pencil1+lab_pencil2+lab_pencil3+lab_pencil4+lab_pencil5+lab_pencil_total, data=TMSNPsiq0)
mytable(group~lab_pencil1+lab_pencil2+lab_pencil3+lab_pencil4+lab_pencil5+lab_pencil_total, data=TMSNPsiq1)
mytable(group~lab_pencil1+lab_pencil2+lab_pencil3+lab_pencil4+lab_pencil5+lab_pencil_total, data=TMSNPsiq2)

mytable(group~lab_toterror, data=TMSNPsiq0)
mytable(group~lab_toterror, data=TMSNPsiq1)
mytable(group~lab_toterror, data=TMSNPsiq2)

### Digits

mytable(group~rdod+rdoi+ln, data=TMSNPsiq0)
mytable(group~rdod+rdoi+ln, data=TMSNPsiq1)
mytable(group~rdod+rdoi+ln, data=TMSNPsiq2)

mytable(stage~rdod+rdoi+ln, data=TMS_Neuropsiq)

### Consonant Trigram Memory Task

mytable(group~hits_subt_7+time_subt_7+hits_subt_3+time_subt_3+hits_sum+time_hits_sum+hits_tot+time.tot, data=TMSNPsiq0)
mytable(group~hits_subt_7+time_subt_7+hits_subt_3+time_subt_3+hits_sum+time_hits_sum+hits_tot+time.tot, data=TMSNPsiq1)
mytable(group~hits_subt_7+time_subt_7+hits_subt_3+time_subt_3+hits_sum+time_hits_sum+hits_tot+time.tot, data=TMSNPsiq2)

mytable(stage~hits_subt_7+time_subt_7+hits_subt_3+time_subt_3+hits_sum+time_hits_sum+hits_tot+time.tot, data=TMS_Neuropsiq)

### Verbal FLuency
TMS_Fluency <- TMS_Neuropsiq[c(199:205)]

mytable(group~vf_letter.p+vf_letter.m+vf_letter.r+vf_animals+vf_verbs+vf_letter_tot+vf_anverb, data=TMSNPsiq0)
mytable(group~vf_letter.p+vf_letter.m+vf_letter.r+vf_animals+vf_verbs+vf_letter_tot+vf_anverb, data=TMSNPsiq1)
mytable(group~vf_letter.p+vf_letter.m+vf_letter.r+vf_animals+vf_verbs+vf_letter_tot+vf_anverb, data=TMSNPsiq2)
mytable(group~vf_letter.p+vf_letter.m+vf_letter.r+vf_animals+vf_verbs+vf_letter_tot+vf_anverb, data=TMSNPsiq3)

mytable(stage~vf_letter.p+vf_letter.m+vf_letter.r+vf_animals+vf_verbs+vf_letter_tot+vf_anverb, data=TMS_Neuropsiq)

aov_Fluency <- aov(vf_anverb~stage*group,TMS_Neuropsiq); aov_Fluency; summary(aov_Fluency)

# No diferencias significativas

### Berg Card Sorting test
TMS_Berg <- TMS_Neuropsiq[c(206:234)]

mytable(group~bcst.cc+bcst.ce+bcst.ne+bcst.rc+bcst.prc+bcst.mrc+bcst.te+bcst.pe+bcst.mce+bcst.rp+bcst.prp+bcst.mrp+bcst.ep+bcst.pep+bcst.mep+bcst.enp+bcst.penp+bcst.menp+bcst.eu+bcst.peu+bcst.meu+bcst.e1c+bcst.fms+bcst.aa+bcst.rnp+bcst.cpmi+bcst.cpma+bcst.cppr+bcst.tcp, data=TMSNPsiq0)
mytable(stage~bcst.cc+bcst.ce+bcst.ne+bcst.rc+bcst.prc+bcst.mrc+bcst.te+bcst.pe+bcst.mce+bcst.rp+bcst.prp+
          bcst.mrp+bcst.ep+bcst.pep+bcst.mep+bcst.enp+bcst.penp+bcst.menp+bcst.eu+bcst.peu+bcst.meu+bcst.e1c+
          bcst.fms+bcst.aa+bcst.rnp+bcst.cpmi+bcst.cpma+bcst.cppr+bcst.tcp, data=TMS_Neuropsiq)

aov_BERG <- aov(bcst_tcp~stage*group,TMS_Neuropsiq); aov_BERG; summary(aov_BERG)
TukeyHSD(aov_BERG)

#Ninguna Diferencia significativa

### Flanker

mytable(group~F_AC+F_AI+F_EC+F_EI+F_NRC+F_NRI+F_PAC+F_PAI+F_PEC+F_PEI+F_PNRC+F_PNRI+F_TRC+
          F_TRI+F_TREC+F_TREI+F_DTRC+F_DTRI+F_DTREC+F_DTREI+F_ECOM+F_ANG+F_PC+F_PANG+F_TRECOM+
          F_DTRECOM+F_AC1+F_AI1+F_EC1+F_EI1+F_NRC1+F_NRI1+F_PAC1+F_PAI1+F_PEC1+F_PEI1+F_PNRC1+
          F_PNRI1+F_TRC1+F_TRI1+F_TREC1+F_TREI1+F_DTRC1+F_DTRI1+F_DTREC1+F_DTRE1+F_ECOM1+F_ANG1+
          F_PECOM1+F_PANG1+F_TRECOM1+F_DTRECOM1+F_AC4+F_AI4+F_EC4+F_EI4+F_NRC4+F_NCI4+F_PAC4+
          F_PAI4+F_PEC4+F_PEI4+F_PNRC4+F_PNRI4+F_TRC4+F_TRI4+F_TREC4+F_TREI4+F_DTRC4+F_DTRI4+
          F_DTREC4+F_DTREI4+F_ECOM4+F_ANG4+F_PC4+F_PANG4+F_TRECOM4+F_DTRECOM4+F_EF+F_EF1+F_EF4, data=TMSNPsiq0)

aov_Flanker <- aov(F_PEI~stage*group,TMS_Neuropsiq); aov_Flanker; summary(aov_Flanker)
TukeyHSD(aov_Flanker)

##F_PNRC1 tuvo p 0.005 tanto en estadio como en interaccion estadio grupo ??? pero la variable que no es porcentaje no tiene diferencias
## de todos modos esa variable indica omisiones, más referente a inatención
##F_AC4 tuvo p 0.02 en estadio, F_PAC4 diferencia entre estadios p 0.003

## F_PAC interacción estadio grupo p 0.07, F_PAI diferencia entre estadios p 0.037,  
## F_PEI diferencia entre grupos p 0.045, F_PNRC diferencia entre estadios p 0.07 e interacción p 0.06
## F_PNRI diferencia entre grupos p 0.056, F_ECOM diferencia entre grupos p 0.074
## F_ANG diferencia entre grupos p 0.067, F_PC y F_PANG diferencia entre grupos p 0.071, 
## F_EI1 diferencia entre estadios p 0.065, F_PEI1 diferencia entre estadios p 0.073
## F_PECOM1 y F_PANG1 diferencia entre grupos p 0.07, F_DTREI4 diferencia entre estadios p 0.070
## F_ECOM4, F_PC4, F_ANG4, F_PANG4 diferencia entre grupos p 0.06, F_EF1 interacción estadio grupo 0.061

## Error de codificación en F_PAI4

# Grafico de boxplot
b <- ggplot(TMS_Neuropsiq, aes(x = stage, y = F_AC4, fill = group)) 
b + geom_boxplot() +  labs(title ="Total de aciertos ante estímulos congruentes 4ª parte", x = 'Estadio', y = 'Puntaje total') 

ggplot(data = TMS_Neuropsiq, aes(x = stage, y = F_PAC4)) + 
  geom_jitter(aes(color = group), size = 1, alpha = 0.7) +
  geom_boxplot(aes(color = group), alpha = 0.7) + 
  xlab('Estadio') + 
  ylab('Puntuación') +
  ggtitle('Total de aciertos ante estímulos congruentes 4ª parte') + 
  theme_minimal()

table(TMS_Neuropsiq$F_AC4, TMS_Neuropsiq$stage)

### London Tower
mytable(group~tl_c+tl_tm+tl_ti+tl_te+tl_tt+tl_vlt, data=TMSNPsiq0)
mytable(group~tl_c+tl_tm+tl_ti+tl_te+tl_tt+tl_vlt, data=TMSNPsiq2)
mytable(group~tl_c+tl_tm+tl_ti+tl_te+tl_tt+tl_vlt, data=TMSNPsiq3)
mytable(group~tl_c+tl_tm+tl_ti+tl_te+tl_tt+tl_vlt, data=TMSNPsiq4)

mytable(stage~tl_c+tl_tm+tl_ti+tl_te+tl_tt+tl_vlt, data=TMS_Neuropsiq)

aov_London <- aov(tl_vlt~stage*group,TMS_Neuropsiq); aov_London; summary(aov_London)
TukeyHSD(aov_London)

# tl_vlt Sólo marca el anova diferencia entre grupos*estadio, pero no se evalúa T1, así que es solo un grupo
# Resto sin diferencias importantes en prueba de planeación

### Iowa Card Sorting Test

mytable(group~igt_i+igt_t+igt_rv+igt_trv+igt_dtrv+igt_rd+igt_trd+igt_dtrd+igt_b1+igt_b2+igt_b3+igt_b4+igt_b5, data=TMSNPsiq0)
mytable(group~igt_i+igt_t+igt_rv+igt_trv+igt_dtrv+igt_rd+igt_trd+igt_dtrd+igt_b1+igt_b2+igt_b3+igt_b4+igt_b5, data=TMSNPsiq2)
mytable(group~igt_i+igt_t+igt_rv+igt_trv+igt_dtrv+igt_rd+igt_trd+igt_dtrd+igt_b1+igt_b2+igt_b3+igt_b4+igt_b5, data=TMSNPsiq3)

mytable(stage~igt_i+igt_t+igt_rv+igt_trv+igt_dtrv+igt_rd+igt_trd+igt_dtrd, data=TMS_Neuropsiq)

aov_Iowa <- aov(igt_i~stage*group,TMS_Neuropsiq); aov_Iowa; summary(aov_Iowa)
TukeyHSD(aov_Iowa)
# Sólo diferencia entre fases en igt_dtrv, pero no en trv, sin aparentes diferencias entre fase en prueba de toma de desiciones

### Reading the mind in the eyes

mytable(group~rmet_ap+rmet_an+rmet_ane+rmet_ah+rmet_am+rmet_ta, data=TMSNPsiq0)
mytable(group~rmet_ap+rmet_an+rmet_ane+rmet_ah+rmet_am+rmet_ta, data=TMSNPsiq2)
mytable(group~rmet_ap+rmet_an+rmet_ane+rmet_ah+rmet_am+rmet_ta, data=TMSNPsiq3)

mytable(stage~rmet_ap+rmet_an+rmet_ane+rmet_ah+rmet_am+rmet_ta, data=TMS_Neuropsiq)

aov_read <- aov(rmet_ta~stage*group,TMS_Neuropsiq); aov_read; summary(aov_read)
TukeyHSD(aov_read)

# Diferencia a los 3 meses en aciertos positivos (pero igual ya es un solo grupo a 3 meses)
# LIgera diferencia global a los 3 meses, pero pble por sesgo de T5 donde redujo mucho pero son 2 pacientes

### Bangor Gambling Task solo mide T1 y T1-4

### Gambling tasks together: Ya que Bangor mide T1 y T1-4 y Iowa mide el resto
Gambling <- read.csv("/home/alan/Documentos/Paper de maestría/Addimex bases/gambling tasks together.csv", header = TRUE)
Gambling$group <- factor(Gambling$group,
                              levels = c(1, 2),
                              labels = c("Sham", "Treatment"))
Gambling$rid <- factor(Gambling$rid)
Gambling$stage <- factor(Gambling$stage)

Gambling0 <- Gambling[which(Gambling$stage=="T0"),]
Gambling1 <- Gambling[which(Gambling$stage=="T1"),]
Gambling1_4 <- Gambling[which(Gambling$stage=="T1-4"),]
Gambling2 <- Gambling[which(Gambling$stage=="T2"),]

mytable(group~igt_i+igt_t+igt_rv+igt_trv+igt_rd+igt_trd+igt_b1+igt_b2+igt_b3+igt_b4+igt_b5, data=Gambling0)
mytable(group~igt_i+igt_rv+igt_trv+igt_rd+igt_trd+igt_b1+igt_b2+igt_b3+igt_b4+igt_b5, data=Gambling1)
mytable(group~igt_i+igt_rv+igt_trv+igt_rd+igt_trd+igt_b1+igt_b2+igt_b3+igt_b4+igt_b5, data=Gambling1_4)

mytable(stage~igt_i+igt_t+igt_rv+igt_trv+igt_rd+igt_trd+igt_b1+igt_b2+igt_b3+igt_b4+igt_b5, data=Gambling) #No sirve por que:
#Se combinan Sham con Real en T1 y T1-4, salen muchas significativas pero en realidad se evalúa como un solo grupo, y Iowa no salen significativas

# en T1 diferencia entre respuestas ventajosas (p=0.18)