##Psychological Impact of Threat and Lockdown - Regressions script##

library("MASS",character.only=TRUE)
library(lme4)
library(lmerTest)
library(apaTables)
library(lavaan)
library(dplyr)
library(foreign)
library(MBESS)
library(jtools)
library(ggstance)
library(broom)
library(broom.mixed)

##Preparation##

data <- read.spss("DATA_PsychologicalImpact_COVID19_anon.sav",
                  use.value.labels = FALSE,
                  to.data.frame = TRUE)
names(data) <- tolower(names(data))

data <- 
  data %>% 
  mutate(anxietyZ = (anxiety_avg - mean(anxiety_avg, na.rm=T))/sd(anxiety_avg, na.rm=T))%>% 
  mutate(depressionZ = (depression_avg - mean(depression_avg, na.rm=T))/sd(depression_avg, na.rm=T))%>% 
  mutate(sense_of_controlZ = (controlsense_avg - mean(controlsense_avg, na.rm=T))/sd(controlsense_avg, na.rm=T))%>% 
  mutate(social_isolationZ = (socisol_avg - mean(socisol_avg, na.rm=T))/sd(socisol_avg, na.rm=T)) %>%
  mutate(freqcomcovZ = (freqcomcov_avg - mean(freqcomcov_avg, na.rm=T))/sd(freqcomcov_avg, na.rm=T))%>% 
  mutate(feelinginformedZ = (feelinginformed_avg - mean(feelinginformed_avg, na.rm=T))/sd(feelinginformed_avg, na.rm=T))
  
data <- 
  data %>% 
  mutate(freqcom1famZ = (freqcom1fam - mean(freqcom1fam, na.rm=T))/sd(freqcom1fam, na.rm=T))%>%
  mutate(freqcom2frZ = (freqcom2fr - mean(freqcom2fr, na.rm=T))/sd(freqcom2fr, na.rm=T))%>%
  mutate(freqcom3colZ = (freqcom3col - mean(freqcom3col, na.rm=T))/sd(freqcom3col, na.rm=T))%>%
  mutate(freqcom4famZ = (freqcom4fam - mean(freqcom4fam, na.rm=T))/sd(freqcom4fam, na.rm=T))%>%
  mutate(freqcom5frZ = (freqcom5fr - mean(freqcom5fr, na.rm=T))/sd(freqcom5fr, na.rm=T))%>%
  mutate(freqcom6colZ = (freqcom6col - mean(freqcom6col, na.rm=T))/sd(freqcom6col, na.rm=T))%>%
  mutate(freqcom7famZ = (freqcom7fam - mean(freqcom7fam, na.rm=T))/sd(freqcom7fam, na.rm=T))%>%
  mutate(freqcom8frZ = (freqcom8fr - mean(freqcom8fr, na.rm=T))/sd(freqcom8fr, na.rm=T))%>%
  mutate(freqcom9colZ = (freqcom9col - mean(freqcom9col, na.rm=T))/sd(freqcom9col, na.rm=T))

data <- 
  data %>% 
  mutate(activ1_washZ = (activ1_wash - mean(activ1_wash, na.rm=T))/sd(activ1_wash, na.rm=T))%>%
  mutate(activ2_wash20Z = (activ2_wash20 - mean(activ2_wash20, na.rm=T))/sd(activ2_wash20, na.rm=T))%>%
  mutate(activ3_stayhomeZ = (activ3_stayhome - mean(activ3_stayhome, na.rm=T))/sd(activ3_stayhome, na.rm=T))%>%
  mutate(activ4_disinfecthomeZ = (activ4_disinfecthome - mean(activ4_disinfecthome, na.rm=T))/sd(activ4_disinfecthome, na.rm=T))%>%
  mutate(activ5_antibactprodZ = (activ5_antibactprod - mean(activ5_antibactprod, na.rm=T))/sd(activ5_antibactprod, na.rm=T))%>%
  mutate(activ6_nofaceZ = (activ6_noface - mean(activ6_noface, na.rm=T))/sd(activ6_noface, na.rm=T))%>%
  mutate(activ7_nohandsZ = (activ7_nohands - mean(activ7_nohands, na.rm=T))/sd(activ7_nohands, na.rm=T))%>%
  mutate(activ8_nophysZ = (activ8_nophys - mean(activ8_nophys, na.rm=T))/sd(activ8_nophys, na.rm=T))%>%
  mutate(activ9_maskZ = (activ9_mask - mean(activ9_mask, na.rm=T))/sd(activ9_mask, na.rm=T))%>%
  mutate(activ10_maskn95Z = (activ10_maskn95 - mean(activ10_maskn95, na.rm=T))/sd(activ10_maskn95, na.rm=T))%>%
  mutate(activ12_othersfoodZ = (activ12_othersfood - mean(activ12_othersfood, na.rm=T))/sd(activ12_othersfood, na.rm=T))%>% 
  mutate(actionsavoid_avgZ = (actionsavoid_avg - mean(actionsavoid_avg, na.rm=T))/sd(actionsavoid_avg, na.rm=T))
  
data <- 
  data %>% 
  mutate(cope_activeZ = (data$cope_active - mean(data$cope_active, na.rm=T))/sd(data$cope_active, na.rm=T))%>% 
  mutate(cope_planZ = (cope_plan - mean(cope_plan, na.rm=T))/sd(cope_plan, na.rm=T))%>% 
  mutate(cope_posrefrZ = (cope_posrefr - mean(cope_posrefr, na.rm=T))/sd(cope_posrefr, na.rm=T))%>% 
  mutate(cope_acceptZ = (cope_accept - mean(cope_accept, na.rm=T))/sd(cope_accept, na.rm=T))%>% 
  mutate(cope_humourZ = (cope_humour - mean(cope_humour, na.rm=T))/sd(cope_humour, na.rm=T))%>% 
  mutate(cope_relspirZ = (cope_relspir - mean(cope_relspir, na.rm=T))/sd(cope_relspir, na.rm=T))%>% 
  mutate(cope_emosupZ = (cope_emosup - mean(cope_emosup, na.rm=T))/sd(cope_emosup, na.rm=T))%>% 
  mutate(cope_instrumsupZ = (cope_instrumsup - mean(cope_instrumsup, na.rm=T))/sd(cope_instrumsup, na.rm=T))%>% 
  mutate(cope_denialZ = (cope_denial - mean(cope_denial, na.rm=T))/sd(cope_denial, na.rm=T))%>% 
  mutate(cope_ventZ = (cope_vent - mean(cope_vent, na.rm=T))/sd(cope_vent, na.rm=T))%>% 
  mutate(cope_substanZ = (cope_substan - mean(cope_substan, na.rm=T))/sd(cope_substan, na.rm=T))%>% 
  mutate(cope_giveupZ = (cope_giveup - mean(cope_giveup, na.rm=T))/sd(cope_giveup, na.rm=T))%>% 
  mutate(cope_selfblameZ = (cope_selfblame - mean(cope_selfblame, na.rm=T))/sd(cope_selfblame, na.rm=T))%>%
  mutate(cope_adaptiveZ = (cope_adaptive - mean(cope_adaptive, na.rm=T))/sd(cope_adaptive, na.rm=T))%>% 
  mutate(cope_maladaptiveZ = (cope_maladaptive - mean(cope_maladaptive, na.rm=T))/sd(cope_maladaptive, na.rm=T)) 
  
data <- 
  data %>%
  mutate(govac1Z = (govac1 - mean(govac1, na.rm=T))/sd(govac1, na.rm=T))%>%
  mutate(govac2Z = (govac2 - mean(govac2, na.rm=T))/sd(govac2, na.rm=T))%>%
  mutate(govac3Z = (govac3 - mean(govac3, na.rm=T))/sd(govac3, na.rm=T))%>%
  mutate(govac4Z = (govac4 - mean(govac4, na.rm=T))/sd(govac4, na.rm=T))%>%
  mutate(govac5Z = (govac5 - mean(govac5, na.rm=T))/sd(govac5, na.rm=T))%>%
  mutate(govac6Z = (govac6 - mean(govac6, na.rm=T))/sd(govac6, na.rm=T))%>% 
  mutate(government_actionsZ = (actionsgov_avg - mean(actionsgov_avg, na.rm=T))/sd(actionsgov_avg, na.rm=T))
  
data <- 
  data %>% 
  mutate(gov_capabZ = (gov_capab - mean(gov_capab, na.rm=T))/sd(gov_capab, na.rm=T))%>% 
  mutate(gov_benevolZ = (gov_benevol - mean(gov_benevol, na.rm=T))/sd(gov_benevol, na.rm=T))%>% 
  mutate(gov_trustZ = (gov_trust - mean(gov_trust, na.rm=T))/sd(gov_trust, na.rm=T))%>% 
  mutate(govstate_capabZ = (govstate_capab - mean(govstate_capab, na.rm=T))/sd(govstate_capab, na.rm=T))%>% 
  mutate(govstate_benevolZ = (govstate_benevol - mean(govstate_benevol, na.rm=T))/sd(govstate_benevol, na.rm=T))%>% 
  mutate(govstate_trustZ = (govstate_trust - mean(govstate_trust, na.rm=T))/sd(govstate_trust, na.rm=T))%>% 
  mutate(institutional_trustZ = (instittrust_avg - mean(instittrust_avg, na.rm=T))/sd(instittrust_avg, na.rm=T))%>% 
  mutate(institutional_truststateZ = (instittruststate_avg - mean(instittruststate_avg, na.rm=T))/sd(instittruststate_avg, na.rm=T))

reduce_anxietyZ <- data$anxietyZ*-1

##Regressions##

tableSM_7_1 = lm(social_isolationZ ~ freqcom1famZ + freqcom2frZ + freqcom3colZ + freqcom4famZ + freqcom5frZ + freqcom6colZ + freqcom7famZ + freqcom8frZ + freqcom9colZ, data = data)
summary(tableSM_7_1)
confint(tableSM_7_1)
apa.reg.table(tableSM_7_1)

tableSM_9_1 = lm(sense_of_controlZ ~ feelinginformedZ + freqcomcovZ+ actionsavoid_avgZ + cope_adaptiveZ + cope_maladaptiveZ + government_actionsZ + instittrust_avg + government_actionsZ*instittrust_avg, data = data)
summary(tableSM_9_1)
confint(tableSM_9_1)
apa.reg.table(tableSM_9_1)

tableSM_9_2 = lm(reduce_anxietyZ ~ feelinginformedZ + freqcomcovZ+ actionsavoid_avgZ + cope_adaptiveZ + cope_maladaptiveZ + government_actionsZ + instittrust_avg + government_actionsZ*instittrust_avg, data = data)
summary(tableSM_9_2)
confint(tableSM_9_2)
apa.reg.table(tableSM_9_2)

tableSM_10_1 = lm(anxietyZ ~ activ1_washZ + activ2_wash20Z + activ3_stayhomeZ + activ4_disinfecthomeZ + activ5_antibactprodZ + activ6_nofaceZ + activ7_nohandsZ + activ8_nophysZ + activ9_maskZ + activ10_maskn95Z + activ12_othersfoodZ, data = data)
summary(tableSM_10_1)
confint(tableSM_10_1)
apa.reg.table(tableSM_10_1)

data_ess <- data[data$sit_general==5,]

tableSM_10_2 = lm(anxietyZ ~ activ1_washZ + activ2_wash20Z + activ3_stayhomeZ + activ4_disinfecthomeZ + activ5_antibactprodZ + activ6_nofaceZ + activ7_nohandsZ + activ8_nophysZ + activ9_maskZ + activ10_maskn95Z + activ12_othersfoodZ, data = data_ess)
summary(tableSM_10_2)
confint(tableSM_10_2)
apa.reg.table(tableSM_10_2)

tableSM_11_1_adaptive = lm(anxietyZ ~ cope_activeZ + cope_planZ + cope_posrefrZ + cope_acceptZ + cope_humourZ + cope_relspirZ + cope_emosupZ + cope_instrumsupZ, data = data)
summary(tableSM_11_1_adaptive)
apa.reg.table(tableSM_11_1_adaptive)              

tableSM_11_1_maladapt = lm(anxietyZ ~  cope_denialZ + cope_ventZ + cope_substanZ + cope_giveupZ + cope_selfblameZ, data = data)
summary(tableSM_11_1_maladapt)
apa.reg.table(tableSM_11_1_maladapt)     

tableSM_11_2_adaptive = lm(depressionZ ~ cope_activeZ + cope_planZ + cope_posrefrZ + cope_acceptZ + cope_humourZ + cope_relspirZ + cope_emosupZ + cope_instrumsupZ, data = data)
summary(tableSM_11_2_adaptive)
apa.reg.table(tableSM_11_2_adaptive)              

tableSM_11_2_maladapt = lm(depressionZ ~ cope_denialZ + cope_ventZ + cope_substanZ + cope_giveupZ + cope_selfblameZ, data = data)
summary(tableSM_11_2_maladapt)
apa.reg.table(tableSM_11_2_maladapt)     

tableSM_14_1 = lm(anxietyZ ~ govac1Z + govac2Z + govac3Z + govac4Z + govac5Z + govac6Z, data = data)
summary(tableSM_14_1)
confint(tableSM_14_1)
apa.reg.table(tableSM_14_1)

tableSM_14_2 = lm(institutional_trustZ ~ govac1Z + govac2Z + govac3Z + govac4Z + govac5Z + govac6Z, data = data)
summary(tableSM_14_2)
apa.reg.table(tableSM_14_2)
