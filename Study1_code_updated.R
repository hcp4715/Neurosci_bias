### code for analysis of neuroimage bias ### 

# code author:Ji-Xing Yin
# email:jixing-yin@outlook.com


# Author       date      log of change
# ==========   ======    ==============
# jixing-yin   18-4-5
# Yuepei XU    19-3-1
# Ji-Xing Yin  19-04-01  Rename variables
# hcp          19-04-01  rename in tidyverse
# Xu Yin       19-04-02  ...
# hcp          19-04-03  revise the code, more tidyverse
# hcp          19-04-03  test
# Yin          19-04-03
# hcp          19_04_05
# Yin          19_04-05



# Input data: "Study1_data_q25_corrected.csv"

# output data
# output file:

## variables of this study

#Independent variable
#age(17(adolscent) vs 37(adult))
#evidence(behavior_court,brain_noimage,brain-brain) 
##behavior_court means behavior evidence with a court image
##brain_noimage means brain evidence with no image
##brain-brain means brain evidence with brain image

# Measurements:
#death(death penalty):1-7
#respnsb(perception of responsibility):1-7
#curDanger(perception of danger):1-7
#retrn_Soc(ability to come back society):1-7
#FAD(free wil):1-5
#justSens(justice sense):0-5
#JstWrldBelief(just world belief):1-6
#legalKnowledge(knowledge of the law):1-7
#sciKnowledge(knowledge of the science):1-7



### preparing ###
curDir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curDir)
rm(list = setdiff(ls(), lsf.str())) # remove all variables except functions
curDir = dirname(rstudioapi::getSourceEditorContext()$path)

Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en")             # set the feedback language to English

### load Packages
library(tidyverse,psych)


## read data (hcp: using relative directory)

total.data <- read.csv("Study1_data_q25&18bb_corrected.csv",header = TRUE,sep = ',', 
                       stringsAsFactors=FALSE, na.strings=c(""," ","NA")) %>%
  mutate_if(is.character,as.numeric)  # change character to numeric

FADNames <- paste('FAD',1:16,sep="_")                      # name for FAD+
justSenNames <- paste('justSens',1:8,sep="_")              # name for Justice sensitivity
JstWrldBeliefNames <- paste('JstWrldBelief',1:13,sep="_")  # name for general belief in just world

## exclude participants who pay insufficient attention to the test problem and the scenarios
valid.data <- total.data %>%
  dplyr::filter(Q34=="4" & Q2_13=="3")   %>%          # select the participants paid attention (i.e., Q34=4 and Q2_13=3)
  dplyr::filter(((adolescent_brain =="2"|adolescent_brainimage == "2" | adolescent_behavior == "2") & Q25<=18) |
                  ((adult_brain =="2"| adult_brainimage == "2" | adult_behavior == "2") & Q25>18)) %>% # select the participant with correct memory of age
  dplyr::filter(!is.na(Q30) & !is.na(Q31) & !is.na(Q32)) %>%                # select no-na data
  dplyr::mutate(CrimeAge1 = ifelse(adolescent_brain == "2" |adolescent_brainimage =="2" | adolescent_behavior =="2", "age17", "age37"),
                CrimeAge2 = ifelse(adult_brain == "2" |adult_brainimage=="2" | adult_behavior=="2", "age37", "age17"),
                CrimeAge1 = ifelse(adolescent_brain == "2" |adolescent_brainimage =="2" | adolescent_behavior=="2", "age17", "age37"),
                CrimeAge2 = ifelse(adult_brain == "2" |adult_brainimage =="2" | adult_behavior =="2", "age37", "age17")) %>%
                   dplyr::mutate(CrimeAge = coalesce(CrimeAge1, CrimeAge2)) %>%                       # coalesce two columns into one
                   dplyr::mutate(EvidenceType1 = ifelse(adolescent_behavior == "2" |adult_behavior =="2", "Behav"),
                                 EvidenceType2 = ifelse(adolescent_brain == "2" |adult_brain =="2", "Neuro_No_img"),
                                 EvidenceType3 = ifelse(adolescent_brainimage == "2" |adult_brainimage =="2", "Neuro_img")) %>%
                   dplyr::mutate(EvidenceType  = coalesce(EvidenceType1, EvidenceType2,EvidenceType3)) %>%    # coalesce three columns into one
                   dplyr::select(-c(CrimeAge1, CrimeAge2,EvidenceType1, EvidenceType2,EvidenceType3)) %>%     # delete intermediate columns
                   dplyr::select(-Q2_13) %>%                                                                  # remove the attention check item
                   dplyr::rename_at(vars(starts_with('Q2_')), ~ FADNames) %>%           # use renames_at of dplyr to rename all items of short version of FAD+
                   dplyr::rename_at(vars(starts_with('Q3_')), ~ justSenNames) %>%        # rename all justice sensitivity
                   dplyr::rename_at(vars(starts_with('Q8_')), ~ JstWrldBeliefNames) %>%  # rename all just world belief
                   dplyr::rename(now_SSS = Q5, past_SSS = Q6, future_SSS = Q7,           # hcp: subjective SES = SSS
                                 gender = Q9, age = Q10, self_Edu = Q11,                 ## hcp: I said included all variable!!
                                 father_edu = Q12, moth_edu = Q13, father_occup = Q14, moth_occup = Q15,
                                 death ="Q30", respnsb="Q31", curDanger="Q32",
                                 retrn_Soc ="Q33",  retribut ="Q35_1", specfc_Deter="Q35_2", Genrl_Deter = "Q35_3",
                                 legalKnowledge ="Q36", sciKnowledge ="Q37", verisim = Q39, verisim_effect = Q40) # rename others one-by-one
                 
                 

                 
###: hcp: why there so much space?? delete all these spaces
###  hcp: no need to calculate the score now, do it later, together with all other scales, generate a new dataframe includes all variables needed. 

#valid.data2<-valid.data%>%
#   dplyr::mutate(free_will_avg=rowMeans(data.frame(FAD_3,FAD_8,FAD_12,FAD_15),na.rm = T))%>%
#   dplyr::mutate(scientific_avg=rowMeans(data.frame(FAD_2,FAD_10,FAD_13,FAD_14),na.rm = T))%>%
                #   dplyr::mutate(fatalistic_avg=rowMeans(data.frame(FAD_1,FAD_4,FAD_6,FAD_9),na.rm = T))%>%
                #   dplyr::mutate(unpreditic_avg=rowMeans(data.frame(FAD_5,FAD_7,FAD_11,FAD_16),na.rm = T))%>%
                ##   dplyr::mutate(justSens_avg=rowMeans(data.frame(justSens_1,justSens_2,justSens_3,justSens_4,justSens_5,justSens_6,justSens_7,justSens_8),na.rm=T))%>%
                #   dplyr::mutate(JstWrldBelief_avg=rowMeans(data.frame(JstWrldBelief_1,JstWrldBelief_2,JstWrldBelief_3,JstWrldBelief_4,JstWrldBelief_5,JstWrldBelief_6,JstWrldBelief_7,JstWrldBelief_8,JstWrldBelief_9,JstWrldBelief_10,JstWrldBelief_11,JstWrldBelief_12,JstWrldBelief_13),na.rm=T))
                 
                 
## measures and their reliability
                 
# free will
# hcp: added how to calculate McDolad's Omega, another index for reliability 
freeWill_reli <- valid.data %>% 
  dplyr::select(FAD_3,FAD_8,FAD_12,FAD_15) %>%       # hcp added comments: select the items for free will subscale
  psych::omega()                                     # calculate the omega using psych package

# hcp: added how to calculate McDolad's Omega, another index for reliability 
freeWill_alpha  <- round(freeWill_reli$alpha,2)      # Alpha
freeWill_omeg_t <- round(freeWill_reli$omega.tot,2)  # Omega (hierachical)
freeWill_omeg_h <- round(freeWill_reli$omega_h,2)    # Alpha (total)
 
#ap_free_will <- psych::alpha(free_will_total)
#ap_free_will <- round(ap_free_will$total[1],2)
                 
scientific_total <- valid.data2 %>% select(FAD_2,FAD_10,FAD_13,FAD_14)
ap_scientific <- psych::alpha(scientific_total)
ap_scientific <- round(ap_scientific$total[1],2)
                 

## 
                 fatalistic_total <- valid.data2 %>% select(FAD_1,FAD_4,FAD_6,FAD_9)
                 ap_fatalistic <- psych::alpha(fatalistic_total)
                 ap_fatalistic <- round(ap_fatalistic$total[1],2)
                 
                 unpreditic_total<- valid.data2 %>% select(FAD_5,FAD_7,FAD_11,FAD_16)
                 ap_unpreditic <- psych::alpha(unpreditic_total)
                 ap_unpreditic <- round(ap_unpreditic$total[1],2)
                 
                 
                 #just sense
                 just_sense_total<- valid.data2 %>% select(justSens_1,justSens_2,justSens_3,justSens_4,justSens_5,justSens_6,justSens_7,justSens_8)
                 ap_just_sense <- psych::alpha(just_sense_total)
                 ap_just_sense <- round(ap_just_sense$total[1],2)
                 
                 #just world belief
                 just_belief_total<- valid.data2 %>% select(JstWrldBelief_1,JstWrldBelief_2,JstWrldBelief_3,JstWrldBelief_4,JstWrldBelief_5,JstWrldBelief_6,JstWrldBelief_7,JstWrldBelief_8,JstWrldBelief_9,JstWrldBelief_10,JstWrldBelief_11,JstWrldBelief_12,JstWrldBelief_13)
                 ap_just_belief <- psych::alpha(just_belief_total)
                 ap_just_belief <- round(ap_just_belief$total[1],2)
                 
                 
                 ### end preparing ###
                 
                 ### information of participants ###
                 participant.age<-as.numeric(as.character(valid.data[,"age"])) #transform factor to numeric
                 participant.gender<-  factor(valid.data$gender,
                                              levels = c(1, 2),
                                              labels = c("Male", "Femle"))
                 age <- summary(participant.age)
                 gender <- summary(participant.gender)
                 
                 ### analysis ###
                 
                 ##   caculate the depentent varibles   ##
                 
                 # ANOVA for three different varibles # 
                 
                 death_penalty_anova <- summary(aov(valid.data2$death~valid.data2$CrimeAge*valid.data2$EvidenceType))
                 
                 percep_of_respon_anova <- summary(aov(valid.data2$respnsb~valid.data2$CrimeAge*valid.data2$EvidenceType))
                 
                 percep_of_danger_anova <- summary(aov(valid.data2$curDanger~valid.data2$CrimeAge*valid.data2$EvidenceType))
                 

                 
                 # multivariable linear regression #
                 
                 # test Multicollinearity #
                 
                 regression_data<-valid.data2 %>% select(free_will_avg,scientific_avg,fatalistic_avg,unpreditic_avg,justSens_avg,JstWrldBelief_avg,death,respnsb,curDanger) # there is 4 NA, we delete them here 
                 regression_data<-na.omit(regression_data)
                 cor_regression_data<-cor(regression_data)
                 mcl_test<-kappa(cor_regression_data[,1:6]) #here,we use "kappa" to test multicollinearity,K<100 means multicollinearity is small
                 
                 # regresssion of three varibles #
                 
                 death_penalty_lm<-summary(lm(death~free_will_avg++scientific_avg++fatalistic_avg++unpreditic_avg++justSens_avg++JstWrldBelief_avg,data=regression_data))
                 
                 percep_of_respon_lm<-summary(lm(respnsb~free_will_avg++scientific_avg++fatalistic_avg++unpreditic_avg++justSens_avg++JstWrldBelief_avg,data=regression_data))
                 
                 percep_of_danger_lm<-summary(lm(curDanger~free_will_avg++scientific_avg++fatalistic_avg++unpreditic_avg++justSens_avg++JstWrldBelief_avg,data=regression_data))
                 
                 
                 ### end of data analysis ###