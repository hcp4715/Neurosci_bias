### code for analysis of neuroimage bias ### 

# code author:Ji-Xing Yin
# email:jixing-yin@outlook.com


# Author       date      log of change
# ==========   ======    ==============
# jixing-yin   18-4-5
# Yuepei XU    19-3-1
# Ji-Xing Yin  19-04-01  Rename variables
# hcp          19-04-01  rename in tidyverse

# input data
# oringinal file: "study1 combined duplicate.csv"

# output data
# output file:

## variables of this study

#Independent variable
#age(18 vs 38)
#evidence(behavior_court,brain_noimage,brain-brain) ##behavior_court means behavior evidence with a court image
                                                    ##brain_no_image means brain evidence with no image
                                                    ##brain-brain means brain evidence with brain image

# Measurements:
  # death_penalty(death penalty):1-7
  # percep_of_respon(perception of responsibility):1-7
  # percep_of_danger(perception of danger):1-7
  # ability_to_back(ability to come back society):1-7
  # free_will(free wil):1-5
  # just_sense(justice sense):0-5
  # just_belief(just world belief):1-6
  # know_of_law(knowledge of the law):1-7
  # know_of_sci(knowledge of the science):1-7
  # bielief_of_case(belief in truth of the case):1-7


### preparing ###
curDir = dirname(rstudioapi::getSourceEditorContext()$path)  # get the directory of current file
setwd(curDir)                                                # change the wording directory to the directory of the current file.
rm(list = setdiff(ls(), lsf.str())) # remove all variables except functions
curDir = dirname(rstudioapi::getSourceEditorContext()$path)

Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en")             # set the feedback language to English


## read data 
total.data <- read.csv("Study1_data.csv",header = TRUE,sep = ',', stringsAsFactors=FALSE, na.strings=c(""," ","NA"))

### load Packages
library(tidyverse,psych)

FADNames <- paste('FAD',1:16,sep="_")
#sciDeterNames <- paste('sciDeter',1:4, sep="_")
#fatDeterNames <- paste('fatDeter',1:4, sep="_")
#unpredicNames <- paste('unpredic',1:4, sep="_")
justSenNames <- paste('justSens',1:8,sep="_")
JstWrldBeliefNames <- paste('JstWrldBelief',1:13,sep="_")

## exclude participants who pay insufficient attention to the test problem and the scenarios
valid.data <- total.data %>%
  dplyr::filter(Q34=="4" & Q2_13=="3")   %>% # select the participants whose Q34=4 and Q2_13=3
  dplyr::filter(((X18b =="2"| X18bb == "2" | X18no == "2") & Q25<=18) |
                  ((X38b =="2"| X38bb == "2" | X38no == "2") & Q25>18)) %>% # select the participant with correct memory of age
  dplyr::filter(!is.na(Q30) & !is.na(Q31) & !is.na(Q32)) %>%              # select no-na data
  dplyr::mutate(CrimeAge1 = ifelse(X18b == "2" |X18bb=="2" | X18no=="2", "age17", "age37"),
                CrimeAge2 = ifelse(X38b == "2" |X38bb=="2" | X38no=="2", "age37", "age17")) %>%
  dplyr::mutate(CrimeAge1 = ifelse(X18b == "2" |X18bb=="2" | X18no=="2", "age17", "age37")) %>%
  dplyr::mutate(CrimeAge2 = ifelse(X38b == "2" |X38bb=="2" | X38no=="2", "age37", "age17")) %>%
  dplyr::mutate(CrimeAge = coalesce(CrimeAge1, CrimeAge2)) %>%                       # coalesce two columns into one
  dplyr::mutate(EvidenceType1 = ifelse(X18no == "2" |X38no =="2", "be_no_b"),
                EvidenceType2 = ifelse(X18b == "2" |X38b =="2", "b_no_b"),
                EvidenceType3 = ifelse(X18bb == "2" |X38bb =="2", "b_b")) %>%
  dplyr::mutate(EvidenceType  = coalesce(EvidenceType1, EvidenceType2,EvidenceType3)) %>%     # coalesce three columns into one
  dplyr::select(-c(CrimeAge1, CrimeAge2,EvidenceType1, EvidenceType2,EvidenceType3)) %>%      # delete intermediate columns
  dplyr::select(-Q2_13) %>% # remove the attention check item
  dplyr::rename_at(vars(starts_with('Q2_')), ~ FADNames) %>%            # use renames_at of dplyr to rename all items of short version of FAD+
  dplyr::rename_at(vars(starts_with('Q3_')), ~ justSenNames) %>%        # rename all justice sensitivity
  dplyr::rename_at(vars(starts_with('Q8_')), ~ JstWrldBeliefNames) %>%  # rename all just world belief
  dplyr::rename(now_SES = Q5, past_SES = Q6, future_SES = Q7, gender = Q9, age = Q10) # rename others one-by-one

                "Q30"="death_penalty","Q31"="percep_of_respon","Q32"="percep_of_danger",
                "Q33"="after_10years_danger","Q35_1"="punishment_for_killing","Q35_2"="punishment_for_protecting","Q35_3"="puishment_for_warning",
                "Q36"="law_familiar","Q37"="sci_familiar")

##rename the variables

library(plyr)
valid.data<-rename(valid.data,c(""="","Q2_8"="free_will_2","Q2_12"="free_will_3","Q2_16"="free_will_4",
                               "Q2_2"="scientific_1","Q2_10"="scientific_2","Q2_14"="scientific_3","Q2_15"="scientific_4",
                               "Q2_1"="fatalistic_1","Q2_4"="fatalistic_2","Q2_6"="fatalistic_3","Q2_9"="fatalistic_4",
                               "Q2_5"="unpreditic_1","Q2_7"="unpreditic_2","Q2_11"="unpreditic_3","Q2_17"="unpreditic_4",
                               "Q3_1"="just_sense_1","Q3_2"="just_sense_2","Q3_3"="just_sense_3","Q3_4"="just_sense_4","Q3_5"="just_sense_5","Q3_6"="just_sense_6","Q3_7"="just_sense_7","Q3_8"="just_sense_8",
                               "Q8_1"="just_belief_1","Q8_2"="just_belief_2","Q8_3"="just_belief_3","Q8_4"="just_belief_4","Q8_5"="just_belief_5","Q8_6"="just_belief_6","Q8_7"="just_belief_7","Q8_8"="just_belief_8","Q8_9"="just_belief_9","Q8_10"="just_belief_10","Q8_11"="just_belief_11","Q8_12"="just_belief_12","Q8_13"="just_belief_13",
                               "Q5"="now_status","Q6"="past_status","Q7"="future_status","Q9"="par_gender","Q10"="par_age",
                               "Q30"="death_penalty","Q31"="percep_of_respon","Q32"="percep_of_danger",
                               "Q33"="after_10years_danger","Q35_1"="punishment_for_killing","Q35_2"="punishment_for_protecting","Q35_3"="puishment_for_warning",
                               "Q36"="law_familiar","Q37"="sci_familiar"))
detach("package:plyr")


##select data of the scales

valid.data2<-lapply(valid.data,as.numeric)

attach(valid.data2)


#free will
free_will_total<- data.frame(free_will_1,free_will_2,free_will_3,free_will_4)
  free_will<- ((free_will_1+free_will_2+free_will_3+free_will_4)/4)
    ap_free_will <- psych::alpha(free_will_total)
      ap_free_will <- round(ap_free_will$total[1],2)

scientific_total <- data.frame(scientific_1,scientific_2,scientific_3,scientific_4)
  scientific <- ((scientific_1+scientific_2+scientific_3+scientific_4)/4)
    ap_scientific <- psych::alpha(scientific_total)
      ap_scientific <- round(ap_scientific$total[1],2)

fatalistic_total <- data.frame(fatalistic_1,fatalistic_2,fatalistic_3,fatalistic_4)
  fatalistic <- ((fatalistic_1+fatalistic_2+fatalistic_3+fatalistic_4)/4)
    ap_fatalistic <- psych::alpha(fatalistic_total)
      ap_fatalistic <- round(ap_fatalistic$total[1],2)

unpreditic_total<- data.frame(unpreditic_1,unpreditic_2,unpreditic_3,unpreditic_4)
  unpreditic<- ((unpreditic_1+unpreditic_2+unpreditic_3+unpreditic_4)/4)
    ap_unpreditic <- psych::alpha(unpreditic_total)
      ap_unpreditic <- round(ap_unpreditic$total[1],2)
  

#just sense
just_sense_total<- data.frame(just_sense_1,just_sense_2,just_sense_3,just_sense_4,just_sense_5,just_sense_6,just_sense_7,just_sense_8)
  just_sense<-(just_sense_1+just_sense_2+just_sense_3+just_sense_4+just_sense_5+just_sense_6+just_sense_7+just_sense_8)/8
    ap_just_sense <- psych::alpha(just_sense_total)
      ap_just_sense <- round(ap_just_sense$total[1],2)

#just world belief
just_belief_total<- data.frame(just_belief_1,just_belief_2,just_belief_3,just_belief_4,just_belief_5,just_belief_6,just_belief_7,just_belief_8,just_belief_9,just_belief_10,just_belief_11,just_belief_12,just_belief_13)
  just_belief <- (just_belief_1+just_belief_2+just_belief_3+just_belief_4+just_belief_5+just_belief_6+just_belief_7+just_belief_8+just_belief_9+just_belief_10+just_belief_11+just_belief_12+just_belief_13)/13
    ap_just_belief <- psych::alpha(just_belief_total)
      ap_just_belief <- round(ap_just_belief$total[1],2)

detach(valid.data2)

### end preparing ###

### information of participants ###
participant.age<-as.numeric(as.character(valid.data[,"par_age"])) #transform factor to numeric
participant.gender<-  factor(valid.data$par_gender,
                             levels = c(1, 2),
                             labels = c("Male", "Femle"))
age <- summary(participant.age)
gender <- summary(participant.gender)

### analysis ###

##   caculate the depentent varibles   ##

# ANOVA for three different varibles # 

death_penalty_anova <- summary(aov(valid.data$death_penalty~valid.data$CrimeAge*valid.data$EvidenceType))

percep_of_respon_anova <- summary(aov(valid.data$percep_of_respon~valid.data$CrimeAge*valid.data$EvidenceType))

percep_of_danger_anova <- summary(aov(valid.data$percep_of_danger~valid.data$CrimeAge*valid.data$EvidenceType))


# multivariable linear regression #

# test Multicollinearity #
attach(valid.data)

pre_regression_data<-data.frame(free_will,scientific,fatalistic,unpreditic,just_belief,just_sense,death_penalty,percep_of_respon,percep_of_danger)
regression_data<-pre_regression_data[-1:-4,] # there is 4 NA, we delete them here 
cor_regression_data<-cor(regression_data)
mcl_test<-kappa(cor_regression_data[,1:6]) #here,we use "kappa" to test multicollinearity,K<100 means multicollinearity is small

# regresssion of three varibles #

death_penalty_lm<-summary(lm(death_penalty~free_will++scientific++fatalistic++unpreditic++just_sense++just_belief,data=regression_data))

percep_of_respon_lm<-summary(lm(percep_of_respon~free_will++scientific++fatalistic++unpreditic++just_sense++just_belief,data=regression_data))

percep_of_danger_lm<-summary(lm(percep_of_danger~free_will++scientific++fatalistic++unpreditic++just_sense++just_belief,data=regression_data))

detach(valid.data)

### end of data analysis ###