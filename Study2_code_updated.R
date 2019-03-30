### code for analysis of neuroimage bias ### 

# code author:Ji-Xing Yin
# email:jixing-yin@outlook.com


# Author       date
# ==========   ======
# jixing-yin   18-4-5
# Yuepei XU    19-3-1

# input data
# oringinal file: "Neuroimaging_bias_study2_1.csv"

# output data
# output file:

## variables of this study

#Independent variable
#age(18 vs 38)
#evidence(behavior_no-brain,brain_no-brain,brain-brain)

#dependent variable
#dp(death penalty):1-7
#por(perception of responsibility):1-7
#pod(perception of danger):1-7
#atcbs(ability to come back society):1-7

#meditating variable
#fr(free wil):1-5
#js(justice sense):0-5
#jwb(just world belief):1-6
#kotl(knowledge of the law):1-7
#kots(knowledge of the science):1-7
#bitotc(belief in truth of the case):1-7


### preparing ###
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English

rm(list = setdiff(ls(), lsf.str())) # remove all variables except functions

## read data 
total.data<-read.csv("Study2_data.csv",header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

## exclude participants who pay insufficient attention to the test problem and the scenarios
valid.data<-subset(total.data,Q34=="4"&Q2_Q2_28=="3",header=ture) # select the participants whose Q34=4 and Q2_28=3
valid.data<-subset(valid.data,(Q18=="1"|Q20=="1"|Q22=="1"&Q25=="0")|(Q19=="1"|Q21=="1"|Q23=="1"&Q25=="1"))
check_missing1 <- is.na(valid.data$Q30)
check_missing2 <- is.na(valid.data$Q31)
check_missing3 <- is.na(valid.data$Q32)
valid.data<-subset(valid.data,check_missing1==FALSE&check_missing2==FALSE&check_missing3==FALSE)
valid.data<-data.frame(valid.data)

age18_check <- valid.data$Q18=="1"|valid.data$Q20=="1"|valid.data$Q22=="1"
age18_check <- is.na(age18_check)
iv_age <- ifelse (age18_check==TRUE, "age38", "age18")

be_check <- valid.data$Q18=="1"|valid.data$Q19=="1"
be_check <- is.na(be_check)
b_check <- valid.data$Q20=="1"|valid.data$Q21=="1"
b_check <- is.na(b_check)
bb_check <- valid.data$Q22=="1"|valid.data$Q23=="1"
bb_check <- is.na(bb_check)
iv_be <- ifelse (be_check==TRUE,0, 1)
iv_b <- ifelse (b_check==TRUE, 0,2)
iv_bb <- ifelse (bb_check==TRUE,0, 3)
iv_evidence <- iv_be+iv_b+iv_bb
iv_evidence<-  factor(iv_evidence,
                             levels = c(1, 2,3),
                             labels = c("behavior_bar", "brain_bar","brain_iamge"))

## select data of the independent variable ##

age18<-subset(valid.data,Q18=="1"|Q20=="1"|Q22=="1")
age38<-subset(valid.data,Q19=="1"|Q21=="1"|Q23=="1")
be_no_b<-subset(valid.data,Q18=="1"|Q19=="1")
b_no_b<-subset(valid.data,Q20=="1"|Q21=="1")
b_b<-subset(valid.data,Q22=="1"|Q23=="1")

age18be_no_b<-subset(valid.data,Q18=="1")
age18b_no_b<-subset(valid.data,Q20=="1")
age18b_b<-subset(valid.data,Q22=="1")
age38be_no_b<-subset(valid.data,Q19=="1")
age38b_no_b<-subset(valid.data,Q21=="1")
age38b_b<-subset(valid.data,Q23=="1")

#be_no_b means behavior evidence with no brain image
#b_no_b means brain evidence with no brain image
#b_b means brain evidence with brain image


##select data of the scales

valid.data2<-lapply(valid.data,as.numeric)

attach(valid.data2)
library("psych")

#free will
free_will_total <- data.frame(Q2_Q2_4,Q2_Q2_8,Q2_Q2_12,Q2_Q2_16,Q2_Q2_21,Q2_Q2_23,Q2_Q2_26)
scientific_total <- data.frame(Q2_Q2_2,Q2_Q2_10,Q2_Q2_14,Q2_Q2_22,Q2_Q2_24)
fatalistic_total <- data.frame(Q2_Q2_1,Q2_Q2_5,Q2_Q2_9,Q2_Q2_13)
unpreditic_total <- data.frame(Q2_Q2_3,Q2_Q2_7,Q2_Q2_11,Q2_Q2_15,Q2_Q2_19,Q2_Q2_20,Q2_Q2_25,Q2_Q2_27)
free_will<- ((Q2_Q2_4+Q2_Q2_8+Q2_Q2_12+Q2_Q2_16+Q2_Q2_21+Q2_Q2_23+Q2_Q2_26)/7)
scientific <- ((Q2_Q2_2+Q2_Q2_10+Q2_Q2_14+Q2_Q2_22+Q2_Q2_24)/5)
fatalistic <- ((Q2_Q2_1+Q2_Q2_5+Q2_Q2_9+Q2_Q2_13)/4)
unpreditic<- ((Q2_Q2_3+Q2_Q2_7+Q2_Q2_11+Q2_Q2_15+Q2_Q2_19+Q2_Q2_20+Q2_Q2_25+Q2_Q2_27)/8)

ap_free_will <- psych::alpha(free_will_total)
ap_free_will <- round(ap_free_will$total[1],2)
ap_scientific <- psych::alpha(scientific_total)
ap_scientific <- round(ap_scientific$total[1],2)
ap_fatalistic <- psych::alpha(fatalistic_total)
ap_fatalistic <- round(ap_fatalistic$total[1],2)
ap_unpreditic <- psych::alpha(unpreditic_total)
ap_unpreditic <- round(ap_unpreditic$total[1],2)

#just sense
just_sense_total<- data.frame(Q3_Q3_1,Q3_Q3_2,Q3_Q3_3,Q3_Q3_4,Q3_Q3_5,Q3_Q3_6,Q3_Q3_7,Q3_Q3_8)
just_sense<-(Q3_Q3_1+Q3_Q3_2+Q3_Q3_3+Q3_Q3_4+Q3_Q3_5+Q3_Q3_6+Q3_Q3_7+Q3_Q3_8)/8
ap_just_sense <- psych::alpha(just_sense_total)
ap_just_sense <- round(ap_just_sense$total[1],2)

#just world belief
just_belief_total<- data.frame(Q8_4,Q8_5,Q8_6,Q8_7,Q8_8,Q8_9,Q8_10,Q8_12,Q8_14,Q8_15,Q8_16,Q8_17)
just_belief<-(Q8_4+Q8_5+Q8_6+Q8_7+Q8_8+Q8_9+Q8_10+Q8_12+Q8_14+Q8_15+Q8_16+Q8_17)/12
ap_just_belief <- psych::alpha(just_belief_total)
ap_just_belief <- round(ap_just_belief$total[1],2)

#SES
SES<-(Q5+Q6+Q7)/3

#emotion
pos_emotion_pre_total<- data.frame(Q54_Q54_1,Q54_Q54_2,Q54_Q54_3,Q54_Q54_4,Q54_Q54_5,Q54_Q54_6,Q54_Q54_7,Q54_Q54_8
                  ,Q54_Q54_9)                 
pos_emotion_host_total<- data.frame(Q55_Q55_1,Q55_Q55_2,Q55_Q55_3,Q55_Q55_4,Q55_Q55_5,Q55_Q55_6,Q55_Q55_7,Q55_Q55_8
                   ,Q55_Q55_9)    
neg_emotion_pre_total<- data.frame(Q54_Q54_10,Q54_Q54_11,Q54_Q54_12,Q54_Q54_13,Q54_Q54_14,Q54_Q54_15,Q54_Q54_16,Q54_Q54_17
                  ,Q54_Q54_18)
neg_emotion_host_total<- data.frame(Q55_Q55_10,Q55_Q55_11,Q55_Q55_12,Q55_Q55_13,Q55_Q55_14,Q55_Q55_15,Q55_Q55_16,Q55_Q55_17
                   ,Q55_Q55_18)
pos_emotion_pre<-(Q54_Q54_1+Q54_Q54_2+Q54_Q54_3+Q54_Q54_4+Q54_Q54_5+Q54_Q54_6+Q54_Q54_7+Q54_Q54_8
                  +Q54_Q54_9)/9                  
pos_emotion_host<-(Q55_Q55_1+Q55_Q55_2+Q55_Q55_3+Q55_Q55_4+Q55_Q55_5+Q55_Q55_6+Q55_Q55_7+Q55_Q55_8
                   +Q55_Q55_9)/9     
neg_emotion_pre<-(Q54_Q54_10+Q54_Q54_11+Q54_Q54_12+Q54_Q54_13+Q54_Q54_14+Q54_Q54_15+Q54_Q54_16+Q54_Q54_17
                   +Q54_Q54_18)/9  
neg_emotion_host<-(Q55_Q55_10+Q55_Q55_11+Q55_Q55_12+Q55_Q55_13+Q55_Q55_14+Q55_Q55_15+Q55_Q55_16+Q55_Q55_17
                   +Q55_Q55_18)/9 
ap_pos_emotion_pre <- psych::alpha(pos_emotion_pre_total)
ap_pos_emotion_pre<- round(ap_pos_emotion_pre$total[1],2)
ap_pos_emotion_host <- psych::alpha(pos_emotion_host_total)
ap_pos_emotion_host<- round(ap_pos_emotion_host$total[1],2)
ap_neg_emotion_pre <- psych::alpha(neg_emotion_pre_total)
ap_neg_emotion_pre<- round(ap_neg_emotion_pre$total[1],2)
ap_neg_emotion_host <- psych::alpha(neg_emotion_host_total)
ap_neg_emotion_host<- round(ap_neg_emotion_host$total[1],2)
  
detach(valid.data2)

### end preparing ###

### information of participants ###
participant.age<-as.numeric(as.character(valid.data[,"Q10"])) #transform factor to numeric
participant.gender<-  factor(valid.data$Q9,
                             levels = c(1, 2),
                             labels = c("Male", "Femle"))
age <- summary(participant.age)
gender <- summary(participant.gender)

dp<-c(as.numeric(as.character(valid.data$Q30)))
por<-c(as.numeric(as.character(valid.data$Q31)))
pod<-c(as.numeric(as.character(valid.data$Q32)))

clean_data <- data.frame(iv_age,iv_evidence,free_will,just_sense,just_belief,pos_emotion_host,neg_emotion_host,
                         dp,por,pod,scientific,fatalistic,unpreditic,SES,pos_emotion_pre,neg_emotion_pre,participant.age
                         ,participant.gender)


#ANOVA for three different dvs
dp_anova <- summary(aov(clean_data$dp~clean_data$iv_age*clean_data$iv_evidence))
por_anova <- summary(aov(clean_data$por~clean_data$iv_age*clean_data$iv_evidence))
pod_anova <- summary(aov(clean_data$pod~clean_data$iv_age*clean_data$iv_evidence))

library(MBESS)
ci.pvaf(F.value=21.773,df.1=1,df.2=338,N=344,conf.level=.90)
ci.pvaf(F.value=9.171,df.1=2,df.2=338,N=344,conf.level=.90)

#multivariable linear regression

#test Multicollinearity
regression_data<-data.frame(free_will,just_belief,just_sense)
regression_data<-regression_data[-1:-4,] # there is 4 NA in the free_will, we delete them here 
cor_regression_data<-cor(regression_data)
mcl_test<-kappa(cor_regression_data) #here,we use "kappa" to test multicollinearity,K<100 means multicollinearity is small

dp_reg<-dp[-1:-4]
dp_lm<-summary(lm(dp_reg~free_will++just_sense++just_belief,data=regression_data))

por_reg<-por[-1:-4]
por_lm<-summary(lm(por_reg~free_will++just_sense++just_belief,data=regression_data))

pod_reg<-pod[-1:-4]
pod_lm<-summary(lm(pod_reg~free_will++just_sense++just_belief,data=regression_data))


#random Forest
library("randomForest")
library("party")
library("lattice")

##predict dp
set.seed(660)
ctree_dp <- ctree(clean_data$dp ~ ., data = clean_data)
plot(ctree_dp)

set.seed (5)
data.controls <- cforest_unbiased(ntree=1000, mtry=4)
mycforest_dp<- cforest(clean_data$dp ~ ., data = clean_data,
                    control = data.controls)
myvarimp_dp<-varimp(mycforest_dp)
as.data.frame(myvarimp_dp)
write.csv(myvarimp, file= 'myvarimp1try4tree1kseed1_dp.csv')
dev.off()
postscript("try4tree1kseed1_dp.eps",  horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)
dotplot(sort(myvarimp_dp), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})
dev.off()
y_hat<-predict(mycforest_dp)
write.csv(y_hat, file='yhattry4tree1kseed1.csv')

##predict por
set.seed(660)
ctree_por <- ctree(clean_data$por ~ ., data = clean_data)
plot(ctree_por)

set.seed (5)
data.controls <- cforest_unbiased(ntree=1000, mtry=4)
mycforest_por<- cforest(clean_data$por ~ ., data = clean_data,
                       control = data.controls)
myvarimp_por<-varimp(mycforest_por)
as.data.frame(myvarimp_por)
write.csv(myvarimp_por, file= 'myvarimp1try4tree1kseed1_por.csv')
dev.off()
postscript("try4tree1kseed1_por.eps",  horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)
dotplot(sort(myvarimp_por), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})
dev.off()
y_hat<-predict(mycforest_por)
write.csv(y_hat, file='yhattry4tree1kseed1.csv')

##predict pod
set.seed(660)
ctree_pod <- ctree(clean_data$pod ~ ., data = clean_data)
plot(ctree_pod)

set.seed (5)
data.controls <- cforest_unbiased(ntree=1000, mtry=4)
mycforest_pod<- cforest(clean_data$pod ~ ., data = clean_data,
                        control = data.controls)
myvarimp_pod<-varimp(mycforest_pod)
as.data.frame(myvarimp_pod)
write.csv(myvarimp_por, file= 'myvarimp1try4tree1kseed1_pod.csv')
dev.off()
postscript("try4tree1kseed1_pod.eps",  horizontal = FALSE, onefile = FALSE, paper = "special", height = 10, width = 10)
dotplot(sort(myvarimp_pod), xlab="Variable Importance (predictors to right of dashed line differ from noise)", panel=function(x,y) {panel.dotplot(x, y, col='darkblue', pch=16, cex=1.1) 
  panel.abline(v=abs(min(myvarimp)), col='red', lty='longdash', lwd=2)})
dev.off()
y_hat<-predict(mycforest_pod)
write.csv(y_hat, file='yhattry4tree1kseed1.csv')

