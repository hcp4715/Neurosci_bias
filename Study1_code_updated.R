### code for analysis of neuroimage bias ### 

# code author:Ji-Xing Yin
# email:jixing-yin@outlook.com


# Author       date
# ==========   ======
# jixing-yin   18-4-5
# Yuepei XU    19-3-1

# input data
# oringinal file: "study1 combined duplicate.csv"

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
total.data<-read.csv("Study1_data.csv",header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

## exclude participants who pay insufficient attention to the test problem and the scenarios
valid.data<-subset(total.data,Q34=="4"&Q2_13=="3",header=ture) # select the participants whose Q34=4 and Q2_13=3
valid.data<-subset(valid.data,((valid.data$X18b =="2"|valid.data$X18bb=="2"|valid.data$X18no=="2")& Q25<=18)
                   |((valid.data$X38b=="2"|valid.data$X38bb=="2"|valid.data$X38no=="2")& valid.data$Q25>18))
check_missing1 <- is.na(valid.data$Q30)
check_missing2 <- is.na(valid.data$Q31)
check_missing3 <- is.na(valid.data$Q32)
valid.data<-subset(valid.data,check_missing1==FALSE&check_missing2==FALSE&check_missing3==FALSE)
valid.data<-data.frame(valid.data)

## select data of the independent variable ##

age18<-subset(valid.data,valid.data$X18b =="2"|valid.data$X18bb=="2"|valid.data$X18no=="2")
age38<-subset(valid.data,valid.data$X38b=="2"|valid.data$X38bb=="2"|valid.data$X38no=="2")
be_no_b<-subset(valid.data,valid.data$X18no =="2"|valid.data$X38no=="2")
b_no_b<-subset(valid.data,valid.data$X18b =="2"|valid.data$X38b=="2")
b_b<-subset(valid.data,valid.data$X18bb =="2"|valid.data$X38bb=="2")

age18be_no_b<-subset(valid.data,valid.data$X18no=="2")
age18b_no_b<-subset(valid.data,valid.data$X18b=="2")
age18b_b<-subset(valid.data,valid.data$X18bb=="2")
age38be_no_b<-subset(valid.data,valid.data$X38no=="2")
age38b_no_b<-subset(valid.data,valid.data$X38b=="2")
age38b_b<-subset(valid.data,valid.data$X38bb=="2")

#be_no_b means behavior evidence with no brain image
#b_no_b means brain evidence with no brain image
#b_b means brain evidence with brain image


##select data of the scales

valid.data2<-lapply(valid.data,as.numeric)

attach(valid.data2)

library("psych")

#free will
free_will_total<- data.frame(Q2_3,Q2_8,Q2_12,Q2_16)
scientific_total <- data.frame(Q2_2,Q2_10,Q2_14,Q2_15)
fatalistic_total <- data.frame(Q2_1,Q2_4,Q2_6,Q2_9)
unpreditic_total<- data.frame(Q2_5,Q2_7,Q2_11,Q2_17)
free_will<- ((Q2_3+Q2_8+Q2_12+Q2_16)/4)
scientific <- ((Q2_2+Q2_10+Q2_14+Q2_15)/4)
fatalistic <- ((Q2_1+Q2_4+Q2_6+Q2_9)/4)
unpreditic<- ((Q2_5+Q2_7+Q2_11+Q2_17)/4)

ap_free_will <- psych::alpha(free_will_total)
ap_free_will <- round(ap_free_will$total[1],2)
ap_scientific <- psych::alpha(scientific_total)
ap_scientific <- round(ap_scientific$total[1],2)
ap_fatalistic <- psych::alpha(fatalistic_total)
ap_fatalistic <- round(ap_fatalistic$total[1],2)
ap_unpreditic <- psych::alpha(unpreditic_total)
ap_unpreditic <- round(ap_unpreditic$total[1],2)

#just sense
just_sense_total<- data.frame(Q3_1,Q3_2,Q3_3,Q3_4,Q3_5,Q3_6,Q3_7,Q3_8)
just_sense<-(Q3_1+Q3_2+Q3_3+Q3_4+Q3_5+Q3_6+Q3_7+Q3_8)/8
ap_just_sense <- psych::alpha(just_sense_total)
ap_just_sense <- round(ap_just_sense$total[1],2)

#just world belief
just_belief_total<- data.frame(Q8_1,Q8_2,Q8_3,Q8_4,Q8_5,Q8_6,Q8_7,Q8_8,Q8_9,Q8_10,Q8_11,Q8_12,Q8_13)
just_belief <- (Q8_1+Q8_2+Q8_3+Q8_4+Q8_5+Q8_6+Q8_7+Q8_8+Q8_9+Q8_10+Q8_11+Q8_12+Q8_13)/13
ap_just_belief <- psych::alpha(just_belief_total)
ap_just_belief <- round(ap_just_belief$total[1],2)

detach(valid.data2)

### end preparing ###

### information of participants ###
participant.age<-as.numeric(as.character(valid.data[,"Q10"])) #transform factor to numeric
participant.gender<-  factor(valid.data$Q9,
                             levels = c(1, 2),
                             labels = c("Male", "Femle"))
age <- summary(participant.age)
gender <- summary(participant.gender)

### analysis ###

##   caculate the depentent varibles   ##

# death penalty #

dp<-c(as.numeric(as.character(valid.data$Q30)))
dp1<-c(as.numeric(as.character(age18be_no_b$Q30)))
dp2<-c(as.numeric(as.character(age18b_no_b$Q30)))
dp3<-c(as.numeric(as.character(age18b_b$Q30)))
dp4<-c(as.numeric(as.character(age38be_no_b$Q30)))
dp5<-c(as.numeric(as.character(age38b_no_b$Q30)))
dp6<-c(as.numeric(as.character(age38b_b$Q30)))

## the perception of responsibility  ##

por<-c(as.numeric(as.character(valid.data$Q31)))
por1<-c(as.numeric(as.character(age18be_no_b$Q31)))
por2<-c(as.numeric(as.character(age18b_no_b$Q31)))
por3<-c(as.numeric(as.character(age18b_b$Q31)))
por4<-c(as.numeric(as.character(age38be_no_b$Q31)))
por5<-c(as.numeric(as.character(age38b_no_b$Q31)))
por6<-c(as.numeric(as.character(age38b_b$Q31)))


##   perception of danger  ##

pod<-c(as.numeric(as.character(valid.data$Q32)))
pod1<-c(as.numeric(as.character(age18be_no_b$Q32)))
pod2<-c(as.numeric(as.character(age18b_no_b$Q32)))
pod3<-c(as.numeric(as.character(age18b_b$Q32)))
pod4<-c(as.numeric(as.character(age38be_no_b$Q32)))
pod5<-c(as.numeric(as.character(age38b_no_b$Q32)))
pod6<-c(as.numeric(as.character(age38b_b$Q32)))


# here,I used length() to calculate the length of vectors above and it is how I got the "times".

df_dp1<-data.frame(dp1,rep("age18",times=12),rep("be_no_b",times=12))
df_dp2<-data.frame(dp2,rep("age18",times=17),rep("b_no_b",times=17))
df_dp3<-data.frame(dp3,rep("age18",times=15),rep("b_b",times=15))
df_dp4<-data.frame(dp4,rep("age38",times=14),rep("be_no_b",times=14))
df_dp5<-data.frame(dp5,rep("age38",times=18),rep("b_no_b",times=18))
df_dp6<-data.frame(dp6,rep("age38",times=18),rep("b_b",times=18))

df_por1<-data.frame(por1,rep("age18",times=12),rep("be_no_b",times=12))
df_por2<-data.frame(por2,rep("age18",times=17),rep("b_no_b",times=17))
df_por3<-data.frame(por3,rep("age18",times=15),rep("b_b",times=15))
df_por4<-data.frame(por4,rep("age38",times=14),rep("be_no_b",times=14))
df_por5<-data.frame(por5,rep("age38",times=18),rep("b_no_b",times=18))
df_por6<-data.frame(por6,rep("age38",times=18),rep("b_b",times=18))   

df_pod1<-data.frame(pod1,rep("age18",times=12),rep("be_no_b",times=12))
df_pod2<-data.frame(pod2,rep("age18",times=17),rep("b_no_b",times=17))
df_pod3<-data.frame(pod3,rep("age18",times=15),rep("b_b",times=15))
df_pod4<-data.frame(pod4,rep("age38",times=14),rep("be_no_b",times=14))
df_pod5<-data.frame(pod5,rep("age38",times=18),rep("b_no_b",times=18))
df_pod6<-data.frame(pod6,rep("age38",times=18),rep("b_b",times=18))  

# rename the data.frame 

colnames(df_dp1)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp2)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp3)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp4)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp5)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp6)[1:3]<-c("dp","dp_age","dp_evidence")

colnames(df_por1)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por2)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por3)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por4)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por5)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por6)[1:3]<-c("por","por_age","por_evidence")

colnames(df_pod1)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod2)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod3)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod4)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod5)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod6)[1:3]<-c("pod","pod_age","pod_evidence")

#merge all the data.frame above

df_dp_total<-rbind(df_dp1,df_dp2,df_dp3,df_dp4,df_dp5,df_dp6)
df_por_total<-rbind(df_por1,df_por2,df_por3,df_por4,df_por5,df_por6)
df_pod_total<-rbind(df_pod1,df_pod2,df_pod3,df_pod4,df_pod5,df_pod6)

df_por_total<-as.data.frame(df_por_total)
df_pod_total<-as.data.frame(df_pod_total)

data_total <- cbind(df_por_total$por,df_pod_total$pod,df_dp_total)
colnames(data_total)[1:5]<-c("pod","por","dp","age","evidence")
data_total <- na.omit(data_total)  #double check for missing value

#ANOVA for three different dvs
library(MBESS)
dp_anova <- summary(aov(data_total$dp~data_total$age*data_total$evidence))
ci.pvaf(F.value=4.822,df.1=1,df.2=88,N=94,conf.level=.90)

por_anova <- summary(aov(data_total$por~data_total$age*data_total$evidence))
pod_anova <- summary(aov(data_total$pod~data_total$age*data_total$evidence))

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


