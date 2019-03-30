### code for analysis of neuroimage bias ### 

# code author:Ji-Xing Yin
# email:jixing-yin@outlook.com


# Author       date
# ==========   ======
# jixing-yin   18-4-5

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
#atcbs(ability to come back society)ï¼?1-7

#meditating variable
#fr(free wil):1-5
#js(justice sense):0-5
#jwb(just world belief):1-6
#ds(disgust sense):1-4
#kotl(knowledge of the law):1-7
#kots(knowledge of the science):1-7
#bitotc(belief in truth of the case):1-7


### preparing ###
Sys.setlocale("LC_ALL", "English")  # set local encoding to English
Sys.setenv(LANG = "en") # set the feedback language to English

rm(list = setdiff(ls(), lsf.str())) # remove all variables except functions

## read data 
total.data<-read.csv("study1 combined duplicate.csv",header = TRUE,sep = ',', stringsAsFactors=FALSE,na.strings=c(""," ","NA"))

## exclude participants who pay insufficient attention to the test problem 
valid.data<-subset(total.data,Q38=="4"&Q3_13=="3",header=ture) # select the participants whose Q38=4 and Q3_13=3
valid.data<-data.frame(valid.data)

## select data of the independent variable ##

age18<-subset(valid.data,Q23=="1"|Q25=="1"|Q28=="1")
age38<-subset(valid.data,Q24=="1"|Q27=="1"|Q26=="1")
be_no_b<-subset(valid.data,Q23=="1"|Q24=="1")
b_no_b<-subset(valid.data,Q25=="1"|Q27=="1")
b_b<-subset(valid.data,Q28=="1"|Q26=="1")


age18be_no_b<-subset(valid.data,Q23=="1")
age18b_no_b<-subset(valid.data,Q25=="1")
age18b_b<-subset(valid.data,Q28=="1")
age38be_no_b<-subset(valid.data,Q24=="1")
age38b_no_b<-subset(valid.data,Q27=="1")
age38b_b<-subset(valid.data,Q26=="1")

#be_no_b means behavior evidence with no brain image
#b_no_b means brain evidence with no brain image
#b_b means brain evidence with brain image


##select data of the scales

valid.data2<-lapply(valid.data,as.numeric)

attach(valid.data2)

#free will
free_will<-(Q3_3+Q3_8+Q3_12+Q3_16)/4

#scientific determinism
scidetermine<-(Q3_2+Q3_10+Q3_14+Q3_15)/4

#fate determinism
fatedetermine<-(Q3_1+Q3_4+Q3_6+Q3_9)/4

#unpredictable
unpredictable<-(Q3_5+Q3_7+Q3_11+Q3_17)/4

#just sense
just_sense<-(Q4_1+Q4_2+Q4_3+Q4_4+Q4_5+Q4_6+Q4_7+Q4_8)/8

detach(valid.data2)

### end preparing ###


### information of participants ###
participant.age<-as.numeric(as.character(valid.data[,"Q14"])) #transform factor to numeric
psych::describe(participant.age,na.rm=TRUE)

### analysis ###

##   1   ##
## death penalty ##

dp<-c(as.numeric(as.character(valid.data$Q34)))
dp1<-c(as.numeric(as.character(age18be_no_b$Q34)))
dp2<-c(as.numeric(as.character(age18b_no_b$Q34)))
dp3<-c(as.numeric(as.character(age18b_b$Q34)))
dp4<-c(as.numeric(as.character(age38be_no_b$Q34)))
dp5<-c(as.numeric(as.character(age38b_no_b$Q34)))
dp6<-c(as.numeric(as.character(age38b_b$Q34)))


# here,I used length() to calculate the length of vectors above and it is how I got the "times".

df_dp1<-data.frame(dp1,rep("age18",times=19),rep("be_no_b",times=19))
df_dp2<-data.frame(dp2,rep("age18",times=21),rep("b_no_b",times=21))
df_dp3<-data.frame(dp3,rep("age18",times=18),rep("b_b",times=18))
df_dp4<-data.frame(dp4,rep("age38",times=17),rep("be_no_b",times=17))
df_dp5<-data.frame(dp5,rep("age38",times=22),rep("b_no_b",times=22))
df_dp6<-data.frame(dp6,rep("age38",times=20),rep("b_b",times=20))                

# rename the data.frame 

colnames(df_dp1)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp2)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp3)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp4)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp5)[1:3]<-c("dp","dp_age","dp_evidence")
colnames(df_dp6)[1:3]<-c("dp","dp_age","dp_evidence")

#merge the data.frame above

df_dp_total<-rbind(df_dp1,df_dp2,df_dp3,df_dp4,df_dp5,df_dp6)
table(df_dp_total)

dp_anova1<-aov(dp~dp_age*dp_evidence,data=df_dp_total) # the effect of age & the effect of evidence when age is controlled
summary(dp_anova1)

MBESS::ci.pvaf(F.value=1.272,df.1=1,df.2=113,N=119)

dp_anova2<-aov(dp~dp_evidence*dp_age,data=df_dp_total) # the effect of evidence & the effect of age when evidence is controlled
summary(dp_anova2)

MBESS::ci.pvaf(F.value=3.792,df.1=2,df.2=113,N=119)

interaction.plot(df_dp_total$dp_age,df_dp_total$dp_evidence,df_dp_total$dp,type="b",col=c("red","blue"),main="interaction between age and evidence")

#multivariable linear regression

lm.sol<-lm(dp~free_will+scidetermine+fatedetermine+unpredictable+just_sense,data=valid.data)
summary(lm.sol)


##   2   ##
## the perception of responsibility  ##

por<-c(as.numeric(as.character(valid.data$Q33)))
por1<-c(as.numeric(as.character(age18be_no_b$Q33)))
por2<-c(as.numeric(as.character(age18b_no_b$Q33)))
por3<-c(as.numeric(as.character(age18b_b$Q33)))
por4<-c(as.numeric(as.character(age38be_no_b$Q33)))
por5<-c(as.numeric(as.character(age38b_no_b$Q33)))
por6<-c(as.numeric(as.character(age38b_b$Q33)))

# here,I used length() to calculate the length of vectors above and it is how I got the "times".

df_por1<-data.frame(por1,rep("age18",times=19),rep("be_no_b",times=19))
df_por2<-data.frame(por2,rep("age18",times=21),rep("b_no_b",times=21))
df_por3<-data.frame(por3,rep("age18",times=18),rep("b_b",times=18))
df_por4<-data.frame(por4,rep("age38",times=17),rep("be_no_b",times=17))
df_por5<-data.frame(por5,rep("age38",times=22),rep("b_no_b",times=22))
df_por6<-data.frame(por6,rep("age38",times=20),rep("b_b",times=20))   

# rename the data.frame 

colnames(df_por1)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por2)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por3)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por4)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por5)[1:3]<-c("por","por_age","por_evidence")
colnames(df_por6)[1:3]<-c("por","por_age","por_evidence")

#merge the data.frame above

df_por_total<-rbind(df_por1,df_por2,df_por3,df_por4,df_por5,df_por6)
table(df_por_total)


por_anova1<-aov(por~por_age*por_evidence,data=df_por_total) # the effect of age & the effect of evidence when age is controlled
summary(por_anova1)

MBESS::ci.pvaf(F.value=1.034,df.1=1,df.2=113,N=119)

por_anova2<-aov(por~por_evidence*por_age,data=df_por_total) # the effect of evidence & the effect of age when evidence is controlled
summary(por_anova2)

MBESS::ci.pvaf(F.value=0.235,df.1=2,df.2=113,N=119)

interaction.plot(df_por_total$por_age,df_por_total$por_evidence,df_por_total$por,type="b",col=c("red","blue"),main="interaction between age and evidence")

#multivariable linear regression

lm.sol<-lm(por~free_will+scidetermine+fatedetermine+unpredictable+just_sense,data=valid.data)
summary(lm.sol)

##   3   ##
##   perception of danger  ##

pod<-c(as.numeric(as.character(valid.data$Q35)))
pod1<-c(as.numeric(as.character(age18be_no_b$Q35)))
pod2<-c(as.numeric(as.character(age18b_no_b$Q35)))
pod3<-c(as.numeric(as.character(age18b_b$Q35)))
pod4<-c(as.numeric(as.character(age38be_no_b$Q35)))
pod5<-c(as.numeric(as.character(age38b_no_b$Q35)))
pod6<-c(as.numeric(as.character(age38b_b$Q35)))

# here,I used length() to calculate the length of vectors above and it is how I got the "times".

df_pod1<-data.frame(pod1,rep("age18",times=19),rep("be_no_b",times=19))
df_pod2<-data.frame(pod2,rep("age18",times=21),rep("b_no_b",times=21))
df_pod3<-data.frame(pod3,rep("age18",times=18),rep("b_b",times=18))
df_pod4<-data.frame(pod4,rep("age38",times=17),rep("be_no_b",times=17))
df_pod5<-data.frame(pod5,rep("age38",times=22),rep("b_no_b",times=22))
df_pod6<-data.frame(pod6,rep("age38",times=20),rep("b_b",times=20))   

# rename the data.frame 

colnames(df_pod1)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod2)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod3)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod4)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod5)[1:3]<-c("pod","pod_age","pod_evidence")
colnames(df_pod6)[1:3]<-c("pod","pod_age","pod_evidence")

#merge the data.frame above

df_pod_total<-rbind(df_pod1,df_pod2,df_pod3,df_pod4,df_pod5,df_pod6)
table(df_pod_total)


pod_anova1<-aov(pod~pod_age*pod_evidence,data=df_pod_total) # the effect of age & the effect of evidence when age is controlled
summary(pod_anova1)

MBESS::ci.pvaf(F.value=0.556,df.1=1,df.2=113,N=119)

pod_anova2<-aov(pod~pod_evidence*pod_age,data=df_pod_total) # the effect of evidence & the effect of age when evidence is controlled
summary(pod_anova2)

MBESS::ci.pvaf(F.value=1.326,df.1=2,df.2=113,N=119)

interaction.plot(df_pod_total$pod_age,df_pod_total$pod_evidence,df_pod_total$pod,type="b",col=c("red","blue"),main="interaction between age and evidence")

#multivariable linear regression

lm.sol<-lm(pod~free_will+scidetermine+fatedetermine+unpredictable+just_sense,data=valid.data)
summary(lm.sol)





