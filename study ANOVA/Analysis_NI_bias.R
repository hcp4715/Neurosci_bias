### an ANOVA of the death penalty ###

# general comments by hcp:
# First, you are not starting with the raw data, that means you have done something manually.
#       This approach is not optimal, since we are using the R script, we will do everything,
#       in R, instead of select data by your hands and then use R. You can read my code, I load
#       all raw data in stead of part of them
# Second, DO NOT USE "=" to assign value, use "<-";
# Third, add more annotations to your code
# Forth, Read my comment carefully, and try to change, I think I already suggested you to read all
#      data and do the analysis, but you seems ignored my suggestions.
# Fifth, calculate the reliability of the questionnaire included, calculate and report the 
#       demographic information of the participant, which you didn't do. All analysis start from
#       descriptive statistics. You need to plot the distribution of the age, SES, and other variables

## read csv Q34 ##
# hcp: why not read all the whole dataset?

dataQ34=read.csv("Q34.csv")
dfQ34=data.frame(dataQ34)

##

## a double factor variance analysis ##
# hcp: you can do all this in a same dataframe, instead of split them all
benob18=as.vector(dfQ34$X18_be_no_b)
bnob18=as.vector(dfQ34$X18_b_no_b)
bb18=as.vector(dfQ34$X18_b_b)
benob38=as.vector(dfQ34$X38_be_no_b)
bnob38=as.vector(dfQ34$X38_b_no_b)
bb38=as.vector(dfQ34$X38_b_b)

# hcp: the colname as A B is nonsense, you'be better use column names that people can understand 
#      as soon as they see the name.

dp=data.frame(Y=c(benob18,bnob18,bb18,benob38,bnob38,bb38),A=gl(2,357,714),B=gl(3,119,714))

dp.aov=aov(Y~A+B+A:B,data=dp)

summary(dp.aov)

##

## a homogeneity of variance test ##

bartlett.test(Y~A,data=dp)
bartlett.test(Y~B,data=dp)

##

###

### an ANOVA of the perception of responsibility Q33 ###

## read csv Q33 ##

dataQ33=read.csv("Q33.csv")
dfQ33=data.frame(dataQ33)

##

## a double factor variance analysis ##

benob18=as.vector(dfQ33$X18_be_no_b)
bnob18=as.vector(dfQ33$X18_b_no_b)
bb18=as.vector(dfQ33$X18_b_b)
benob38=as.vector(dfQ33$X38_be_no_b)
bnob38=as.vector(dfQ33$X38_b_no_b)
bb38=as.vector(dfQ33$X38_b_b)


por=data.frame(Y=c(benob18,bnob18,bb18,benob38,bnob38,bb38),A=gl(2,357,714),B=gl(3,119,714))

por.aov=aov(Y~A+B+A:B,data=por)

summary(por.aov)

##

## a homogeneity of variance test ##

bartlett.test(Y~A,data=por)
bartlett.test(Y~B,data=por)

##

###

### an ANOVA of the perception of danger Q35 ###

## read csv Q35 ##

dataQ35=read.csv("Q35.csv")
dfQ35=data.frame(dataQ35)

##

## a double factor variance analysis ##

benob18=as.vector(dfQ35$X18_be_no_b)
bnob18=as.vector(dfQ35$X18_b_no_b)
bb18=as.vector(dfQ35$X18_b_b)
benob38=as.vector(dfQ35$X38_be_no_b)
bnob38=as.vector(dfQ35$X38_b_no_b)
bb38=as.vector(dfQ35$X38_b_b)


pod=data.frame(Y=c(benob18,bnob18,bb18,benob38,bnob38,bb38),A=gl(2,357,714),B=gl(3,119,714))

pod.aov=aov(Y~A+B+A:B,data=pod)

summary(pod.aov)

##

## a homogeneity of variance test ##

bartlett.test(Y~A,data=pod)
bartlett.test(Y~B,data=pod)

##

###