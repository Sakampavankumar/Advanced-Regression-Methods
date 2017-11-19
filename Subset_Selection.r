#Subset Regression
#Load the packages 
library(glmnet) #Lasso and ridge regression 
library(ISLR) #Collection of Data Sets 
library(caret) #Classification and Regression Training
library(leaps) #

#Load the data
FStat <- read.csv('http://math.mercyhurst.edu/~sousley/STAT_139/data/FStat.csv')

colnames(FStat)

WMStat<-FStat[which(FStat$PopSex== "WM"),]

#First we will perform subset selection

str(WMStat)

#Remove NA's 
WMStat <- na.omit(WMStat)

str(WMStat)

reg.ss <- regsubsets(FSTAT ~ claxln+scapht+scapbr+humxln+radxln+ulnxln+sacaht+femxln+fembln+tibxln+fibxln, data = WMStat)

summary1 <- summary(reg.ss)

summary1$outmat

summary1$rsq


#Let's plot some results 
#par(mfrow=c(2,2))
plot(summary1$rss,xlab=" Number of Variables ",ylab=" RSS", type = 'l')

plot(summary1$adjr2,xlab=" Number of Variables ", ylab=" Adjusted RSq",type = 'l')


bestset<-which.max(summary1$adjr2)
bestset
points (bestset, summary1$adjr2[bestset], col = "red",cex= 1.5, pch= 20)



plot(summary1$cp,xlab=" Number of Variables ", ylab= "Cp", type = 'l')

cpmin<-which.min(summary1$cp)
cpmin

points (cpmin, summary1$cp[cpmin], col ="red",cex= 1.5, pch= 20)
bicmin<-which.min(summary1$bic)
bicmin

plot(summary1$bic,xlab=" Number of Variables ", ylab= " BIC",type= 'l')

points (bicmin, summary1$bic[3], col =" red", cex= 2, pch= 20)
par(mfrow=c(1,1))

#We can also do other diagnostic graphs related to variable importance 

plot(reg.ss,scale="r2")
plot(reg.ss,scale="adjr2")
plot(reg.ss,scale="Cp")
plot(reg.ss,scale="bic")

coef(reg.ss, bicmin)

#We can also use forward stepwise selection of predictors using regsubsetsandforward
reg.fwd<-regsubsets(FSTAT ~., data= WMStat,nvmax= 18, method = "forward")
summary(reg.fwd)


which.min(summary(reg.fwd)$bic)
which.max(summary(reg.fwd)$adjr2)
which.min(summary(reg.fwd)$cp)


#Let's compare best-subsets and forward-stepwise models
coef(reg.ss, 3)

coef(reg.fwd, 3)

coef(reg.ss, 4)

coef(reg.fwd, 4)
