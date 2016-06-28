library(caret)
##reading training dataset ##
train<-read.csv("C:/project/Dataset/trainHistory.csv")
## reading offers dataset##
offers<-read.csv("C:/project/Dataset/offers.csv")
##merging datasets##
shoppersone<-merge(train,offers,by="offer")
dim(shoppersone)
names(shoppersone)
#view(shoppersone)
##selecting required data for analysis##
shoppers<-subset(shoppersone,select=-c(id,repeattrips,offerdate,quantity,offervalue))
dim(shoppers)
#view(shoppers)
summary(shoppers)
##density plots##
qplot(market,offer,colour=repeater,data=shoppers)
##pre-processing the data##
pre<-preProcess(shoppers[,-4],method=c("center","scale"))
shopper<-predict(pre,shoppers[,-4])
shopper$repeater<-shoppers$repeater
shoppers<-shopper
##analysing pre-processed data##
summary(shoppers)
##density plot after preprocesing##
qplot(market,offer,colour=repeater,data=shoppers)
##splitting data##
library(caTools)
set.seed(100)
split=sample.split(shoppers$repeater,SplitRatio = 0.65)
shopperstrain=subset(shoppers,split==TRUE)
shopperstest=subset(shoppers,split==FALSE)
##Building  logistic Regression Model##
model=glm(repeater~.,data=shopperstrain,family=binomial)
summary(model)
cor(shopperstrain[,-7])
model2=glm(repeater~offer+chain+market+category+brand,data=shopperstrain,family=binomial)
shopperspredict=predict(model2,newdata=shopperstest,type="response")
table(shopperstest$repeater,shopperspredict>0.4)
##Building CART model##
library(ROCR)
rocrpred=predict(shopperspredict,shopperstest$repeater)
as.numeric(performance(rocrpred,"auc")@y.values)
library(rpart)
library(rpart.plot)
shopperstree=rpart(repeater~.,data=shopperstrain,method="class",cp=0.00005)
predictshop=predict(shopperstree,newdata=shopperstest,type="class")
table(shopperstest$repeater,predictshop)
##Building Random forest Model##
library(randomForest)
shopperstrain$repeater=as.factor(shopperstrain$repeater)
stevensforest=randomForest(repeater~.,data=shopperstrain,ntree=200,nodesize=25)
predictforest=predict(stevensforest,newdata=shopperstest)
table(shopperstest$repeater,predictforest)
##Reading test data##
test<-read.csv("C:/project/Dataset/testHistory.csv")
test<-merge(test,offers,by="offer")
test=subset(test,select=-c(id,offerdate,quantity,offervalue))
##Predicting on test set usng CART##
predicttest=predict(shopperstree,newdata=test,type="class")
summary(predicttest)
##The end##


