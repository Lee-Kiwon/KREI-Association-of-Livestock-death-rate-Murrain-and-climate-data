  

Chicken<-read.csv("C:/Users/wrwqr/OneDrive/Desktop/농산물 예측모형/rice.csv")
Cow<-read.csv("C:/Users/wrwqr/OneDrive/Desktop/농산물 예측모형/rice.csv")
Pigg<-read.csv("C:/Users/wrwqr/OneDrive/Desktop/농산물 예측모형/rice.csv")



library(sas7bdat)
library(dplyr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(purrr)
library(tidyr)
library(ggplot2)
library(Epi)
library(adabag)


Chicken %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

Chicken_scale<-Chicken
Pig_scale<-Pig
Cow_scale<-Cow

Chicken_scale<-transform(Chicken, Count = scale(Chicken$Count),MAX_T=scale(Chicken$MAX_T),AVG_H=scale(Chicken$AVG_H),MIN_H=scale(Chicken$MIN_H),RAIN=scale(Chicken$RAIN),WIND=scale(Chicken$WIND),CLOUD=scale(Chicken$CLOUD),SUN_R=scale(Chicken$SUN_R),SNOW=scale(Chicken$SNOW),DUST=scale(Chicken$DUST))

Pig_scale<-transform(Pig, Count = scale(Pig$Count),MAX_T=scale(Pig$MAX_T),AVG_H=scale(Pig$AVG_H),MIN_H=scale(Pig$MIN_H),RAIN=scale(Pig$RAIN),WIND=scale(Pig$WIND),CLOUD=scale(Pig$CLOUD),SUN_R=scale(Pig$SUN_R),SNOW=scale(Pig$SNOW),DUST=scale(Pig$DUST))

Cow_scale<-transform(scale(Cow))

attach(Chicken_scale)
Chicken_glm<-glm(Count~MAX_T+AVG_H+MIN_H+RAIN+WIND+CLOUD+SUN_R+SNOW+DUST)
summary(Chicken_glm)
detach(Chicken_scale)

attach(Pig_scale)
Pig_glm<-glm(Count~MAX_T+AVG_H+MIN_H+RAIN+WIND+CLOUD+SUN_R+SNOW+DUST)
summary(Pig_glm)
detach(Pig_scale)

Cow_glm<-glm(Count~.,data=Cow_scale)
summary(Cow_glm)

ORtable=function(x,digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result,digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5%","p")
  result
}

ORtable(Chicken_glm)
ORtable(Pig_glm)
ORtable(Cow_glm)

library(moonBook)
odds_ratio = ORtable(Pig_glm)
odds_ratio = odds_ratio[2:nrow(odds_ratio),]
HRplot(odds_ratio, type=2,show.CI=TRUE,cex=2)

odds_ratio = ORtable(Chicken_glm)
odds_ratio = odds_ratio[2:nrow(odds_ratio),]
HRplot(odds_ratio, type=2,show.CI=TRUE,cex=2)

odds_ratio = ORtable(Cow_glm)
odds_ratio = odds_ratio[2:nrow(odds_ratio),]
HRplot(odds_ratio, type=2,show.CI=TRUE,cex=2)




#colnames(Chicken_scale) <- c('Count','MAX_T',	'AVG_H',	'MIN_H',	'RAIN',	'WIND',	'CLOUD',	'SUN_R',	'SNOW',	'DUST')


set.seed(123)
idx<-sample(1:nrow(Chicken_scale),nrow(Chicken_scale)*0.7,replace=FALSE)
atrainp2<-Chicken_scale[idx,]
atestp2<-Chicken_scale

set.seed(123)
idx<-sample(1:nrow(Pig_scale),nrow(Pig_scale)*0.7,replace=FALSE)
atrainp3<-Pig_scale[idx,]
atestp3<-Pig_scale[-idx,]


set.seed(123)
idx<-sample(1:nrow(Cow_scale),nrow(Cow_scale)*0.7,replace=FALSE)
atrainp4<-Cow_scale[idx,]
atestp4<-Cow_scale[-idx,]

library(randomForest)

rf_chicken<-randomForest(Count~.,data=atrainp2,ntree=10000,mtry=sqrt(40),importance=F)
rf_chicken
rf_chicken$importance
varImpPlot(rf_chicken)

rf_pig<-randomForest(Count~.,data=atrainp3,ntree=10000,mtry=sqrt(40),importance=F)
rf_pig
rf_pig$importance
varImpPlot(rf_pig)


rf_cow<-randomForest(Count~.,data=atrainp4,ntree=10000,mtry=sqrt(40),importance=F)
rf_cow
rf_cow$importance
varImpPlot(rf_cow)

library(caret)
library(Metrics)

pred.rf_chicken<-predict(rf_chicken,atestp2[,-1])
pred.rf_chicken1<-as.numeric(pred.rf_chicken)
atestp21<-unlist(atestp2)
atestp21<-as.numeric(atestp21)

mse(atestp21,pred.rf_chicken1)
mae(atestp21,pred.rf_chicken1)
mape(atestp21,pred.rf_chicken1)

pred.rf_pig<-predict(rf_pig,atestp3[,-1])
pred.rf_pig1<-as.numeric(pred.rf_pig)
atestp31<-unlist(atestp3)
atestp31<-as.numeric(atestp31)

mse(atestp31,pred.rf_pig1)
mae(atestp31,pred.rf_pig1)
mape(atestp31,pred.rf_pig1)

pred.rf_cow<-predict(rf_cow,atestp4[,-1])
pred.rf_cow1<-as.numeric(pred.rf_cow)
atestp41<-unlist(atestp4)
atestp41<-as.numeric(atestp41)

mse(atestp41,pred.rf_cow1)
mae(atestp41,pred.rf_cow1)
mape(atestp41,pred.rf_cow1)

#dt.model<-rpart(Count~.,method="poisson",data=atrainp2,control=rpart.control(maxdepth=5,minsplit=15))
#prp(dt.model,type=4)

#예측
library(NeuralNetTools)
library(nnet)
set.seed(1231)
nn_chicken<-nnet(Count~.,data=atrainp2,size=2,maxit=200,decay=5e-04)
summary(nn_chicken)
X11()
garson(nn_chicken)

pred.nn_chicken<-predict(nn_chicken,atestp2[,-1])
pred.nn_chicken1<-as.numeric(pred.nn_chicken)


mse(atestp21,pred.nn_chicken1)
mae(atestp21,pred.nn_chicken1)
mape(atestp21,pred.nn_chicken1)

set.seed(1231)
nn_pig<-nnet(Count~.,data=atrainp3,size=2,maxit=200,decay=5e-04)
summary(nn_pig)
X11()
garson(nn_pig)

pred.nn_pig<-predict(nn_chicken,atestp3[,-1])
pred.nn_pig1<-as.numeric(pred.nn_pig)


mse(atestp31,pred.nn_pig1)
mae(atestp31,pred.nn_pig1)
mape(atestp31,pred.nn_pig1)

set.seed(1231)
nn_cow<-nnet(Count~.,data=atrainp4,size=2,maxit=200,decay=5e-04)
summary(nn_cow)
X11()
garson(nn_cow)

pred.nn_cow<-predict(nn_cow,atestp4[,-1])
pred.nn_cow1<-as.numeric(pred.nn_cow)

mse(atestp41,pred.nn_cow1)
mae(atestp41,pred.nn_cow1)
mape(atestp41,pred.nn_cow1)