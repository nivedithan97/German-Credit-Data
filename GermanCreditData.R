#Name: Niveditha Nagasubramanian

rm(list=ls())
options(digits=4)
install.packages("tree")
library(tree)
install.packages("e1071")
library(e1071)
install.packages(("ROCR"))
library(ROCR)
install.packages("randomForest")
library(randomForest)
install.packages("adabag")
library(adabag)
install.packages("rpart")
library(rpart)

setwd("C:/local/Niveditha Nagasubramanian/DATA ANALYTICS/ASSIGNMENT 2")
GCD= read.csv("GCD2018.csv")
GCD=GCD[complete.cases(GCD),] #remove rows that contains NA
GCD$Class=factor(GCD$Class)

#calculate the proportion of Good credit risk (1) to Bad credit risk (2)
table(GCD$Class)/length(GCD$Class)
#The proportion of good credit risk is 0.6995 and the proportion of bad credit risk is 0.3005

summary(GCD)
attach(GCD)
GCD_goodcredit=GCD[which(Class==1),] #Good credit Risk data
GCD_badcredit=GCD[which(Class==2),] #Bad credit Risk data
summary(GCD_goodcredit)
apply(GCD_goodcredit,2,sd,na.rm=TRUE)
summary(GCD_badcredit)
apply(GCD_badcredit,2,sd,na.rm=TRUE)
#assuming that duration is the no. of months a person takes to pay back
#one of the factor for bad credit risk is long duration
#noteworthy
boxplot(GCD$Duration~GCD$Employment, xlab="Employment", ylab="Duration") 
#this is interesting becos a person who is employed between 4 to 7 yrs take longer to pay back when they should be the one paying back quickly

detach(GCD)



attach(GCD)
set.seed(28037758) #random seed
train.row=sample(1:nrow(GCD),0.7*nrow(GCD))
GCD.train=GCD[train.row,] #70% training data
GCD.test=GCD[-train.row,] #30% test data



#calculate decision tree
GCD.tree=tree(Class ~. ,data=GCD.train)
print(summary(GCD.tree))
plot(GCD.tree)
text(GCD.tree,pretty=0)

#do predictions as classes and draw a table
GCD.predtree= predict(GCD.tree, GCD.test, type = "class")
t1=table(Predicted_Class = GCD.predtree, Actual_Class = GCD.test$Class) 
cat("\n#Decision Tree Confusion \n")
print(t1)
#report accuracy i.e (TP+TN)/(TP+TN+FP+FN)
#(155+30)/(155+30+11+48)= 0.7582 = 75.82%


#calculate naive bayes
GCD.bayes = naiveBayes(Class ~. , data = GCD.train) 
GCD.predbayes = predict(GCD.bayes, GCD.test) 
t2=table(Predicted_Class = GCD.predbayes, Actual_Class = GCD.test$Class) 
cat("\n#Naive Bayes Confusion\n")
print(t2)
#report accuracy 
#(146+46)/(146+46+32+20) = 0.7869 = 78.69%

#calculate bagging
GCD.bag=bagging(Class~.,data=GCD.train,mfinal=5)
GCD.predbag=predict.bagging(GCD.bag,GCD.test)
cat("\nBagging Confusion\n")
print(GCD.predbag$confusion)
#report accuracy
#(154+29)/(154+29+49+12) = 0.75 = 75%

#calculate boosting
GCD.boost=boosting(Class~.,data=GCD.train,mfinal=10)
GCDpred.boost = predict.boosting(GCD.boost,newdata=GCD.test)
cat("\nBoosting Confusion\n")
print(GCDpred.boost$confusion)
#report accuracy
#(149+42)/(149+42+36+17) = 0.7828 = 78.28%

#calculate Random Forest
GCD.randomF=randomForest(Class~.,data=GCD.train,na.action=na.exclude)
GCDpredRF=predict(GCD.randomF,GCD.test)
t3=table(Predicted_Class=GCDpredRF,Actual_Class=GCD.test$Class)
cat("\nRandom Forest Confusion\n")
print(t3)
#report accuracy
#(158+36)/(158+36+42+8) = 0.7951 = 79.51%



#Decision Tree
#do predictions as probabilities and draw ROC
GCD.pred.tree=predict(GCD.tree,GCD.test,type="vector")
GCD.pred.tree #confidence interval
#computing a simple ROC curve (x-axis: false positive rate, y-axis: true positive rate)
GCDpred=prediction(GCD.pred.tree[,2],GCD.test$Class)
GCDperf=performance(GCDpred,"tpr","fpr")
plot(GCDperf,col="red")
abline(0,1)
GCDauc = performance(GCDpred,"auc") #calculate AUC for DT
GCD_auc=as.numeric(GCDauc@y.values)
#The AUC for DT is 0.7313 

#Naive Bayes
GCDpred.bayes=predict(GCD.bayes,GCD.test,type='raw')
GCDpred.bayes
GCBpred=prediction(GCDpred.bayes[,2],GCD.test$Class)
GCBperf=performance(GCBpred,"tpr","fpr")
plot(GCBperf,add=TRUE,col="darkblue")
GCBauc=performance(GCBpred,"auc") #calculate AUC for Naive Bayes
GCB_auc=as.numeric(GCBauc@y.values)
#The AUC for Naive Bayes is 0.8135

#Bagging
GCDBagpred = prediction(GCD.predbag$prob[,2],GCD.test$Class)
GCDBagperf = performance(GCDBagpred,"tpr","fpr")
plot(GCDBagperf,add=TRUE,col="purple")
GCBagauc=performance(GCDBagpred,"auc") #calculate AUC for Bagging
GCBag_auc=as.numeric(GCBagauc@y.values)
#The AUC for Bagging is 0.7976 

#Boosting
GCDBoostpred=prediction(GCDpred.boost$prob[,2],GCD.test$Class)
GCDBoostperf=performance(GCDBoostpred,"tpr","fpr")
plot(GCDBoostperf,add=TRUE,col="brown")
GCBoostauc=performance(GCDBoostpred,"auc") #calculate AUC for Boosting
GCBoost_auc=as.numeric(GCBoostauc@y.values)
#The AUC for Boosting is 0.7992 

#Random Forest
GCDpred.RF=predict(GCD.randomF,GCD.test,type="prob")
GCDRFpred=prediction(GCDpred.RF[,2],GCD.test$Class)
GCDRFperf=performance(GCDRFpred,"tpr","fpr")
plot(GCDRFperf,add=TRUE,col="darkgreen")
GCRFauc=performance(GCDRFpred,"auc") #calculate AUC for Random Forest
GCRF_auc=as.numeric(GCRFauc@y.values)
#The AUC for Random Forest is 0.8323

#adding legend to the plot
legend("bottomright", legend=c("Random","Decision Tree", "Naive Bayes","Bagging","Boosting","Random Forest"),col=c("black","red", "Darkblue","purple","brown","darkgreen"), lty=1:2, cex=0.55,title="Legend:",text.font=30)



#Random forest is the best because it has high Accuracy and high AUC



#DT Attribute Importance
cat("\n Decision Tree Attribute Importance\n")
print(summary(GCD.tree))
#impt attributes: Status, Purpose, Age, Other, History, Duration, Savings

#Bagging Attribute Importance
cat("\nBagging Attribute Importance")
print(GCD.bag$importance)
#impt attributes: Status, Purpose, Duration

#Boosting Attribute Importance
cat("\nBoosting Attribute Importance")
print(GCD.boost$importance)
#impt attributes: Credit, Purpose, Status

#Random Forest Attribute Importance 
cat("\nRandom Forest Attribute Importance\n")
print(GCD.randomF$importance)
#impt attributes: Status, Duration, History, Purpose, Credit, Savings, Employment,Property, Age

#Importance attribute for Naive Bayes doesn't apply here because all the variables here are given equal importance





#pruning the decision tree

#resampling with replacement
GCDPrune_train= GCD[sample(nrow(GCD),100,replace=TRUE),]
#fitting the model
GCDPrune_fit=tree(Class~.,data=GCDPrune_train)
summary(GCDPrune_fit)
plot(GCDPrune_fit)
text(GCDPrune_fit,pretty=0)

#do predictions as classes and draw a table
GCD.prunepredtree= predict(GCDPrune_fit, GCD.test, type = "class")
t5=table(Predicted_Class = GCD.prunepredtree, Actual_Class = GCD.test$Class) 
cat("\n#Pruned Decision Tree w/o CV Confusion \n")
print(t5)
#report accuracy i.e (TP+TN)/(TP+TN+FP+FN)
#(129+46)/(129+46+32+37)= 0.7172  = 71.72%
#the accuracy is worst than what I expected. So now I am going to do CV

#cross validation
testpruneGCD=cv.tree(GCDPrune_fit,FUN=prune.misclass)
testpruneGCD #best is 5 leaves with sd 23
newprune_GCDfit=prune.misclass(GCDPrune_fit,best=5)
summary(newprune_GCDfit)
plot(newprune_GCDfit)
text(newprune_GCDfit,pretty=0)

GCD.cvtree=predict(newprune_GCDfit,GCD.test,type="class")
t6=table(Predicted_Class = GCD.cvtree, Actual_Class = GCD.test$Class)
cat("\n#Pruned Decision Tree with CV Confusion \n")
print(t6)
#report accuracy
#(119+54)/(119+54+24+47) = 0.7090 = 70.90% 

#drawing ROC
GCDprune_cvpred=predict(newprune_GCDfit,GCD.test,type="vector")
GCDprune_cv=prediction(GCDprune_cvpred[,2],GCD.test$Class)
GCDprune_cvperf=performance(GCDprune_cv,"tpr","fpr")
plot(GCDprune_cvperf,col="red")
abline(0,1)
GCDpruneauc = performance(GCDprune_cv,"auc") #calculate AUC for DT pruning CV
GCDprune_auc=as.numeric(GCDpruneauc@y.values)
#The AUC for DT pruning CV is 0.7510


#adding legend to the plot
legend("bottomright", legend=c("Random","Decision Tree"),col=c("black","red"), lty=1:2, cex=0.55,title="Legend:",text.font=30)



library(neuralnet)
setwd("C:/local/Niveditha Nagasubramanian/DATA ANALYTICS/ASSIGNMENT 2")
GCDD= read.csv("GCD2018.csv")
GCDD=GCDD[complete.cases(GCDD),] #remove rows that contains NA
#GCDD$Class=factor(GCDD$Class)
ind=sample(2,nrow(GCDD),replace=TRUE,prob=c(0.7,0.3))
GCDDtrain=GCDD[ind==1,]
GCDDtest=GCDD[!ind==1,]
attach(GCDD)
#impt attributes: Duration, Age, Credit
GCDD.nn=neuralnet(Class~ Duration + Credit + Age ,GCDDtrain,hidden=3)
plot(GCDD.nn)
GCDD.pred=compute(GCDD.nn, GCDDtest[c(2,5,13)])
GCDD.pred=as.data.frame(round(GCDD.pred$net.result,0))
cat("\n#ANN Confusion\n")
table(observed=GCDDtest$Class,predicted=GCDD.pred$V1)
#accuracy=(157+0)/(157+0+70+1) = 0.6886


