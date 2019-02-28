traindata<-read.csv(file = "train.csv",header = T,sep = ",")
testdata<-read.csv(file = "test.csv",header = T,sep = ",")

summary(traindata)

str(traindata)
is.na(traindata)
sum(is.na(traindata))
library(DMwR)
traindata<-centralImputation(traindata)
sum(is.na(traindata))


summary(testdata)
str(testdata)
is.na(testdata)
sum(is.na(testdata))
library(DMwR)
testdata<-centralImputation(testdata)
sum(is.na(testdata))
testdata1<-testdata[,-c(1,3,8,10)]
str(testdata1)

traindata1<-traindata[,-c(1,4,9,11)]

set.seed(123)
library(caTools)
split<-sample.split(traindata1,SplitRatio = 0.80)
split
train<-subset(traindata1,split=="TRUE")
valid<-subset(traindata1,split=="FALSE")

library(caret)
dummify = dummyVars(Survived~.,data = traindata1)
x.train = predict(dummify,traindata1)
y.train = traindata1$Survived
x.validate = predict(dummify,valid)
y.validate = valid$Survived
model1<-glm(Survived~. , data = train, family = "binomial")
summary(model1)


library(MASS)
step1<-stepAIC(model1,direction="both")
summary(step1)

library(car)
vif(model1)


prob_train<-predict(model1,train,type="response")

preds_train <- ifelse(prob_train > 0.5, "1", "0")
preds_train<-as.factor(preds_train)

prob_test<-predict(model1,valid,type="response")
preds_valid<- ifelse(prob_test>0.5,"1","0")
preds_valid<-as.factor(preds_valid)


library(caret)
confusion_matrix_train<-table(preds_train,train$Survived)
colnames(confusion_matrix_train) = c("predicted Surrvival","Predicted Not Surrvived")
rownames(confusion_matrix_train) = c("Actual Survival","Actual Not Survived")
confusion_matrix_train

confusion_matrix_valid<-table(preds_valid,valid$Survived)
confusion_matrix_valid
accuracy_train<-sum(diag(confusion_matrix_train))/sum(confusion_matrix_train)
accuracy_train
accuracy_valid<-sum(diag(confusion_matrix_valid))/sum(confusion_matrix_valid)
accuracy_valid

prob_test1 = predict(model1, testdata1, type = "response")
testdata1$predicted =ifelse(prob_test1 > 0.5, "1", "0")
testdata1$predicted
sample1<-data.frame(testdata)
sample1$Survived=testdata1$predicted
View(sample1)
sample1=sample1[,-c(2,11)]
sample1
write.csv(sample1,"harshavardhan_sample.csv",row.names = FALSE)
