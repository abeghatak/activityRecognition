load("~/samsungData.rda")
library(VIM)
aggr(samsungData, prop=FALSE, numbers=TRUE)

# activity as factor
samsungData$activity <- as.factor(samsungData$activity)

# Assign unique column names
make.names(colnames(samsungData), unique = TRUE)

samsungData<-data.frame(samsungData)

#Split the data
set.seed(20130228)
trainConstants = c(1,3,5,6)
testConstants = c(27, 28, 29, 30)
remainderSubjects<-setdiff(setdiff(unique(samsungData$subject),trainConstants),testConstants)
trainConstants<-c(trainConstants,setdiff(remainderSubjects, sample(remainderSubjects,length(remainderSubjects)/2)))
testConstants<-c(testConstants,setdiff(remainderSubjects,trainConstants))

trainData<-samsungData[samsungData$subject%in%trainConstants,]
testData<-samsungData[samsungData$subject%in%testConstants,]

library(tree)
library(rpart)

# using rpart
train.tree<-rpart(activity ~ ., data=trainData)
library(rattle)
fancyRpartPlot(train.tree)

#predict testdata activity using train.tree
pred<-predict(train.tree, newdata=testData,type="class")
sum(pred == testData$activity)/nrow(testData) 

#Cross Validation Results
plotcp(train.tree)

#using classification tree
traintree<-tree(activity ~ ., data=trainData)

#predict testdata activity using traintree
pred<-predict(traintree, newdata=testData,type="class")
sum(pred == testData$activity)/nrow(testData) 

# 10 fold Cross Validation
treefit.cv<-cv.tree(traintree)
opt.trees<-which(treefit.cv$dev == min(treefit.cv$dev))
best.leaves<-min(treefit.cv$size[opt.trees])
treefit.pruned<-prune.tree(traintree,best=best.leaves)

#Plots size vs. error
plot(treefit.cv)

##linear model
lm1<-lm(as.numeric(activity) ~ fBodyAccJerk.std...X + tGravityAcc.min...X + tGravityAcc.max...Y + tBodyAccMag.std.. + tGravityAcc.arCoeff...Y.2 + fBodyAccJerk.maxInds.X + tBodyGyro.arCoeff...Y.1, data=trainData)
summary(lm1)

#Misclassification Error
missClass<-function(values, predictions) 
          {
abs(sum(values - round(predictions))) / length(values)
           }
  
missClass(as.numeric(trainData$activity), predict(lm1, type="response"))