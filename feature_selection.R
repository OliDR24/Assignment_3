####Feature selection
setwd("/R stuff/UCI HAR Dataset")


###libraries
library(mlbench)
library(caret)
library(randomForest)


#load datasets

train <- read.table("train_set.csv", sep =",", header = TRUE)


train$label <- factor(train$label)

####Feature selection by correlation

#generate correlation matrix

correlationMatrix <- cor(train[,4:564])

#print correlation matrix
print(correlationMatrix)

#Search for attributes that are highly correclated
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5, names = FALSE, exact = TRUE)

#highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75, names=FALSE, exact = TRUE)
#display output 
print(highlyCorrelated)

train_removal <- train[,c(-1,-highlyCorrelated)]

write.csv(train_removal, file = "feature_train_0.5.csv", row.names = FALSE)

####Ranking feature by importance

#prepare training scheme
control <- trainControl(method = "repeatedcv", number=10, repeats = 5)

#train model
model <- train(factor(label)~., data=train_removal, method="lvq", preProcess="scale", trControl=control)

#estimate variable importance
importance <- varImp(model, scale=TRUE)

print(importance)

plot(importance)


#####Automatic feature selection


##Random forest feature selection
control <- rfeControl(functions=rfFuncs, method="repeatedcv", number = 10, repeats = 5)

#Run RFE

results <- rfe(train_removal[,2:85], train_removal[,1], sizes=c(1:85), rfeControl=control)

##Summarize
print(results)

#Chosen features
predictors(results)

#plot results

plot(results, type = c("g", "o"))


###Random forest with party library

##initiate model 
fit <- randomForest(factor(label)~., data=train_removal)

(VI_F <- importance(fit))

varImp(fit)

varImpPlot(fit, type=2)


#combination


