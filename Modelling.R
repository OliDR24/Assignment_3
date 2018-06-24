####Modelling

#libraries
library(caret)
library(fitdistrplus)
library(logspline)
library(monmlp)
library(e1071)
library(subspace)
library(ROCR)
library(mice)
library(outliers)
#set WD
setwd("/R stuff/UCI HAR Dataset")


#Load data
train <- read.delim("feature_train_0.5.csv", sep = ",", header = TRUE)
train <- train[-2]
test <- read.delim("test_set.csv", sep = ",", header = TRUE)
mlp_train <- as.matrix(train[-1])
mlp_train_label <- as.matrix(train[1])
test <- test[,-c(1,3)]
test_labels <- test$label
#Search for distribution of data

descdist(train$angle.Y.gravityMean., discrete = FALSE)
descdist(train$tGravityAcc.min...X, discrete = FALSE)
descdist(train$tGravityAcc.energy...X, discrete = FALSE)
descdist(train$angle.X.gravityMean., discrete = FALSE)
descdist(train$tBodyAccMag.energy.., discrete = FALSE)
descdist(train$angle.Z.gravityMean., discrete = FALSE)
features <- colnames(train[,-1])


#outlier detection

outlier(train)

#cap outliers



##Distance based 



#Clustering

#subspace clustering
CLIQUE(test, xi = 10, tau = 0.2)


#More complex models 

#Set up cross validation
control <- trainControl(method="repeatedcv", number=10, repeats=5, savePredictions = TRUE)

#multilayer perceptron
#mlp <- monmlp.fit(mlp_train, mlp_train_label , hidden1=3, n.ensemble=15, monotone = 1, bag = TRUE)
#mlp_result <- monmlp.predict(x= mlp_train, weights = mlp)
#plot(performance(prediction(mlp_result, mlp_train_label)))

#Naive bayes classifier
nb <- train(train[,-1], as.factor(train$label), method = "nb", trControl=control)
nb_result <- confusionMatrix(predict(nb,test[,-c(1)]), factor(test$label))

#Gradient boosting model
gbm <- train(train[,-1], as.factor(train$label), method = "gbm", trControl = control, preProcess = c("center", "scale"))
gbm_result <- confusionMatrix(predict(gbm, test[,-1]), factor(test_labels))
#gbm_table <- table(factor(gbm, levels=min(test):max(test), factor(test, levels=min(test):max(test))))

# Decision tree model
dtree <- train(train[,-1],as.factor(train$label), method = "rpart", trControl = control, preProcess = c("center", "scale"))
dtree_result <- confusionMatrix(predict(dtree, test[, -1]), factor(test$label))
#dtree_check <- table(predict(dtree, complete.cases(test[,features])), test$label) 
# random forest model
rf <- train(train[,features],as.factor(train$label), method = "rf", trControl = control, preProcess = c("center", "scale"))
rf_result <- confusionMatrix(predict(rf,test[,-1]),factor(test$label))

#print results

png("gbm.png", width = 640, height = 480)
plot(gbm, main="Gradient Boosting")
dev.off()

png("nb.png", width = 640, height = 480)
plot(nb, main="Naive Bayes Classification")
dev.off()

png("dtree.png", width = 640, height = 480)
plot(dtree, main="Decision Tree")
dev.off()

png("rf.png", width = 640, height = 480)
plot(rf, main = "Random forest")
dev.off()

#plot feature importance

extract_param = function(result){
  result_end = list()
  result = unlist(result[6])
  for (i in sequence(length(result))){
    result_end[i] = paste(gsub("bestTune.","",names(result)[i]), round(result[i],4), sep = ': ')
  }
  result_end = do.call(paste, c(as.list(result_end), sep = ", "))
  return(result_end)
}



# make a table with the training and testing results
results_all = resamples(list(NB = nb, GBM = gbm, DTREE = dtree ,RF=rf))

results_table = data.frame(Classifier = c("NB", "GBM", "DTREE", "RF"),Training_Accuracy = c(max(nb$results$Accuracy), max(gbm$results$Accuracy), max(dtree$results$Accuracy), max(rf$results$Accuracy)), Testing_Accuracy = c(nb_result$overall[1], gbm_result$overall[1],dtree_result$overall[1],rf_result$overall[1]),Accuracy_lower_95 = c(nb_result$overall[3], gbm_result$overall[3],nb_result$overall[3],rf_result$overall[3]),Accuracy_upper_95 = c(nb_result$overall[4], gbm_result$overall[4],dtree_result$overall[4],rf_result$overall[4]), Kappa = c(nb_result$overall[2], gbm_result$overall[2],dtree_result$overall[2],rf_result$overall[2]),Parameters = c(extract_param(nb), extract_param(gbm),extract_param(dtree),extract_param(rf)))
results_table[,-c(1,7)] = round(results_table[,-c(1,7)],2)

write.table(results_table,'results_table.csv', sep=',', row.names = FALSE)

# class results
results_classes = rbind(t(dtree_result$byClass),t(gbm_result$byClass),t(nb_result$byClass),t(rf_result$byClass))
write.table(results_classes, 'results_classes.csv', sep=',',row.names = TRUE)


