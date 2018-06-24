######Data transformation and cleaning
setwd("/R stuff/UCI HAR Dataset")


###Libraries
library(tidyr)
library(dplyr)



memory.limit(10 * 10^10)




###Import and compile dataset

#Set up train set
train <- read.table("train/X_train.txt", sep = "", header= FALSE)

labels_x <- read.table("train/Y_train.txt", sep="\t", header = FALSE)

subjectID <- read.table("train/subject_train.txt", sep ="\t", header = FALSE)

activity_labels <- read.table("activity_labels.txt", sep ="", header = FALSE)

features <- read.table("features.txt", sep=" ", header = FALSE)

colnames(labels_x) <- "labels"

colnames(subjectID) <- "subjectID"

colnames(train) <- features[1:561,2]


for (i in labels_x){
  labels_x$activity <- activity_labels[i,2]
}

labels_x$ID <- subjectID[,1]

train$ID <- subjectID[,1]

train$ID <- labels_x$ID

train$label <- labels_x$labels

train$activity <- labels_x$activity

train <- train[,c(564,563,562, 1 : 561)]

write.csv(train, file = "train_set.csv", row.names = FALSE)


#set up test set

test <- read.table("test/X_test.txt", sep = "", header = FALSE)

labels_y <- read.table("test/y_test.txt", sep = "", header = FALSE)

subjectID_y <- read.table("test/subject_test.txt", sep = "", header = FALSE)

colnames(test) <- features[1:561,2]

colnames(labels_y) <- "labels"

colnames(subjectID_y) <- "subjectID"

for (i in labels_y){
  labels_y$activity <- activity_labels[i,2]
}

labels_y$ID <- subjectID_y[,1]
test$ID <- subjectID_y[,1]

test$label <- labels_y$labels

test$activity <- labels_y$activity

test <- test[,c(564,563,562, 1 :561)]

write.csv(test, file = "test_set.csv", row.names = FALSE)
