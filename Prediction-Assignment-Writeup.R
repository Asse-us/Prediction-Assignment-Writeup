

# Loading and preprocessing the data   
# Necessary packages
library(knitr)
library(caret)  
library(MASS)
library(klaR)
library(rattle)
library(readr) 
library(ggplot2)

# DATA DOWNLOADING AND READING
filePath<- getwd()
fileName1<- "pml-training.csv"
fileName2<- "pml-testing.csv"
urll<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(urll, destfile = fileName1, method = "curl")
download.file(url2, destfile = fileName2, method = "curl")

trainingRD<- read.csv(fileName1)
testingRD<- read.csv(fileName2)
dim(trainingRD)
dim(testingRD)


View(trainingRD)
View(testingRD)


# CLEANING THE DATA
nearZero<- nearZeroVar(trainingRD)
trainingRD <- trainingRD[ ,-nearZero]
trainingRD<- trainingRD[ ,which(colSums(is.na(trainingRD))== 0)]

# the first 7 columns are variables that has no relationship with "classe"
trainingSet<- trainingRD[ ,-c(1:7)]
testing<- testingRD[ ,-c(1:7)]

# The number of observations for each class category after the data is cleaned
table(trainingSet$classe)

# CREATING TAINING, TEST AND VALIDATION DATA SETS
inTrain<- createDataPartition(y = trainingSet$classe, p = 0.7, list = FALSE)
training<- trainingSet[inTrain, ]
validation<- trainingSet[-inTrain, ]
dim(training)
dim(validation)

# SAMPLE DATA PLOTS
par(mfrow = c(2, 2))
plot(training$classe, training$total_accel_belt, xlab = "Class", 
     ylab = "total_accel_belt", main = "Class vs Total acceleration on belt")
plot(training$classe, training$total_accel_arm, xlab = "Class", 
     ylab = "total_accel_arm", main = "Class vs Total acceleration on arm")
plot(training$classe, training$total_accel_dumbbell, xlab = "Class", 
     ylab = "total_accel_dumbbell", main = "Class vs Total acceleration on dumbbell")
plot(training$classe, training$total_accel_forearm, xlab = "Class", 
     ylab = "total_accel_forearm", main = "Class vs Total acceleration on forearm")

# BEST FIT MODEL SELECTION(lda, rpart and rf)
# 1. Linear discriminant analysis ("lda")
mod_lda<- train(classe ~., data = training, method = "lda")
plda <- predict(mod_lda, validation)
confusionMatrix(plda, validation$classe)

# 2. Recursive Partitioning ("rpart") and plot Trees
mod_rpart<- train(classe ~., data = training, method = "rpart")
prpart<- predict(mod_rpart, validation)
confusionMatrix(prpart, validation$classe)
fancyRpartPlot(mod_rpart$finalModel)

# 3. Random forest analysis("rf")
mod_rf<- train(classe ~., method = "rf", data = training,  importance = T, 
               trControl = trainControl(method = "cv", classProbs=TRUE,
                                        savePredictions=TRUE,allowParallel=TRUE, 
                                        number =3))
prf<- predict(mod_rf, validation)
confusionMatrix(prf, validation$classe)

# PREDICTIONS WITH TEST DATA
testing_pre<- predict(mod_rf, newdata = testing)
testing_pre


