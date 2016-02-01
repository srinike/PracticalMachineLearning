library(rpart)
library(caret)
library(rattle)

# =========================
# PREPROCESSING OF THE DATA
# =========================

# Load the training and testing data
trainingData = read.csv("pml-training.csv")
testingData = read.csv("pml-testing.csv")

#select accelerometer data from the training data
#names(trainingData)
accelTrainingData = trainingData[,grep("accel", names(trainingData), value=TRUE)]

#Remove the four columns since there are not enough data and since most of the data observation is empty
columnsToRemove = c("var_total_accel_belt", "var_accel_arm", "var_accel_dumbbell", "var_accel_forearm") 
accelTrainingData = accelTrainingData[ , -which(names(accelTrainingData) %in% columnsToRemove)]
accelTrainingData = cbind(accelTrainingData, trainingData[,c("classe")])

# Split the training data in two such that one set is used as the training set and other set is 
# used as the testing data
inTrain = createDataPartition(y=accelTrainingData$classe, p=0.8, list = FALSE)
newTrainingData = accelTrainingData[inTrain,]
newTestingData = accelTrainingData[-inTrain,]

accelTestingData = testingData[,grep("accel", names(testingData), value=TRUE)]
accelTestingData = accelTestingData[ , -which(names(accelTestingData) %in% columnsToRemove)]

#Rename the column to 'classe'
colnames(accelTrainingData)[17] <- "classe"

# =========================
# DESCRIPTIVE ANALYSIS
# =========================

# Plot the overall Classe variable 
barplot(table(trainingData$classe), main="Class Variable", names= c("A", "B", "C", "D", "E"))

# Plot the Classe variable by different variables

# ======================================
# BUILD THE TREE BASED PREDICTION MODEL 
# ======================================

# Use the new training data created from the training data to build prediction tree
classeTree = rpart(classe ~ ., data = newTrainingData)
printcp(classeTree)

# Plot the cost complexity parameters
plotcp(classeTree)
# Using the plot, it is possible to determinie that the minimum cross validation occurs at 
# when the tree size is at 14. The dotted line is the upper limit of the standard deviation.

# plot the visual tree
#plot(classeTree, margin= 0.1)
#text(classeTree, all=TRUE, use.n = TRUE)

# ===================================================
# PREDICT THE OUTCOME OF THE TESTDATE USING THE MODEL 
# ===================================================
predictTestingData = predict(classeTree, newTestingData, type="class")

# Compare the prediction with the actual data
#table(newTestingData$classe, predictTestingData)

# Display the confusion matrix
confusionMatrix(table(predictTestingData, newTestingData$classe))

# The accuracy of the mode is estimated to be at 52%.

# ===================================================
# PERFORM CROSS VALIDATION
# ===================================================

controlRef = trainControl(method="repeatedcv", number=10, repeats=3)
model = train(classe~., data=accelTrainingData, method="rpart", preProcess="scale", trControl=controlRef)
model


