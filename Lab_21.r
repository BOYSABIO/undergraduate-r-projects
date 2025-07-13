# Question 1
library(mlbench)
library(caret)
library(MLeval)

data("PimaIndiansDiabetes")
data = PimaIndiansDiabetes


# Question 2
data$diabetes = relevel(data$diabetes, ref="pos")


# Question 3
index = createDataPartition(data$diabetes, p = 0.7, list = F)
train = data[index,]
test = data[-index,]


# Question 4 (I had an issue when I set layer2 and 3 to 0)
NN_auc_tune1 = train(diabetes~., data = train, method = "mlpML",
        tuneGrid = expand.grid(layer1 = 10, layer2 = 0, layer3 = 0),
        trControl = trainControl(method = "cv", number = 10,
        classProbs = TRUE, summaryFunction = twoClassSummary, 
        savePredictions = TRUE))
print("NN_AUC: ")
print(NN_auc_tune1)

NN_auc_tune2 = train(diabetes~., data = train, method = "mlpML",
        tuneGrid = expand.grid(layer1 = 10, layer2 = 10, layer3 = 0),
        trControl = trainControl(method = "cv", number = 10,
        classProbs = TRUE, summaryFunction = twoClassSummary, 
        savePredictions = TRUE))
print("NN_AUC2: ")
print(NN_auc_tune2)




# Question 5
pred_NN_auc_tune1 = predict(NN_auc_tune1, newdata = test, type = "prob")

pred_NN_auc_tune2 = predict(NN_auc_tune2, newdata = test, type = "prob")

# ADA AND SVM CONFUSION MATRIX
cm1 = confusionMatrix(NN_auc_tune1, test$diabetes, mode = "everything")
print("Confusion Matrix (1)")
print(cm1)

cm_NN_auc1 = confusionMatrix(NN_auc_tune2, test$diabetes, mode = "everything")
print("Confusion Matrix (2)")
print(cm2)

X = evalm(NN_auc_tune1,  gnames = "tree")
X = evalm(NN_auc_tune2,  gnames = "tree")

