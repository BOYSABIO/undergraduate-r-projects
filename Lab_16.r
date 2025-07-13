library(mlbench)
library(caret)
install.packages('MLeval')
library(MLeval)

# Retrieve Data
data("PimaIndiansDiabetes")
data = PimaIndiansDiabetes

# We particularly want to predict the positive rather than the negatives
summary = summary(data$diabetes)
print('Summary: ')
print(summary)

data$diabetes = relevel(data$diabetes, ref = "pos") #This function allows us to select the reference level


#Split data into train and test
index = createDataPartition(data$diabetes, p = 0.7, list = F)
train = data[index,]
test = data[-index,]


# TUNING
tree = train(diabetes~., data = train, method = "rpart", 
        tuneGrid = expand.grid(cp = seq(0.1, 1, by = 0.1)),
        trControl = trainControl(method = "cv", number = 10))

print("Cross-Validation Accuracy:")
print(tree) #Accuracy coming from cross-validation
print("Best cp = 0.2")

# If you are not happy with accuracy you can use AUC & ROC
tree_auc = train(diabetes~., data = train, method = "rpart",
        tuneGrid = expand.grid(cp = seq(0.1, 1, by = 0.1)),
        trControl = trainControl(method = "cv", number = 10,
        classProbs = TRUE, summaryFunction = twoClassSummary, 
        savePredictions = TRUE))
print("Retrained RPART using ROC:")
print(tree_auc)

X = evalm(tree_auc,  gnames = "tree")
        


# Test Set Evaluation
pred_tree_class = predict(tree_auc, newdata = test) #Predict class
pred_tree_prob = predict(tree_auc, newdata = test, type = "prob") #Predict probability

# CONFUSION MATRIX
cm = confusionMatrix(pred_tree_class, test$diabetes)
print("Confusion Matrix:")
print(cm)

cm2 = confusionMatrix(pred_tree_class, test$diabetes, mode = "everything")
print("Confusion Matrix (with F1 Score)")
print(cm2)

# ROC curves on test set
pred_tree_prob$obs = test$diabetes #Creating new variable called obs (observations)
X = evalm(pred_tree_prob)

#Lift Chart
lift = lift(test$diabetes~pred_tree_prob$pos)
ggplot(lift, values = 60) #If you want to predict 60%
print('Lift:')
print(lift)


# REPEAT PROCESS WITH ADA AND SVMLINEAR
tree_ada_tune = train(diabetes~., data = train, method = "ada", 
                tuneGrid = expand.grid(nu = 0.1, maxdepth = c(1:4), 
                iter = c(50, 75, 100)), trControl = trainControl(method = "cv", 
                number = 10, classProbs = TRUE, summaryFunction = twoClassSummary, 
                savePredictions = TRUE))
print("TUNE ADA 10 FOLD: ")
print(tree_ada_tune)

svm_tune = train(diabetes~., data = train, method = "svmLinear", 
                tuneGrid = expand.grid(seq(0.1, 1, by = 0.1)), 
                trControl = trainControl(method = "cv", number = 10,
                classProbs = TRUE, summaryFunction = twoClassSummary, 
                savePredictions = TRUE))
print("TUNE SVMLINEAR 10 FOLD: ")
print(svm_tune)

# ADA AND SVM PREDICTIONS (CLASSES AND PROBABILITY)
pred_ada_class = predict(tree_ada_tune, newdata = test)
pred_ada_prob = predict(tree_ada_tune, newdata = test, type = "prob")

pred_svm_class = predict(svm_tune, newdata = test)
pred_svm_prob = predict(svm_tune, newdata = test, type = "prob")

# ADA AND SVM CONFUSION MATRIX
cm_ada = confusionMatrix(tree_ada_tune, test$diabetes, mode = "everything")
print("Confusion Matrix (ada)")
print(cm_ada)

cm_svm = confusionMatrix(svm_tune, test$diabetes, mode = "everything")
print("Confusion Matrix (svm)")
print(cm_svm)

# ROC CURVES
pred_ada_prob$obs = test$diabetes
Y = evalm(pred_ada_prob)

pred_svm_prob$obs = test$diabetes
z = evalm(pred_svm_prob)

# LIFT CHART
lift = lift(test$diabetes~pred_tree_prob$pos + pred_ada_prob$pos + pred_svm_prob$pos)
ggplot(lift, values = 60)


# REGRESSION
data("BostonHousing")
data2 = BostonHousing

index = createDataPartition(data2$medv, p = 0.7, list = F)
train2 = data2[index,]
test2 = data2[-index,]

knnFit = train(medv~., data = train2, method = "knn", preProcess = c("center", "scale"),
        tuneGrid = expand.grid(k = c(1:20)),
        trControl = trainControl(method = "cv", number = 10))
print("Tuned KNN")
print(knnFit)

btree = train(medv~., data = train2, method = "bstTree", tuneGrid = expand.grid(nu = 0.1, maxdepth = c(1:5), mstop = 50), # nolint
        trControl = trainControl(method = "cv", number = 10)) #Only adding train control 
print("Tuned Boosted Regression Tree:")
print(print(btree))

pred_knn = predict(knnFit, test2)
pred_btree = predict(btree, test2)


print("KNN: RSME, RSQUARED, MAE")
print(postResample(pred_knn, test2$medv))
print("BOOST TREE: RSME, RSQUARED, MAE")
print(postResample(pred_btree, test2$medv))

print("Boosted tree is better fit with lower RSME and lower MAE")

