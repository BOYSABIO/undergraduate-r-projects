#install.packages('mlbench')
#install.packages('caret')

library(mlbench)
library(caret)

# Retreive Data
data("BostonHousing")
data = BostonHousing

print(summary(data))

# MEDV is TARGET VARIABLE

# Split Data
index = createDataPartition(data$medv, p = 0.7, list = F)
train = data[index,]
test = data[-index,]


# Linear Regression
lm = train(medv~., data = train, method = 'lm')
print("LINEAR REGRESSION MODEL:")
print(lm)

print("WEIGHTS / COEFFICIENTS OF LINEAR REGRESSION MODEL:")
print(lm$finalModel)
print("Holding all other characteristics constant, a 1 increase in CRIM would result in a 0.087080")
print("decrease in MEDV")

lmPred = predict(lm, newdata = test)
print("LINEAR REGRESSION PREDICTION (RMSE):")
print(postResample(lmPred, test$medv)) # Measure accuracy in prediction
print('#RMSE : root mean squared error, this is one popular measurement for the continuous case. Similar to when we')
print('measure errors in linear regression, here, we square the error (to prevent large errors to cancel each other),') 
print('take the average (so average error made per observation), then we take the squareroot of it to bring it back to normal') 
print('scale instead of square scale. Roughly, it tells us the average error that the model makes per observation.')


# Regression KNN (Everytime you use knn you MUST use preProcess)
knnFit = train(medv~., data = train, method = 'knn', preProcess = c("center", "scale"), tuneGrid = expand.grid(k = seq(1, 30, by = 5)))
print("TUNED KNN REGRESSION MODEL:")
print(knnFit)

knnPred = predict(knnFit, test)
print("KNN PREDICTION (RMSE):")
print(postResample(knnPred, test$medv))


# Regression Tree
regtree = train(medv~., data = train, method ="rpart", tuneGrid = expand.grid(cp = seq(0.1, 1, by = 0.1)))
print("TUNED REGRESSION TREE:")
print(regtree)

regtreePred = predict(regtree, test)
print("REGRESSION TREE PREDICTION:")
print(postResample(regtreePred, test$medv))

btree = train(medv~., data = train, method = 'bstTree')
print("BOOSTED TREE:")
print(btree)

btree_tune = train(medv~., data = train, method = 'bstTree', tuneGrid = expand.grid(maxdepth = c(1:3), mstop = seq(20, 100, by = 20), nu = 0.1))
print("TUNED BOOSTED TREE:")
print(btree_tune)
btreePred = predict(btree_tune, test)
print("BOOSTED TREE PREDICTION (RMSE):")
print(postResample(regtreePred, test$medv))

print("Best model could be KNN Regression with the lowest RMSE value")
