require(caret); require(tidyverse); require(car); require(boot); require(class)
require(kernlab);  require(rpart); require(rpart.plot)
data <- read_csv("Higher_Ed.csv")
names(data)
attach(data)



# creating factor levels

data2 <- data %>%
  mutate(GENDER = factor(GENDER), MINRTY = factor(MINRTY), RACETH = factor(RACETH),
         DGRDG = factor(DGRDG), OCEDRLP = factor(OCEDRLP), NOCPR = factor(NOCPR))

# I used the data without factor levels 

#splitting the data
set.seed(123)
split <- createDataPartition(SALARY, p=.75, list = FALSE)
train <- data[split, ]; test <- data[-split,]; y_test <- data[-split, 7]
names(data)
# rpart
rpart.mod <- rpart(log(SALARY)~., data = train)
rpart.plot(rpart.mod)

# prediction for the training data
pred.tree.train <- predict(rpart.mod, data = train)
mse(pred.tree.train, log(train$SALARY)) #0.2270925

# predicting the test data
pred.tree <- predict(rpart.mod, test)
mse(pred.tree, log(test$SALARY)) # 0.2313029

# glm
attach(data)
set.seed(189)
mod.linear <- glm(log(SALARY)~., data = train)

# predicting the train data
pred.linear.train <- predict(mod.linear, train)
mse(pred.linear.train, log(train$SALARY)) #0.2367072

# checking the summary
summary(mod.linear)
vif(mod.linear) # see if multicolinearity exist in the model

#prediction for linear model with the test set
pred.linear.test <- predict(mod.linear, test)
mse(pred.linear.test, log(test$SALARY)) #0.2424846
rmse(pred.linear.test, log(test$SALARY)) #  0.4924272
rmsle(pred.linear.test, log(test$SALARY)) # 0.04177548



# randomforest
require(randomForest)
require(ModelMetrics)
set.seed(12345)

rf.mod <- randomForest(log(SALARY)~., data = train, ntree = 100, importance = TRUE)
rf.mod
plot(rf.mod)
summary(rf.mod)

# predicting the train data
pred.rf.train <- predict(rf.mod, train)
mse(pred.rf.train, log(train$SALARY)) # 0.1050066
varImpPlot(rf.mod)


# predicting the test data
pred.rf.test <- predict(rf.mod, test)
mse(pred.rf.test, log(test$SALARY)) # 0.1780654
rmse(pred.rf.test, log(test$SALARY)) # 0.421978
rmsle(pred.rf.test, log(test$SALARY)) # 0.03586838


# Stochastic Gradient Boosting (gbm)
set.seed(078)
require(gbm)
gbm.mod <- gbm(log(SALARY)~., data = train, cv.folds = 5, n.trees = 5000,
               distribution = "gaussian")
gbm.mod
summary(gbm.mod)
gbm.perf(gbm.mod)


# predicting the train data
pred.gbm.train <- predict(gbm.mod, train)
mse(pred.gbm.train, log(train$SALARY)) # 0.1793619


# predicting the test data
pred.gbm.test <- predict(gbm.mod, test)
mse(pred.gbm.test, log(test$SALARY)) # 0.1830619


# Support Vector Regression (SVR)
set.seed(6485)
require(e1071)

svr.mod <- svm(log(SALARY)~., train, cost = 100, gamma = 0.00001)
summary(svr.mod)
plot(svr.mod)


# predicting the train data
pred.svr.train <- predict(svr.mod, train)
mse(pred.svr.train, log(train$SALARY)) # 0.2412739
rmse(pred.svr.train, log(train$SALARY)) # 0.4911964
rmsle(pred.svr.train, log(train$SALARY))


# predicting the test data
pred.svr.test <- predict(svr.mod, test)
mse(pred.svr.test, log(test$SALARY)) # 0.2473282 
rmse(pred.svr.test, log(test$SALARY)) #0.497321


# xgboost
set.seed(100)
require(xgboost)

# cross validation to get the best number of rounds
cv.xgb <- xgb.cv(data = as.matrix(train), label = log(train$SALARY), objective = "reg:linear",
                 nrounds = 500, nfold = 5, eta = 0.3, depth = 6)

(nround = which.min(cv.xgb$test_rmse_mean))

# building the model
xgb.mod <- xgboost(data = as.matrix(train),
                   label = log(train$SALARY),
                   nrounds = nround,
                   obejctive = "reg:linear",
                   eta = 0.3,
                   depth = 6)
summary(xgb.mod)


# predicting the train data
pred.xgb.train <- predict(xgb.mod, newdata = as.matrix(train))
mse(pred.xgb.train, log(train$SALARY)) # 1.579971e-08


# predicting the test data
pred.xgb.test <- predict(xgb.mod, newdata = as.matrix(test))
mse(pred.xgb.test, log(test$SALARY)) # 1.556355e-08
