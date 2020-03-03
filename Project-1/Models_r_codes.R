require(caret);require(tidyverse);require(car);require(boot);require(class)
require(kernlab);require(rpart);require(rpart.plot);require(MLmetrics)
require(rcompanion);require(corrplot);require(Metrics);library(sjPlot)
require(randomForest);require(ModelMetrics);require(gbm);require(e1071)
require(Ckmeans.1d.dp);require(snakecase)


# loading the Higher Eduaction data set
data <- read_csv("Higher_Ed.csv")
names(data)
attach(data)



# creating factor levels

factored_data <- data %>%
  mutate(GENDER = factor(GENDER), MINRTY = factor(MINRTY), RACETH = factor(RACETH),
         DGRDG = factor(DGRDG), OCEDRLP = factor(OCEDRLP), NOCPR = factor(NOCPR),
         NOCPRMG = factor(NOCPRMG))



#splitting the data
set.seed(9955) #(123)
split <- createDataPartition(SALARY, p=.75, list = FALSE)
train <- data[split, ]; test <- data[-split,]; y_test <- data[-split, 7]
dim(data)

# plotting correlation plot
cor_data <- cor(data)
corrplot(cor_data)

# glm
attach(data)
set.seed(189)

# Building a linear model with 'raw data'
mod.linear <- glm(log(SALARY)~., data = train)

# without multicollinearity
mod.linear.partial <- glm(log(SALARY)~WEIGHT+AGE+GENDER+MINRTY+DGRDG+OCEDRLP
                          +NOCPRMG, data = train)

# predicting the train data
pred.linear.train <- predict(mod.linear, train)
mse(pred.linear.train, log(train$SALARY)) #0.2379765


# predicting 'raw' data without multicollinearity
pred.partial.train <- predict(mod.linear.partial, train)
mse(pred.partial.train, log(train$SALARY)) # 0.2457409

# checking the summary
summary(mod.linear)
vif(mod.linear) # see if multicolinearity exist in the model

# prediction for linear model with the test set
pred.linear.test <- predict(mod.linear, test)
mse(pred.linear.test, log(test$SALARY)) #0.2424846
rmse(pred.linear.test, log(test$SALARY)) #  0.4924272
rmsle(pred.linear.test, log(test$SALARY)) # 0.04177548

# prediction without multicolinearity
pred.partial.test <- predict(mod.linear.partial, test)
mse(pred.partial.test, log(test$SALARY)) #0.2467384


# it seems that multicollinearity doesn't affect the generalization 
# power of the model. Actually, without those variables the model
# seems to lose some accuracy power



# let us try to use a factorized data and see the performance of the linear models



# splitting the factored data
idx <- createDataPartition(factored_data$SALARY, p=.75, list = FALSE)
fc_train <- factored_data[idx,]
fc_test <- factored_data[-idx,]

# linear model with multicolinearity
set.seed(2020)
fc.linear <- glm(log(SALARY)~., data = fc_train)
pred.fc <- predict(fc.linear, fc_train)
mse(pred.fc, log(fc_train$SALARY)) # 0.1927259


# prediction with test set
pred.fc.test <- predict(fc.linear, fc_test)
mse(pred.fc.test, log(fc_test$SALARY)) # 0.1954082


# linear models without multicolinearity
fc.linear.partial <- glm(log(SALARY)~WEIGHT+AGE+GENDER+MINRTY+DGRDG+OCEDRLP
    +NOCPRMG, data = fc_train)

summary(fc.linear.partial)

pred.fc.partial <- predict(fc.linear.partial, fc_train)
mse(pred.fc.partial, log(fc_train$SALARY)) #0.2290617


# prediction with test set
pred.fc.p_test <- predict(fc.linear.partial, fc_test)
mse(pred.fc.p_test, log(fc_test$SALARY)) # 0.2298312

# we can also use anova to compare the two models
anova(fc.linear, fc.linear.partial)

#Resid. Df Resid. Dev  Df Deviance
#1     71608      13808             
#2     71630      16411 -22  -2603.3

# plotting linear model variable importance

plot_model(fc.linear.partial)

# So the linear model generalizes better with multicolinearity
# which means multicolinearity only damages the coefficients not the overall model

# let us look at the variable importance of the winning model




### Tree-based models
# Decision Tree: rpart
set.seed(2341)
rpart.mod <- rpart(log(SALARY)~., data = train, method = "anova")
rpart.plot(rpart.mod, yesno = 2, type = 0, extra = 0)
plotcp(rpart.mod)
print(rpart.mod$cptable)

# Retrieve optimal cp value based on cross-validated error
(cp.idx <- which.min(rpart.mod$cptable[,'xerror']))
(cp.opt <- rpart.mod$cptable[cp.idx, "CP"])

# Pruning the model (to optimized cp value)
rpart.mod.opt <- prune(tree = rpart.mod, 
                         cp = cp.opt)

# Plot the optimized model
rpart.plot(x = rpart.mod.opt, yesno = 2, type = 0, extra = 0)




# prediction for the training data
pred.tree.train <- predict(rpart.mod.opt, data = train, method = "anova")
mse(pred.tree.train, log(train$SALARY)) # 0.2288648

# predicting the test data
pred.tree.test <- predict(rpart.mod.opt, test)
mse(pred.tree.test, log(test$SALARY)) # 0.2259905





# randomforest
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
set.seed(9956) #(100)

# cross validation to get the best number of rounds
cv.xgb <- xgb.cv(data = as.matrix(train[, -11]), label = log(train$SALARY),
                 objective = "reg:linear",
                 nrounds = 200, nfold = 5, eta = 0.3, depth = 6)

(nround = which.min(cv.xgb$evaluation_log$test_rmse_mean))

# building the model
xgb.mod <- xgboost(data = as.matrix(train[,-11]),
                   label = log(train$SALARY),
                   nrounds = 35,
                   obejctive = "reg:linear",
                   eta = 0.3,
                   depth = 6)
summary(xgb.mod)


# predicting the train data
pred.xgb.train <- predict(xgb.mod, newdata = as.matrix(train[,-11]))
mse(pred.xgb.train, log(train$SALARY)) # 1.579971e-08 # 0.1499239


# predicting the test data
pred.xgb.test <- predict(xgb.mod, newdata = as.matrix(test[,-11]))
mse(pred.xgb.test, log(test$SALARY)) # require(Ckmeans.1d.dp)

# plotting variable importance 
xgb.ggplot.importance(xgb.importance(model = xgb.mod))


