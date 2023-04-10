#setwd("C:/Users/andre/Documents/msba/machine_learning/theburgerboys")
###add ur wd however it is to get data in, I can't get in the test data

## or go to session in the ribbon: set WD: to source file location 


rm(list=ls())

set.seed(1693)

traindata <- read.csv("train.csv")

#data$Open.Date <- as.Date(Open.Date)



#14 to 18
# 24 to 27
#30 to 37
library(dplyr)

traindata <- select(traindata, -P14, -P15, -P16, -P17, -P18, -P24, -P25, -P26, -P27, -P30, -P31, -P32, -P33, -P34, -P35, -P36, -P37)


# extract the year from the character date
traindata$year <- substr(traindata$Open.Date, start = nchar(traindata$Open.Date) - 3, stop = nchar(traindata$Open.Date))

traindata$year <- as.numeric(traindata$year)
traindata$age <- 2015 - traindata$year

traindata <- select(traindata, -Open.Date)
traindata <- select(traindata, -year)
traindata <- select(traindata, -Id)

               
traindata$City <- as.factor(traindata$City)

traindata$City.Group <- as.factor(traindata$City.Group)

traindata$Type <- as.factor(traindata$Type)

traindata$P1 <- as.factor(traindata$P1)

traindata$P2 <- as.factor(traindata$P2)
traindata$P3 <- as.factor(traindata$P3)
traindata$P4 <- as.factor(traindata$P4)
traindata$P5 <- as.factor(traindata$P5)
traindata$P6 <- as.factor(traindata$P6)
traindata$P7 <- as.factor(traindata$P7)
traindata$P8 <- as.factor(traindata$P8)
traindata$P9 <- as.factor(traindata$P9)
traindata$P10 <- as.factor(traindata$P10)
traindata$P11 <- as.factor(traindata$P11)
traindata$P12 <- as.factor(traindata$P12)

traindata$P13 <- as.factor(traindata$P13)
traindata$P19 <- as.factor(traindata$P19)
traindata$P20 <- as.factor(traindata$P20)

traindata$P21 <- as.factor(traindata$P21)
traindata$P22 <- as.factor(traindata$P22)
traindata$P23 <- as.factor(traindata$P23)

traindata$P28 <- as.factor(traindata$P28)
traindata$P29 <- as.factor(traindata$P29)








### basic ridge and lasso
library(glmnet)
set.seed(1)

### we have NAs

#traindata[is.na(traindata)] <- 0

### we downloaded, just the training data, test data does not have revenue

x_train <- model.matrix(revenue ~ ., traindata)[, -1]

y_train <- traindata$revenue



grid = 10^seq(-2, 4,length=200)


ridge_mod <- glmnet(x_train, y_train, lambda = grid, alpha = 0)

cv.ridge <- cv.glmnet(x_train, y_train, lambda = grid, alpha = 0, nfolds = 12)

bestlam_ridge <- cv.ridge$lambda.min


ridge.pred <- predict(ridge_mod, 
                      s=bestlam_ridge, 
                      newx=x_train)
ridge_mse <- mean((ridge.pred-y_train)^2)



lasso.mod <- glmnet(x_train, y_train, lambda = grid, alpha = 1,)

cv.lasso <- cv.glmnet(x_train, y_train, lambda = grid, alpha = 1, nfolds = 12)

bestlam_lasso <- cv.lasso$lambda.min

lasso.pred <- predict(lasso.mod, 
                      s=bestlam_lasso, 
                      newx=x_train)
lasso_mse <- mean((lasso.pred-y_train)^2)




#Gradient Boosting Machine
library(gbm)
library(caret)
n_trees <- 100
learning_rate <- 0.01
interaction_depth <- 5

#predictor_names <- colnames(traindata)
#predictor_names <- predictor_names[-c(1,2,24)]
#predictors <- traindata[, predictor_names]
#response <- traindata$revenue

gbm_model <- gbm(revenue ~ ., data=traindata, n.trees = n_trees, shrinkage = learning_rate, interaction.depth = interaction_depth)

predictions <- predict(gbm_model, newdata = traindata)

mse <- mean((predictions - traindata$revenue)^2);mse

predictions <- predict(gbm_model, newdata = testdata)

rsquared <- cor(traindata$revenue, predictions)^2
print(paste("R-squared:", round(rsquared, 4)))
#much better r squared than random forest model


#Random Forest
#without cross validation
library(randomForest)

predictor_names <- colnames(traindata)
predictor_names <- predictor_names[-c(1,2,24)]
predictors <- traindata[, predictor_names]
response <- traindata$revenue


num_trees <- 100
mtry <- sqrt(ncol(predictors))
nodesize <- 1

rf_model <- randomForest(x = predictors, y = response, ntree = num_trees, mtry = mtry, nodesize = nodesize)
print(rf_model)

importance_measures <- importance(rf_model)
print(importance_measures)

#With cross-validation
# Specify the number of folds for cross-validation
num_folds <- 5

# Define the control parameters for cross-validation
ctrl <- trainControl(
  method = "cv",            # Cross-validation method
  number = num_folds,      # Number of folds
  verboseIter = FALSE      # Whether to display verbose output during cross-validation
)

# Train a Random Forest model using cross-validation
rf_model <- train(
  revenue ~ .,            # Formula specifying the response variable and predictor variables
  data = traindata,    # Training dataset
  method = "rf",           # Specify Random Forest as the method
  trControl = ctrl,        # Control parameters for cross-validation
  ntree = 100,             # Number of trees in the Random Forest model
  importance = TRUE       # Calculate variable importance measures
)

print(rf_model)
#with cross validation, r square is 16%

var_importance <- varImp(rf_model)
print(importance_measures)

##### at bottom: all the test MSE


ridge_mse

lasso_mse

