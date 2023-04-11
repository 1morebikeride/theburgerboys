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

#remove columns with missing data
traindata <- select(traindata, -P14, -P15, -P16, -P17, -P18, -P24, -P25, -P26, -P27, -P30, -P31, -P32, -P33, -P34, -P35, -P36, -P37)


#splitting date
traindata$year <- substr(as.character(traindata$Open.Date),7,10) %>% as.factor()
traindata$month <- substr(as.character(traindata$Open.Date),1,2) %>% as.factor()
traindata$day <- substr(as.character(traindata$Open.Date),4,5) %>% as.numeric()
traindata$Date <- as.Date(strptime(traindata$Open.Date, "%m/%d/%Y"))

#creating days since opening and months since opening
traindata$days <- as.numeric(as.Date("2014-02-02")-traindata$Date)
traindata$months <- as.numeric(as.Date("2014-02-02")-traindata$Date) / 30

#remove columns
traindata <- traindata[,-c(1,2,3,30)]

#change demo data to factors
traindata[, 1:22] <- lapply(traindata[, 1:22], factor)

#look for outliers
boxplot(traindata$revenue,
        main = "Boxplot of Revenue",
        ylab = "Revenue",
        col = "lightblue",
        border = "black",
        horizontal = TRUE)

#remove outliers
traindata <- traindata[traindata$revenue <= 13000000,]

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

x <- traindata[,-23]
y <- traindata[,23]
library(gbm)
library(caret)

trainControl <- trainControl(method = "cv",
                             number = 10,
                             returnResamp="all", ### use "all" to return all cross-validated metrics
                             search = "grid")

tune.grid <- expand.grid(interaction.depth = c(6, 7, 8, 9),
                       n.trees = (3:7) * 10,
                       shrinkage = c(0.05),
                       n.minobsinnode=c(5, 10, 15))

gbm.op <- train(x, y,
                method='gbm',
                tuneGrid=tune.grid,
                trControl=trainControl,
                verbose=FALSE,
                distribution='gaussian')
                    
#best hyperparameters
best.tune <- gbm.op$bestTune

#important variables
spam7Imp <- varImp(gbm.op, scale = T)
plot(spam7Imp, top = 5)

#predicting with grid model
pre_caret_gbm <- predict(gbm.op, newdata=traindata)
grid.mse <- mean((pre_caret_gbm - traindata$revenue)^2)

#creating new best model
best.mod <- gbm(revenue ~ ., data=traindata,
                interaction.depth = best.tune$interaction.depth,
                shrinkage = best.tune$shrinkage,
                n.minobsinnode = best.tune$n.minobsinnode,
                n.trees = best.tune$n.trees,
                distribution='gaussian',
                verbose=FALSE)

pre_caret_gbm <- predict(best.mod, newdata=traindata)
best.mod.mse <- mean((pre_caret_gbm - traindata$revenue)^2)

rsquared <- cor(traindata$revenue, pre_caret_gbm)^2
print(paste("R-squared:", round(rsquared, 4)))

##### at bottom: all the test MSE


ridge_mse

lasso_mse

