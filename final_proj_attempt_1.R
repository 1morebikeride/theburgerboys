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



testdata <- read.csv("test")

##clean test_data

### same as train

testdata <- select(testdata, -P14, -P15, -P16, -P17, -P18, -P24, -P25, -P26, -P27, -P30, -P31, -P32, -P33, -P34, -P35, -P36, -P37)


# extract the year from the character date
testdata$year <- substr(testdata$Open.Date, start = nchar(testdata$Open.Date) - 3, stop = nchar(testdata$Open.Date))

testdata$year <- as.numeric(testdata$year)
testdata$age <- 2015 - testdata$year

testdata <- select(testdata, -Open.Date)
testdata <- select(testdata, -year)
testdata <- select(testdata, -Id)


testdata$City <- as.factor(testdata$City)

testdata$City.Group <- as.factor(testdata$City.Group)

testdata$Type <- as.factor(testdata$Type)

testdata$P1 <- as.factor(testdata$P1)

testdata$P2 <- as.factor(testdata$P2)
testdata$P3 <- as.factor(testdata$P3)
testdata$P4 <- as.factor(testdata$P4)
testdata$P5 <- as.factor(testdata$P5)
testdata$P6 <- as.factor(testdata$P6)
testdata$P7 <- as.factor(testdata$P7)
testdata$P8 <- as.factor(testdata$P8)
testdata$P9 <- as.factor(testdata$P9)
testdata$P10 <- as.factor(testdata$P10)
testdata$P11 <- as.factor(testdata$P11)
testdata$P12 <- as.factor(testdata$P12)

testdata$P13 <- as.factor(testdata$P13)
testdata$P19 <- as.factor(testdata$P19)
testdata$P20 <- as.factor(testdata$P20)

testdata$P21 <- as.factor(testdata$P21)
testdata$P22 <- as.factor(testdata$P22)
testdata$P23 <- as.factor(testdata$P23)

testdata$P28 <- as.factor(testdata$P28)
testdata$P29 <- as.factor(testdata$P29)






### basic ridge and lasso
library(glmnet)
set.seed(1)

### we have NAs

#traindata[is.na(traindata)] <- 0

### we downloaded, just the training data

x_train <- traindata[, -which(names(data) == "revenue")]

#something wrong with matrix cannot run code
#x_train <- as.matrix(x_train)

y_train <- traindata[, "revenue"]

x_test <- testdata[, -which(names(data) == "revenue")]


y_test <- testdata[, "revenue"]


grid = 10^seq(-2, 4,length=200)


ridge_mod <- glmnet(x_train, y_train, lambda = grid, alpha = 1)

cv.ridge <- cv.glmnet(x_train, y_train, lambda = grid, alpha = 0, nfolds = 12)

bestlam_ridge <- cv.ridge$lambda.min


ridge.pred <- predict(ridge_mod, 
                      s=bestlam_ridge, 
                      newx=x_test)
ridge_mse <- mean((ridge.pred-y.test)^2)



lasso.mod <- glmnet(x_train, y_train, lambda = grid, alpha = 1,)

cv.lasso <- cv.glmnet(x_train, y_train, lambda = grid, alpha = 1, nfolds = 12)

bestlam_lasso <- cv.lasso$lambda.min

lasso.pred <- predict(lasso_mod, 
                      s=bestlam_lasso, 
                      newx=x_test)
lasso_mse <- mean((lasso.pred-y.test)^2)


#### test accuracy


#gam

#rf



##### at bottom: all the test MSE


ridge_mse

lasso_mse

