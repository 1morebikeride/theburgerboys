setwd("C:/Users/andre/Documents/msba/spring/ML")

rm(list=ls())

set.seed(1693)

data <- read.csv("train.csv")

#data$Open.Date <- as.Date(Open.Date)



#14 to 18
# 24 to 27
#30 to 37
library(dplyr)

data <- select(data, -P14, -P15, -P16, -P17, -P18, -P24, -P25, -P26, -P27, -P30, -P31, -P32, -P33, -P34, -P35, -P36, -P37)


# extract the year from the character date
data$year <- substr(data$Open.Date, start = nchar(data$Open.Date) - 3, stop = nchar(data$Open.Date))

data$year <- as.numeric(data$year)
data$age <- 2015 - data$year

data <- select(data, -Open.Date)
data <- select(data, -year)
data <- select(data, -Id)

               
data$City <- as.factor(data$City)

data$City.Group <- as.factor(data$City.Group)

data$Type <- as.factor(data$Type)

data$P1 <- as.factor(data$P1)

data$P2 <- as.factor(data$P2)
data$P3 <- as.factor(data$P3)
data$P4 <- as.factor(data$P4)
data$P5 <- as.factor(data$P5)
data$P6 <- as.factor(data$P6)
data$P7 <- as.factor(data$P7)
data$P8 <- as.factor(data$P8)
data$P9 <- as.factor(data$P9)
data$P10 <- as.factor(data$P10)
data$P11 <- as.factor(data$P11)
data$P12 <- as.factor(data$P12)

data$P13 <- as.factor(data$P13)
data$P19 <- as.factor(data$P19)
data$P20 <- as.factor(data$P20)

data$P21 <- as.factor(data$P21)
data$P22 <- as.factor(data$P22)
data$P23 <- as.factor(data$P23)

data$P28 <- as.factor(data$P28)
data$P29 <- as.factor(data$P29)


##make changes


### basic ridge and lasso

#gam

#rf





