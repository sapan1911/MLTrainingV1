# Simple Linear Regression

# Importing the dataset
dataset = read.csv('Book1.csv')

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Output1, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
colnames(dataset)

# Feature Scaling
training_set = scale(training_set)
test_set = scale(test_set)

# Fitting Simple Linear Regression to the Training set
regressor = lm(formula = Output2 ~ Feature1+Feature2+Feature3+Feature5+Feature6+Output1,
               data = training_set)
summary(regressor)
#install.packages("Boruta")
#library(Boruta)
#boruta.train <- Boruta(Output1~Feature1+Feature2+Feature3+Feature4+Feature5+Feature6,data = training_set, doTrace = 3, holdHistory = F)  
#library(rpart)
#regressor = rpart(formula = Output2 ~ Feature1+Feature2+Feature3+Feature5+Feature6,
                  #data = training_set,
                  #control = rpart.control(minsplit = 7))
#regressor=lm(formula = Output1 ~ Feature3+Feature5+Feature6,
             #data = training_set)

#library(e1071)
#regressor = svm(formula = Output2 ~ Feature1+Feature2+Feature3+Feature5+Feature6,
                #data = training_set,
                #type = 'eps-regression',
                #kernel = 'radial')

# Fitting Polynomial Regression to the dataset
#dataset$Feature32 = dataset$Feature3^2


#dataset$Feature52 = dataset$Feature5^2
#dataset$Feature53 = dataset$Feature5^3
#dataset$Feature54 = dataset$Feature5^4
#dataset$Feature62 = dataset$Feature6^2
#dataset$Feature63 = dataset$Feature6^3
#dataset$Feature64 = dataset$Feature6^4

#dataset$Feature55 =dataset$Feature5^5

#regressor = lm(formula = Output2 ~ Feature55+Feature64+Feature63+Feature62+Feature54+Feature53+Feature52+Feature32,
              #data = training_set)

plot(regressor)
text(regressor)
summary(regressor)

# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)
library(hydroGOF)
RMSE=rmse(y_pred,test_set$Output2)
RMSE
summary(y_pred)
#plot(y_pred)

y_pred
# Visualising the Training set results

library(tidyverse)
library(caret)
library(leaps)
library(MASS)

regressor1<- stepAIC(regressor, direction = "both", 
           trace = FALSE)
summary(regressor1)
library(glmnet)
regressor<-glmnet(training_set$Feature1+training_set$Feature2+training_set$Feature3+training_set$Feature5+training_set$Feature6,training_set$Output1, family="gaussian", alpha=1)
