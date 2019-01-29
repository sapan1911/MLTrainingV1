# Random Forest Regression

# Importing the dataset
dataset = read.csv('Book1.csv')
dataset = dataset[,c(1,2,3,4,5,6,10)]

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Output1, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
#training_set = scale(training_set)
#test_set = scale(test_set)

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
#library(randomForest)
#set.seed(1234)
#regressor = randomForest(x = training_set[-7],
                         #y = training_set$Output1,
                         #ntree = 500)
regressor = lm(formula = Output1 ~ Feature3+Feature5+Feature2+Feature6,
               data = training_set)
# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, test_set)
summary(regressor)

# Visualising the Random Forest Regression results (higher resolution)
# install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Random Forest Regression)') +
  xlab('Level') +
  ylab('Salary')