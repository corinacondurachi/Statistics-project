# Import dataset
dataset <- as.data.frame(Seatbelts)
View(dataset)

#----------------------1----------------------------
# Media penstru fiecare coloana
Mean <- apply(dataset, 2, mean)
View(Mean)

# Dispersia
Var <- var(dataset)
View(Var)

# Quantile
Quantile <- apply(dataset, 2, quantile)
View(Quantile)

# Boxplot
#boxplot(DriversKilled~ kms, data = dataset, xlab = "Number of drivers killed",
#        ylab = "Number of kms", main = "Seatbelts Data")

#boxplot(DriversKilled~ drivers, data = dataset, xlab = "Number of drivers killed",
#        ylab = "Number of kms", main = "Seatbelts Data")

#install.packages("tidyverse")
boxplot(dataset$DriversKilled, main = "DriversKilled")
boxplot(dataset$drivers, main = "drivers")
boxplot(dataset$rear, main = "rear")
boxplot(dataset$front, main = "front")
boxplot(dataset$kms, main = "kms")
boxplot(dataset$PetrolPrice, main = "PetrolPrice")
boxplot(dataset$VanKilled, main = "VanKilled")
#install.packages('ggplot2')
library(ggplot2)
ggplot(dataset, aes(x = factor(law), y = DriversKilled)) + geom_boxplot(fill = "pink") +
  ylab ("Monthly Driver Mortality") + xlab("Before and after law introduced")


boxplot(dataset$DriversKilled, main = "DriversKilled")
boxplot(dataset$drivers, main = "drivers")
boxplot(dataset$rear, main = "rear")
boxplot(dataset$front, main = "front")
boxplot(dataset$kms, main = "kms")
boxplot(dataset$PetrolPrice, main = "PetrolPrice")
boxplot(dataset$VanKilled, main = "VanKilled")
boxplot(
  dataset$DriversKilled ~ dataset$law,
  xlab = "law",
  ylab = "Drivers killed",
  main = "Drivers killed vs law",
  las = 1
)


#----------------------2----------------------------

#Simple Linear Regression
library(caTools)
split = sample.split(dataset$DriversKilled, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting Multiple Linear regression to the Training Set
regressor2 = lm(formula = DriversKilled ~ drivers , data = training_set)

#Predicting the Test set results
y_pred2 = predict(regressor2, newdata = test_set)

# Visualising the Training set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$drivers, y = training_set$DriversKilled),
             colour = 'red') +
  geom_line(aes(x = training_set$drivers, y = predict(regressor2, newdata = training_set)),
            colour = 'blue') +
  ggtitle('DriversKilled vs Drivers (Training set)') +
  xlab('Drivers') +
  ylab('DriversKilled')

# Visualising the Test set results
library(ggplot2)
ggplot() +
  geom_point(aes(x = test_set$drivers, y = test_set$DriversKilled),
             colour = 'red') +
  geom_line(aes(x = training_set$drivers, y = predict(regressor2, newdata = training_set)),
            colour = 'blue') +
  ggtitle('DriversKilled vs Drivers (Test set)') +
  xlab('Drivers') +
  ylab('DriversKilled')



#Multiple Linear Regression

#Splitting the dataset into the Training set and Test set
library(caTools)
split = sample.split(dataset$DriversKilled, SplitRatio = 2/3)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting Multiple Linear regression to the Training Set
regressor = lm(formula =DriversKilled ~ ., data = training_set)
summary(regressor)

#Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

#Building the optimal model using Backward Elimination
regressor=lm(formula=DriversKilled ~ drivers + front + kms + PetrolPrice + VanKilled + law 
             ,data=dataset)
summary(regressor)

regressor=lm(formula=DriversKilled ~ drivers + kms + PetrolPrice + VanKilled + law 
             ,data=dataset)
summary(regressor)

regressor=lm(formula=DriversKilled ~ drivers + kms + VanKilled + law 
             ,data=dataset)
summary(regressor)

regressor=lm(formula=DriversKilled ~ drivers + kms + law 
             ,data=dataset)
summary(regressor)

regressor=lm(formula=DriversKilled ~ drivers + kms  
             ,data=dataset)
summary(regressor)

modelSummary <- summary(regressor)  # capture model summary as an object for multiple liniar regression
modelSummary2 <- summary(regressor2)  # capture model summary as an object for simple liniar regression

#R squared test
modelSummary2$r.squared
modelSummary$r.squared

#Adjusted R squared test
modelSummary2$adj.r.squared 
modelSummary$adj.r.squared 

#AIC test
AIC(regressor2)
AIC(regressor)

#BIC test
BIC(regressor2)
BIC(regressor)

#F statistic test
modelSummary2$fstatistic[1]  
modelSummary$fstatistic[1]  

#Standard error
modelCoeffs2 <- modelSummary2$coefficients  # model coefficients SLR(simple liniar regression)
beta.estimate2 <- modelCoeffs2["drivers", "Estimate"]  # get beta estimate for DriversKilled
std.error2 <- modelCoeffs2["drivers", "Std. Error"]  # get std.error for DriversKilled
std.error2

modelCoeffs <- modelSummary$coefficients  # model coefficients MLR(multiple liniar regression)
beta.estimate <- modelCoeffs["drivers", "Estimate"]  # get beta estimate for DriversKilled
std.error <- modelCoeffs["drivers", "Std. Error"]  # get std.error for DriversKilled
std.error



#----------------------3----------------------------

library(DiscreteLaplace)

# The Probability mass function
p<-0.7
q<-0.45


x<--15:15
prob<-ddlaplace(x, p, q)
plot(x, prob, type="h", main="The Probability mass function")

# The cumulative distribution function
cmd<-pdlaplace(x, p, q)
plot(x,cmd,main="The cumulative distribution function")





