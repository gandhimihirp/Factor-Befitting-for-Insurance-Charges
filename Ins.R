
## initializing and loading the dataset and looking at the sample dataset ##
insurance = read.csv("insurance.csv")
head(insurance)

##loading the libraries
library(ggplot2)
library(MASS)
library(lmtest)
library(car)

### pre checking the values as target variable should follow normal distribution  ### 

hist(insurance$charges)

### target variable is right skewed 
## apply log transformation to make it a normal distribution

insurance$log_charges = log(insurance$charges)

hist(insurance$log_charges)

### finding the linear relationship b/w input and target for both the original and log values

plot(insurance$age, insurance$log_charges)
plot(insurance$age , insurance$charges)

### multicollinearity (input variables are correlated with each other)

cor(insurance$age, insurance$bmi) ### cor with input variable (not desirable as value tends to 0)

#checking the effect of smokers on the log_charge values
ggplot(insurance, aes( smoker, log_charges)) + geom_boxplot()

### Model bulding for linear regression analysis ### 

insurance$charges = NULL 

## splitting the test and train sets of data

set.seed(675)

test_train = sample(nrow(insurance), nrow(insurance)*0.8)

train = insurance[test_train,]
test = insurance[-test_train,]

## model 

lin_model = lm( log_charges ~ . , data=train )

summary(lin_model)

## Test the model 

test$pred = predict(lin_model, newdata=test)

### finding the RMSE

test$error = test$log_charges - test$pred

test$error_sq = test$error ** 2

rmse = sqrt(mean(test$error_sq))

rmse

summary(test$log_charges)

cofv <- 0.43/9.13 #coefficient of variation
cofv

################ Diagnosis #####################

### select only a few variable 

fit = lm(log_charges ~ ., data=train)

#### correlation check or Multicollinearity 
summary(fit)
names(fit)

fit$coefficients 

head(fit$residuals) 

### check for normality of errors  by plotting the residuals plot

hist(fit$residuals)

### check for auto correlation and heteroscedasticity

## Standardized residuals 
residuals = stdres(fit)

summary(residuals)

### predicted values vs. fitted values 

plot(fit$fitted.values, residuals)

### statistical test for autocorelation
## to check auto correlation

dwtest(fit)

### Outliers test

outlierTest(fit)

### measure cooks.distance 

cd = cooks.distance(fit)

cutoff = 4/(nrow(train) - length(fit$coefficients))

### plot for finding the observation which has high leverage (using value cd) 
plot(fit, which=4, cook.levels=cutoff)

#Outliers and leverage observations

### Variance inflation factor to check multicollinearity 

vif(fit)

## rebuild the model by removing outlier observations 

train = train[ -c(431, 220, 1028, 1040, 103,527, 1020), ]

####### fit the model for train ######

fit = lm(log_charges  ~  . -bmi , data=train)

summary(fit)

## check if Autcorrelation or Heteroscedasticty has improved 
plot(fit, which=3)

##explore the relationship between target and input 

ggplot(train, aes( bmi, log_charges)) + geom_point()

ggplot(train, aes( age, log_charges)) + geom_point()

###combine bmi and age by dividing them and taking the square root into a new variable 

train$age_bmi = sqrt(train$bmi/train$age)

ggplot(train, aes( sqrt(age_bmi), log_charges)) + geom_point()

#### final model with ratio of age and bmi 

fit = lm( log_charges ~ . -bmi -age,  data=train)
summary(fit) 

### combine age and bmi by multiplying them and taking them as a new variable
train$bmi.age = log( train$bmi*train$age)

ggplot( train, aes( sqrt(bmi.age), log_charges)) + geom_point()

#adding the new value to the fit model
fit = lm( log_charges ~ . -bmi -age -age_bmi, data=train)

fit_pred = lm( log_charges ~ . -bmi -age -age_bmi, data=train)

summary(fit)

plot(fit, which = 1)

########### fit the model for test ##########

fit_test = lm(log_charges  ~  . -bmi , data=test)

summary(fit_test)

## check if Autocorrelation or Heteroscedasticty has improved 
plot(fit_test, which=3)

##explore the relationship b/w target and input 

ggplot( test, aes( bmi, log_charges)) + geom_point()

ggplot( test, aes( age, log_charges)) + geom_point()

### combine bmi and age into a new variable (similar to train model)

test$age_bmi = sqrt(test$bmi/test$age)

ggplot( test, aes( sqrt(age_bmi), log_charges)) + geom_point()

#### final model with ratio of age and bmi 

fit_test = lm( log_charges ~ . -bmi -age,  data= test)

summary(fit_test) 

test$bmi.age = log( test$bmi*test$age)

ggplot( test, aes( sqrt(bmi.age), log_charges)) + geom_point()

#fitting the model with the new added variables
fit_test = lm( log_charges ~ . -bmi -age -age_bmi, data=test)

summary(fit_test)

plot(fit_test, which = 1)

##############predictive model test###################

#case 1
bob <- data.frame(age = 19, sex = "male", bmi = 27.9, children = 0, smoker = "yes", region = "northwest", age_bmi = 0.982, bmi.age = 6.40)
charges.bob = predict(fit, bob)
charges.bob

#case 2
tracy <- data.frame(age = 30, sex = "female", bmi = 27.9, children = 2, smoker = "yes", region = "northwest", age_bmi = 0.884, bmi.age = 5.32)
charges.tracy = predict(fit, tracy)
charges.tracy

#case 3
jim <- data.frame(age = 20, sex = "male", bmi = 20, children = 0, smoker = "yes", region = "northwest", age_bmi = 0.729, bmi.age = 6.10)
charges.jim = predict(fit, jim)
charges.jim

#case 4
lucy <- data.frame(age = 20, sex = "female", bmi = 20, children = 0, smoker = "no", region = "northeast", age_bmi = 0.729, bmi.age = 6.10)
charges.lucy = predict(fit, lucy)
charges.lucy