Multiple Regression-problem-2

library(readr)
attach(Computer_Data)
View(Computer_Data)# we observe categorical data in the columns of cd,multi and premium.

data5=Computer_Data[,2:11]#here column 1 is the serial number so we ignore that column and store the remaining in new dataset
#data5
summary(data5)#we check for the mean and median which shows that mean> median.we also check that as there is categorical data
and hence mean,median and range values cannot be displayed.

# Building Multiple Linear Regression model before cleaning-up the data for categorical variable values.
model2 <- lm(formula = price ~ ., data = data5)#we build a linear model/equation and store the data into a new variable.
summary(model2)#we observe all the values of the i/p parameters are significant,except for the categorical values in the
columns.And the r-Squared value is o.7123(71.23%),a moderate accuracy in the built model.

# we observed that there are Dummy Variables in 3 columns (cd, multi, premium) which are categorical(text) values.
so we Replace these categorical values with numerical values by giving binary (0/1) i/ps.
#so individually for each column we create dummy variables.

install.packages("dummy")
library(dummy)
install.packages("dplyr")
library(dplyr)
install.packages("dummies")
library(dummies)

#dummy values for cd variable
data5$cd <- ifelse((data5$cd == "yes"),
                  c(1),
                  ifelse((data5$cd == "no"),
                         c(0),
                         data5$cd))



#dummy values for multi variable
data5$multi <- ifelse((data5$multi == "yes"),
                     c(1),
                     ifelse((data5$multi == "no"),
                            c(0),
                            data5$multi))



#dummy values for premium variable
data5$premium <- ifelse((data5$premium == "yes"),
                       c(1),
                       ifelse((data5$premium == "no"),
                              c(0),
                              data5$premium))

# we check the Summary of dataset for mean/median post data clean-up.

summary(data5) #we check for the mean and median which shows that mean> median and the range is also high,except for the
#cd,multi,premium where we find zero mean and zero median.
str(data5)# we find the structure of the dataset as a combination of integer and numeric values for the columns.

# Scatter plot between all the variables

pairs(data5)# This pairs function gives the scatter diagram for all pairs in one plot and we are unable to read the 
#plots as it's too small.

# Correlation analysis i.e., checking value of 'r'
cor(data5)# here we cannot define the corelation as we observed that the standard deviation is zero.

#after cleaning up the data for the categorical values we again build the linear model/equation.
model3 <- lm(formula = price ~ ., data = data5)#we build a linear model/equation and store the data into a new variable.
summary(model3)#we observe all the values of the i/p parameters are significant,except for the categorical values in the
columns.we find no change in the r-Squared value which is o.7123(71.23%),a moderate accuracy in the built model.

predict(model2, interval = "predict")#here we predict the model for its fit in the confidence interval.

# There are insignificant values observed for cd,multi and premium and which indicates an 
#influence in building the model.

## we check individually these i/p parameters which are infuencing in model building.##

q1<-lm(price ~ cd) 
summary(q1) # when observed with Administration i/p its shows a significant value which can be used 
#in the equation and the R-Squared value is 0.03894 which indicates the model to be of poor accuracy.

q2<-lm(price ~ multi) 
summary(q2) # when observed with Administration i/p its shows an insignificant value which cannot be used 
#in the equation and the R-Squared value is 0.0002773 which indicates the model to be of very very poor accuracy.

q3<-lm(price ~ premium) 
summary(q3) # when observed with Administration i/p its shows an insignificant value which cannot be used 
#in the equation and the R-Squared value is 0.006512 which indicates the built model is to be of very poor accuracy.
 
# in the above we observed the o/p with insignificant i/p parameters which showed very poor R-Squared values with poor 
accuracies.

# we apply certain transformation techniques to improve the R-Squared value

# log(x)(we apply log transformation to all the i/p parameters,except for the categorical variables cd,multi,premium)
model2 <- lm(price ~ log(Number) + log(speed) + log(hd) + log(ram) + log(screen) + (cd) + (multi) + (premium) + log(ads) + log(trend))
summary(model2)# we observed that R-Squared value is 0.7123,which shows a moderate accuracy in the built model

#log(y)(we apply log transformation to the o/p parameter)
model3<- lm(log(price) ~ (Number) + (speed) + (hd) + (ram) + (screen) + (cd) + (multi) + (premium) + (ads) + (trend))
summary(model3) # we observed that R-Squared value is 0.7123,which is same as previous and which shows a moderate accuracy 
#in the built model

#exp(log(y))#as we need the o/p y independent of log so we apply exponential transformation to the o/p parameter
model5<-lm(exp(log(price))~ ., data=data5)
summary(model5)# we observed that R-Squared value is 0.7123,which is same as previous and which shows a moderate accuracy 
#in the built model.

# sqr of x (we apply square transformation to all the i/p parameters)
model6<-lm((price) ~ .^2, data=data5)

summary(model6)# we observed that R-Squared value has increased to 0.768,which shows a moderate accuracy 
#in the built model.

# sqr of y (we apply square transformation to the o/p parameter)
model7 <- lm((price^2) ~ ., data=data5)
summary(model7) # we observed that R-Squared value has decreased to 0.6601,which shows a poor accuracy 
#in the built model.

# sqroot of y (we apply squareroot transformation to the o/p parameter as we need y alone)
model9 <- lm((price^2)^1/2 ~ ., data=data5)
summary(model9) # we observed that R-Squared is 0.6601 as previous,which shows a poor accuracy 
#in the built model.

# Among the different models built we choose model6 as the better model,because the R-Squared value is 0.768(76.8%),
#which shows a better moderate accuracy in the built model.














