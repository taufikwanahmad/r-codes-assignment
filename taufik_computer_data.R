attach(Computer_Data)
summary(Computer_Data)
str(Computer_Data)

newdata<-Computer_Data[,2:11] #x1 is ID, no need to include
summary(newdata)

#cd,multi,premium are categorical. must change to dummy variables to make it possible or multilinear regression

model1 <- lm(formula = price ~ ., data = newdata)
summary(model1)
install.packages("dummy")
library(dummy)


install.packages("dummies")
library(dummies)


install.packages("dplyr")
library(dplyr)



newdata$cd <- ifelse((newdata$cd == "yes"),
                   c(1),
                   ifelse((newdata$cd == "no"),
                          c(0),
                          newdata$cd))


newdata$multi <- ifelse((newdata$multi == "yes"),
                      c(1),
                      ifelse((newdata$multi == "no"),
                             c(0),
                             newdata$multi))


newdata$premium <- ifelse((newdata$premium == "yes"),
                        c(1),
                        ifelse((newdata$premium == "no"),
                               c(0),
                               newdata$premium))
summary(newdata)
str(newdata)
pairs(newdata)
cor(newdata)


model2 <- lm(formula = price ~ ., data = newdata)
summary(model2)



predict(model2, interval = "predict")


q1<-lm(price ~ cd) 
summary(q1)


q2<-lm(price ~ multi) 
summary(q2)


q3<-lm(price ~ premium) 
summary(q3)



model2 <- lm(price ~ log(speed) + log(hd) + log(ram) + log(screen) + (cd) + (multi) + (premium) + log(ads) + log(trend))
summary(model2)


model3<- lm(log(price) ~(speed) + (hd) + (ram) + (screen) + (cd) + (multi) + (premium) + (ads) + (trend))
summary(model3)


model4<-lm(exp(log(price))~ ., data=newdata)
summary(model4)


model5<-lm((price) ~ .^2, data=newdata)
summary(model5)


model6<- lm((price^2) ~ ., data=newdata)
summary(model6)


model7 <- lm((price^2)^1/2 ~ ., data=newdata)
summary(model7)

#model 5 have highest r squared value, where square are applied on all input paramters