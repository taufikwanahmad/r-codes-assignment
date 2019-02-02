
View(X50_Startups)
attach(X50_Startups)

summary(X50_Startups)

attach(X50_Startups)

install.packages("dummy")
library(dummy)

dummy(data.frame(X50_Startups$State))
state1<-dummy(data.frame(X50_Startups$State))
state1

data2=X50_Startups[-4]
View(data2)


install.packages("reshape")
library(reshape)

data2=cbind(data2,state1)
View(data2)

summary(data2)

boxplot(data2)

attach(data2)

data2<-data.frame(lapply(data2,as.character))
str(data2)
data2<-data.frame(lapply(data2, as.numeric))
str(data2)


hist(data2$R.D.Spend) 
hist(data2$Administration)
hist(data2$Marketing.Spend)
hist(data2$X50_Startups.State_California)
hist(data2$X50_Startups.State_Florida)
hist(data2$X50_Startups.State_New.York)
hist(data2$Profit)

barplot(data2$X50_Startups.State_California)
barplot(data2$X50_Startups.State_Florida)
barplot(data2$X50_Startups.State_New.York)

table(data2$X50_Startups.State_California)
table(data2$X50_Startups.State_Florida)
str(data2)
pairs(data2)
cor(data2)

attach(data2)

install.packages("Hmisc")
library(Hmisc)

p<-lm(Profit ~ .,data=data2) 
summary(p)

q<-lm(Profit ~ Administration) 
summary(q)

m<-lm(Profit ~Marketing.Spend) 
summary(m)        

s<-lm(Profit ~ X50_Startups.State_New.York)  
summary(s)

t<-lm(Profit ~ data2$Administration+data2$Marketing.Spend)
summary(t)

u<-lm(Profit ~ Administration+data2$X50_Startups.State_New.York)
summary(u)

v<-lm(Profit ~ Marketing.Spend+data2$X50_Startups.State_New.York)
summary(v)



w<-lm(Profit ~ Administration+Marketing.Spend+X50_Startups.State_New.York)
summary(w)


install.packages("car")
library(car)

influenceIndexPlot(p,id.n=2)

x<-lm(Profit ~ .,data=data2[-50])
summary(x)
        