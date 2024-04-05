getwd()
setwd("E:/R weekender")
set.seed(101)

# loading both data - white wine and red wine 
white<-read.csv('winequality-white.csv',header=T) 
red<-read.csv('winequality-red.csv',header=T)

#understanding the data by checking its dimension, structure etc.
head(white)
dim(white)
str(white)

head(red)
dim(red)
str(red)
#both the red and white wine datasets have the same no of columns. 
#now let's check for na values

table(is.na(white))
table(is.na(red))
#no  missing values in both datasets 
#now binding the data row wise

data<-rbind(white,red)
data
str(data)
class(data)  #class is dataframe 
summary(data)
levels(data$quality)
colnames(data)

#checking for correlation
cor<-cor(data[,1:11])
cor>0.75 
library(corrplot)
corrplot(cor, method = 'circle')


library(usdm)
vif(data[,1:11])
#we exclude the variable'density
vif(data[,c(1:7,9:11)])
#now all values are less than 3   


data_new<-data[,c(1:2,4:7,9:12)]
colnames(data_new)

#outlier detection and treatment 
 boxplot(data_new)

 boxplot(data_new$residual.sugar)
 boxplot(data_new$free.sulfur.dioxide)
 boxplot(data_new$total.sulfur.dioxide)
 
# #dealing with outlier values
 
 quantile1<-quantile(data_new$residual.sugar,probs=c(0.25,0.75))
 range1<-1.5*IQR(data_new$residual.sugar)
data2<-subset(data_new,data_new$residual.sugar >(quantile1[1]-range1) & data_new$residual.sugar < (quantile1[2]+range1))
data2

 quantile2<-quantile(data2$free.sulfur.dioxide,probs=c(0.25,0.75))
range2<-1.5*IQR(data2$free.sulfur.dioxide)
data3<-subset(data2,(data2$free.sulfur.dioxide > (quantile2[1]-range2) & data2$free.sulfur.dioxide  < (quantile2[2]+range2)))
boxplot(data3)

quantile3<-quantile(data3$total.sulfur.dioxide,probs=c(0.25,0.75))
 range3<-1.5*IQR(data3$total.sulfur.dioxide)
 data4<-subset(data3,data3$total.sulfur.dioxide > (quantile3[1]-range3) & data3$total.sulfur.dioxide < (quantile3[2]+range3))
 
 quantile4<-quantile(data4$pH,probs=c(0.25,0.75))
 range4<-1.5*IQR(data4$pH)
 data5<-subset(data4,data4$pH > (quantile4[1]-range4) & data4$pH < (quantile4[2]+range4))
 
 quantile5<-quantile(data5$sulphates,probs=c(0.25,0.75))
 range5<-1.5*IQR(data5$sulphates)
 data6<-subset(data5,data5$sulphates > (quantile5[1]-range5) & data5$sulphates < (quantile5[2]+range5))
 

 quantile6<-quantile(data6$alcohol,probs=c(0.25,0.75))
 range6<-1.5*IQR(data6$alcohol)
 data1<-subset(data6,data6$alcohol > (quantile6[1]-range6) & data6$alcohol < (quantile6[2]+range6))
 

#we deal with dataset data1 now 
 
 
 
 #visualisations 
 hist(data1$fixed.acidity)
 hist(data1$volatile.acidity)
 hist(data1$residual.sugar)
 hist(data1$chlorides)
 
 hist(data1$free.sulfur.dioxide)
 hist(data1$total.sulfur.dioxide)
 hist(data1$pH)
 hist(data1$sulphates)
 
 
 #skewness and kurtosis 
 install.packages("Psych")
 library(psych)
 describe(data1)
 
nrow(white)
nrow(red)

train<-data1[1:nrow(white),]
test<-data1[-(1:nrow(white)),]
nrow(train)

nrow(test)

#making the model on the train dataset
model<-lm(quality~., data=train)
summary(step(model, direction='backward'))
options(scipen=100)
#by backward elimination we get the model to be 
#lm(formula = quality ~ volatile.acidity + residual.sugar + 
     #chlorides + free.sulfur.dioxide + total.sulfur.dioxide + 
     #pH + sulphates + alcohol, data = train)
colnames(data1)
#basically citric acid and fixed acidity varible should not be taken , so we remove both from data1 now 
#therefore all our important variables have been selected 

data1_dash<-data1[,2:10]
train<-data1_dash[1:nrow(white),]
test<-data1_dash[-(1:nrow(white)),]
model2<-lm(quality~., data=train)

#checking for OLS assumptions 
#Multicolinearity has already been taken care of above

#autocorelation 
  library(lmtest)
dwtest(model2)
#there is no autocorelation 

#heterocedasticity
plot(model2$fitted.values,model2$residuals)
#no funnel shape has been observed, therefore no heterocedasticity . 

#cjecking normality of errors 
qqnorm(model$residuals)
hist(model$residuals)
shapiro.test(model$residuals)

#error terms are normally distributed 
#Thus all OLS assumptions are checked 

#Accuracy 
plot(model2)
#its the qqnorm showing normality 

prediction<-predict(model2,test)
prediction
install.packages("Metrics")
library(Metrics)
colnames(test)
RMSE<-rmse(prediction,test[,9])
RMSE
#the value is 0.6745

