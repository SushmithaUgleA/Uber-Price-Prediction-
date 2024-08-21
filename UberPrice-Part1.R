library(fpp2)
library(urca)
library(readxl)
library(ggplot2)
library(forecast)
library(dplyr)
library(zoo)

data = read.csv("/Users/sushmithaugle/Library/Mobile Documents/com~apple~CloudDocs/Spring 2024/ECON 5337-Business & Economic Forecasting/Project/UBER Historical Data.csv", sep = ",")

#interpolating missing values
data$Date = na.locf(data$Date, na.rm = FALSE) 
data$Price = na.approx(data$Price)
data$Open = na.approx(data$Open)
data$High = na.approx(data$High)
data$Low = na.approx(data$Low)

data$Vol.[is.na(data$Vol.)] = na.locf(data$Vol., na.rm = FALSE)
data$Change..[is.na(data$Change..)] = na.locf(data$Change.., na.rm = FALSE)

View(data)
data
length(data)
nrow(data)
summary(data)

tsn = ts(data[,"Date"], start =c(2019, 1), frequency=1) 
tsn 
summary(tsn)
autoplot(tsn)


myts_u =ts(data, start=c(2019, 1), end=c(2024, 1), frequency=365)
myts_u
#plotting the data
plot(myts_u)
#strong increasing trend with some cycles
ggseasonplot(myts_u)

#as we dont have seasonal data in stock data - seasonality is not included 




#Exponential Smoothing for date 
str(mydata_ts)
univariate_ts = myts_u[, "Date"]

univariate_ts



# Check the structure and head of the univariate time series data
str(univariate_ts)
head(univariate_ts)
length(univariate_ts)
ses1 = ses(univariate_ts, h = 12, alpha = 0.5)
ses1


ses2 = ses(univariate_ts,h=12,alpha = 0.75)
ses2

#creating a forecast with the alpha value determined through minimizing least squares
seso = ses(univariate_ts,h=12)
seso
seso$model

autoplot(window(univariate_ts))+
  autolayer(ses1,series = "Alpha=0.5",PI=F)+
  autolayer(ses2,series = "Alpha=0.75",PI=F)+
  autolayer(seso,series = "Alpha=Optimal",PI=F)

#creating a forecast with the holt model. It adds trend to the ses model. 
mod1 = holt(univariate_ts,h=12)
mod1
mod1$model

autoplot(window(univariate_ts))+
  autolayer(ses1,series = "Alpha=0.5",PI=F)+
  autolayer(ses2,series = "Alpha=0.75",PI=F)+
  autolayer(seso,series = "Alpha=Optimal",PI=F)+
  autolayer(mod1,series = "Holt",PI=F)

#creating forecasts with holt-winter's. It adds trend and seasonality to ses.
#seasonality can be included additively if it's agnostic to the level of the 
#time series

#or multiplicatively if it increases with the level of the time series. 
mod2 = hw(univariate_ts,h=12,seasonal = "multiplicative")
mod2$model

autoplot(univariate_ts)

autoplot(window(univariate_ts))+
  autolayer(ses1,series = "Alpha=0.5",PI=F)+
  autolayer(ses2,series = "Alpha=0.75",PI=F)+
  autolayer(seso,series = "Alpha=Optimal",PI=F)+
  autolayer(mod1,series = "Holt",PI=F)+
  autolayer(mod3,series = "Multiplicative Season",PI=F)

#we can also include a damped trend if we're worried about over forecasting.
mod4 = holt(univariate_ts,h=12,damped = T)
mod4$model

autoplot(window(univariate_ts))+
  autolayer(mod1,series = "Holt",PI=F)+
  autolayer(mod3,series = "Multiplicative Season",PI=F)+
  autolayer(mod4,series = "Damped",PI=F)







#checking exponential smoothing again for price

univariate_ts1= myts_u[,"Price"]
univariate_ts1
summary(univariate_ts1)

autoplot(univariate_ts1)
#Increasing trend till 2022 and decreasing till 2024


#we can either specify alpha
help("ses")
sesFit1 = ses(univariate_ts1,h=12,alpha = 0.5)
sesFit2 = ses(univariate_ts1,h=12,alpha = 0.25)
summary(sesFit1)
summary(sesFit2)

autoplot(univariate_ts1)+
  autolayer(sesFit1)+
  autolayer(sesFit2)
#almost the same

#or we can let R pick alpha to minimizing the squared residuals.
sesFitOptimal = ses(univariate_ts1,h=12)
summary(sesFitOptimal)
#and we can graph this to see what it looks like
autoplot(univariate_ts1)+
  autolayer(sesFitOptimal)
#looks like this 

#Holt
#the holt function will include a trend term on top of the standard ses
holtFit1 = holt(univariate_ts1,h=12)
summary(holtFit1)

autoplot(univariate_ts1)+
  autolayer(holtFit1,series = "Holt", PI=FALSE)

#Holt damped
#or if we need to include a damped term we can do
holtFit2 = holt(univariate_ts1,h=12,damped = TRUE)
summary(holtFit2)
#we can graph theses to see the different by damping
autoplot(univariate_ts1)+
  autolayer(holtFit1,series = "Holt", PI=FALSE)+
  autolayer(holtFit2,series = "Damped", PI=FALSE)

#Holt Winter
#we can also add in a seasonal component in addition to the trend additively
hwFit1 = hw(univariate_ts1,seasonal = "additive",h=12)
summary(hwFit1)

#or multiplicatively
hwFit2 = hw(univariate_ts1,seasonal = "multiplicative",h=12)
summary(hwFit2)

#and we can compare them graphically
autoplot(univariate_ts1)+
  autolayer(hwFit1,series = "Additive", PI=FALSE)+
  autolayer(hwFit2,series = "Multiplicative", PI=FALSE)


ses_1 = ses(univariate_ts1,h=12,alpha = 0.5)
ses_1

ses_2 = ses(univariate_ts1,h=12,alpha = 0.75)
ses_2

#creating a forecast with the alpha value determined through minimizing least squares
ses_o = ses(univariate_ts1,h=12)
ses_o
ses_o$model

autoplot(window(univariate_ts1,start=2019))+
  autolayer(ses1,series = "Alpha=0.5",PI=F)+
  autolayer(ses2,series = "Alpha=0.75",PI=F)+
  autolayer(seso,series = "Alpha=Optimal",PI=F)

#creating a forecast with the holt model. It adds trend to the ses model. 
mod_1 = holt(univariate_ts1,h=12)
mod_1
mod_1$model

autoplot(window(univariate_ts1,start=2019))+
  autolayer(ses1,series = "Alpha=0.5",PI=F)+
  autolayer(ses2,series = "Alpha=0.75",PI=F)+
  autolayer(seso,series = "Alpha=Optimal",PI=F)+
  autolayer(mod1,series = "Holt",PI=F)

#creating forecats with holt-winter's. It adds trend and seasonality to ses.
#seasonality can be included additively if it's agnostic to the level of the 
#time series
mod_2 = hw(univariate_ts1,h=12,seasonal = "additive")
mod_2$model
mod_2

#or multiplicatively if it increases with the level of the time series. 
mod_3 = hw(univariate_ts1,h=12,seasonal = "multiplicative")
mod_3$model

autoplot(univariate_ts1)

autoplot(window(univariate_ts1,start=2019))+
  autolayer(ses_1,series = "Alpha=0.5",PI=F)+
  autolayer(ses_2,series = "Alpha=0.75",PI=F)+
  autolayer(ses_o,series = "Alpha=Optimal",PI=F)+
  autolayer(mod_1,series = "Holt",PI=F)+
  autolayer(mod_2,series = "Additive Season",PI=F)+
  autolayer(mod_3,series = "Multiplicative Season",PI=F)

#we can also include a damped trend if we're worried about over forecasting.
mod_4 = holt(univariate_ts1,h=12,damped = T)
mod_4$model

autoplot(window(univariate_ts1,start=2019))+
  autolayer(mod_1,series = "Holt",PI=F)+
  autolayer(mod_2,series = "Additive Season",PI=F)+
  autolayer(mod_3,series = "Multiplicative Season",PI=F)+
  autolayer(mod_4,series = "Damped",PI=F)

#training and testing data split 
#and we determine which model a particular time series best using training
#and testing sets.

length(myts_u)

#checking for univariate_ts
summary(univariate_ts1)
length(univariate_ts1)

1826*.8
#checking for TS object
summary(myts_u)
length(myts_u)



traindata = head(univariate_ts1,1460)
summary(traindata)
1826-1460


testdata = tail(univariate_ts1,366)
summary(testdata)


trainHolt = holt(traindata,h=366)
trainHolt
trainDamped= holt(traindata,h=366,damped = T)
trainDamped
#
trainAdd= hw(traindata,h=366,seasonal = "additive")
trainAdd
trainMulti= hw(traindata,h=366,seasonal = "multiplicative")
trainMulti

accuracy(trainHolt,testdata)
accuracy(trainDamped,testdata)
accuracy(trainAdd,testdata)
accuracy(trainMulti,testdata)

autoplot(univariate_ts)+
  autolayer(trainAdd,series = "seasonal add",PI=F)+
  autolayer(trainMulti,series = "seasonal multi",PI=F)+
  autolayer(trainDamped,series = "Damped",PI=F)+
  autolayer(trainHolt,series = "holt",PI=F)

#we can also include damped trends with the holt winter's model. 
trainAddDamp = hw(traindata,h=13,seasonal = "additive",damped = T)
accuracy(trainAddDamp,testdata)

autoplot(univariate_ts1)+
  autolayer(trainAdd,series = "seasonal add",PI=F)+
  autolayer(trainMulti,series = "seasonal multi",PI=F)+
  autolayer(trainDamped,series = "Damped",PI=F)+
  autolayer(trainHolt,series = "holt",PI=F)+
  autolayer(trainAddDamp,"Additive Damped",PI=F)+
  autolayer(trainMultiDamp,"Multiplicative Damped",PI=F)

trainMultiDamp = hw(traindata,h=13,seasonal = "multiplicative",damped = T)
accuracy(trainMultiDamp,testdata)

holt_linear_trend_model = HoltWinters(univariate_ts, beta = TRUE)
ets_model <- ets(univariate_ts)
autoplot(ets_model)
#time series regression

#creating a ts object from the data set
myts_u =ts(data, start=c(2019, 1), end=c(2024, 1), frequency=12)
myts_u
View(myts_u)
#plotting the data
plot(myts_u)


#running a timeseries regression
#its not working out
if ("Date" %in% colnames(data)) {
  # Plot data
  ggplot(data, aes(x = Date, y = Price)) +
    geom_line() +
    labs(title = "Uber Data: Price over Time")
} else {
  print("Error: 'Date' column not found in uber_data dataframe.")
}

ggplot(data, aes(x = Date, y = Price)) +
  geom_line() +
  labs(title = "Uber Data: Price over Time") +
  theme_minimal()

# Build the regression model
model1 = lm(Price ~ Date +Open+High+Low+Vol.+Change.., data = myts_u)

# Evaluate the model
summary(model1)
autoplot(model1)
plot(model1)


autoplot(myts_u [,c("Price","Date")],facets=TRUE)
reg1 = tslm(Price~Date,myts_u)
summary(reg1)


autoplot(myts_u[,c("Price","Date","Open","High","Low","Vol.","Change..")],facets=TRUE)
reg2 = tslm(Price ~ Date +Open+High+Low+Vol.+Change..,myts_u)
summary(reg2)
#This is a multiple TS regression. Controlling for other variables, the 
#relationship between date and price 
#we can use the check residuals function to run diagnostics on our regression
checkresiduals(reg1)
#This regression violates many of our assumptions, and would likely be a poor
#choice for forecasting

checkresiduals(reg2)


#we can add a trend to this model to see if that improves the fit
reg3 = tslm(Price ~ Date +Open+High+Low+Vol.+Change..+trend+season,myts_u)
summary(reg3)


checkresiduals(reg3)
#little better -the data has white noise



#another simple regression looks at the relationship between date and prcie predictions
autoplot(myts_u,facets = TRUE)


#doing backwards stepwise regression. 
bsr1 = tslm(Price ~ Date +Open+High+Low+Vol.+Change..+trend+season,myts_u)
CV(bsr1)#-30.1674677-Worse
bsr2 = tslm(Price ~ Date +Open+High+Low+Vol.+Change..+season,myts_u)
CV(bsr2)#-34.6151876
bsr3 = tslm(Price ~ Date +Open+High+Low+Vol.+Change..+trend,myts_u)
CV(bsr3)#-61.161095-better till now
bsr4 = tslm(Price ~ Open+High+Low+Vol.+Change..+trend+season,myts_u)
CV(bsr4) #34.6090993-didnt get better
bsr5 = tslm(Price ~ Open+High+Vol.+trend+season,myts_u)
CV(bsr5)#-10.7700083-worse
bsr6 = tslm(Price ~ Open+High+Change..+trend+season,myts_u)
CV(bsr6)#-21.9226697-still worse

bsr7 = tslm(Price ~ Date +Open+High+Low+trend+season,myts_u)
CV(bsr7)#-28.1691351 -still worse

#picking the best case scenario
bsr8 = tslm(Price ~ Date +Open+High+Low+Vol.+trend,myts_u)
CV(bsr8)#-45.9166511-getting better

bsr9 = tslm(Price ~ Date +Open+High+Low+trend+season,myts_u)
CV(bsr9)#-28.1691351 -worse

bsr10 = tslm(Price ~ Date +Open+High+Low+trend,myts_u)
CV(bsr10) #-48.4399528-getting better


bsr11 = tslm(Price ~ Date +Open+High+trend,myts_u)
CV(bsr11)#-31.8224793 -getting worse

bsr12 = tslm(Price ~ Date +Open+trend,myts_u)
CV(bsr12) #35.5838349 -best till now

bsr13 = tslm(Price ~ Date +trend,myts_u)
CV(bsr13)#161.3381870-worse

bsr13 = tslm(Price ~ Date +trend+season,myts_u)
CV(bsr13)#184.5418414-getting worse


bsr14 = tslm(Price ~ Date +Open+trend+season,myts_u)
CV(bsr14) #49.4542372-worse

#checking residuals for the best model 

checkresiduals(bsr12)
#looks likes white noise


#A forward stepwise regression 
sr1 = tslm(Price ~ Date,myts_u)
CV(sr1)#167.2663000-high to begin with
sr2 = tslm(Price~trend+Date,myts_u)
CV(sr2)#161.3381870-little better

sr3 = tslm(Price~trend+Date+season,myts_u)
CV(sr3)#184.5418414-Worse

sr4 = tslm(Price~trend+Date+Open,myts_u)
CV(sr4)#35.5838349-Best till now


sr5 = tslm(Price~trend+Date+Open+High,myts_u)
CV(sr5) #-31.8224793-Worse

sr6 = tslm(Price~trend+Date+Open+High+Low,myts_u)
CV(sr6)#-48.4399528-Worse

sr7 = tslm(Price~trend+Date+Open+High+Low+Vol.,myts_u)
CV(sr7) #-45.9166511-Worse

sr8 = tslm(Price~trend+Date+Open+High+Low+Vol.+Change..,myts_u)
CV(sr8) # -61.1610955-Worse


sr9= tslm(Price~trend+Date+Open+High+Low+Vol.+Change..+season,myts_u)
CV(sr9)#-30.1674677-Worse


#checking residuals for the best model in forward regression 
checkresiduals(sr4)

#looks like white noise in fwd regression
