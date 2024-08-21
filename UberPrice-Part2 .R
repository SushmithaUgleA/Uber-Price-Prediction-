library(fpp2)
library(urca)
library(readxl)
library(ggplot2)
library(forecast)


data = read.csv("/Users/sushmithaugle/Library/Mobile Documents/com~apple~CloudDocs/Spring 2024/ECON 5337-Business & Economic Forecasting/Project/UBER Historical Data.csv", sep = ",")
View(data)
data

class(data)

length(data)
nrow(data)
summary(data)
#data is changed to annually



#annual data
myts_a =ts(data, start=c(2019, 1), end=c(2024, 4), frequency=12)
myts_a
#plotting the data

myts_a[,"Price"]
class(myts_a[,"Price"])
plot(myts_a[,"Price"])
autoplot(myts_a,facets=T)
#strong increasing trend with some cycles

autoplot(myts_a[,"Price"])+
  geom_line(color = "Blue",size = 1)+
  labs(x = "Year", y = "Price", title = "Gold Price Over Time")+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))



#EDA:

#SeasonPLot:

ggseasonplot(myts_a[,"Price"], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Price") +
  ggtitle("UBER-Monthly Plot")+
  geom_line(size = 0.6)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))

#Decompose Plot:

#plot(decompose(ts_Monthly_data[,"Price"]))

plot(decompose(myts_a[,"Price"]), col = "Blue",lwd = 2, xlab = "Year")+
  par(cex.lab =2, cex.main = 2, cex.axis = 1.5)+title(main = "Updated Title")

# Splitting into Train & test Set:

Train_set = window(myts_a[,"Price"],start = c(2019,1),end = c(2022,3),frequency = 12)
length(Train_set)
Test_set = window(myts_a[,"Price"], start = c(2022,4), end = c(2024,4), frequency = 12)
length(Test_set)


#1)ARIMA with RAW Data

autoplot(myts_a[,"Price"])

#No unusual observation and variance may be needs to be stabilized a bit. Let's check transformations

lamval = BoxCox.lambda(myts_a[,"Price"])
#autoplot(cbind("Raw" = ts_Monthly_data[,"Price"],"BC" = BoxCox(ts_Monthly_data[,"Price"], elam),"Log" = log(ts_Monthly_data[,"Price"])),facets = T)

autoplot(cbind(Raw=(myts_a[,"Price"]),BoxCox=BoxCox((myts_a[,"Price"]), lamval),Log=log(myts_a[,"Price"])),facets = T)+
  ylab(" ")+
  xlab("Year")+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20))


#After comparing RAW, Boxcox and Log transformation, we see all three looks same, so we go with raw data, no transformation required.

ggtsdisplay(myts_a[,"Price"])

#The ACF graph shows that the time series is not stationary as it decreasing slowly, it need to be differenced.

#Let's check for the no of differencing required and presence of unit root, there is no seasonality in our data but still making a confirmation from Augmented Dickey-Fuller Test Unit Root Test # 

nsdiffs(myts_a[,"Price"]) # No seasonal differencing required

unit_test = ur.df(myts_a[,"Price"], type ="trend")
summary(unit_test)


#Value of test-statistic is:  -2.3689 2.8326 3.7384 

#Critical values for test statistics: 
#        1pct  5pct 10pct
#tau3   -4.04 -3.45 -3.15
#phi2    6.50  4.88  4.16
#phi3    8.73  6.49  5.47

#The test statistic is grater than critical values so we failed to reject the null hypothesis. There is unit root and it requires differencing 

autoplot(myts_a[,"Price"])
autoplot(diff(myts_a[,"Price"]))


unit_test_2 = ur.df(diff(myts_a[,"Price"]), type ="drift")
summary(unit_test_2)

#Value of test-statistic is: -7.1287 25.4246  

#Critical values for test statistics: 
#     1pct  5pct 10pct
#tau2 -3.51 -2.89 -2.58
#phi1  6.70  4.71  3.86

#We can reject the null hypothesis of unit root. No evidence of unit root after first differencing.

ggtsdisplay(diff(myts_a[,"Price"]))


#building initial model
#trying to auto arima to start initial model
ar_model=auto.arima(myts_a[,"Price"])
summary(ar_model)

#starting with constant d value is 1 and changing p and q values.
#We we see that sinusoidal pattern on both ACF and PACF graph. Significant spikes on both graphs at lag 6. So we can estimate three possible initial models, AR, MA, and ARIMA(6,1,6)
initial_model1 = Arima(myts_a[,"Price"],c(0,1,0),include.drift=TRUE)
summary(initial_model1) #253.96 -better

initial_model2 = Arima(myts_a[,"Price"],c(1,1,0),include.drift=TRUE)
summary(initial_model2) #255.67 

initial_model3 = Arima(myts_a[,"Price"],c(0,1,1),include.drift=TRUE)
summary(initial_model3) #255.05


#Lets try nearby best models for our initial_model3
#adjusting P, q values to try near by best models

near_model1 = Arima(myts_a[,"Price"],c(0,1,2),include.drift=TRUE)
summary(near_model2) #253.17-better

near_model2 = Arima(myts_a[,"Price"],c(0,1,3),include.drift=TRUE)
summary(near_model2) #253.17-better

near_model3 = Arima(myts_a[,"Price"],c(0,1,5),include.drift=TRUE)
summary(near_model3) #255.54 -worse

near_model4 = Arima(myts_a[,"Price"],c(0,1,4),include.drift=TRUE)
summary(near_model4) #255.4-worse

near_model5 = Arima(myts_a[,"Price"],c(0,1,3),include.drift=TRUE)
summary(near_model5) #253.17-better

near_model6 = Arima(myts_a[,"Price"],c(2,1,2),include.drift=TRUE)
summary(near_model6) #253.77

near_model7 = Arima(myts_a[,"Price"],c(3,1,2),include.drift=TRUE)
summary(near_model7) #256.56


near_model8 = Arima(myts_a[,"Price"],c(4,1,4),include.drift=TRUE)
summary(near_model8) #261.2 -worse

#trying one more near model

near_model9 = Arima(myts_a[,"Price"],c(4,1,3),include.drift=TRUE)
summary(near_model9) #258.68-worse


near_mode20 = Arima(myts_a[,"Price"],c(4,1,2),include.drift=TRUE)
summary(near_mode20) #258.95s-worse

near_model21 = Arima(myts_a[,"Price"],c(1,1,3),include.drift=TRUE)
summary(near_model21) #255.74 -worse



#Based on AICc values near_model1,near_model2 and near_model5 is the best

#Check all models residuals

checkresiduals(near_model1)
#The residuals are white noise

checkresiduals(near_model2)
#The residuals are white noise

checkresiduals(near_model5)
#The residuals are white noise

#all models have lower AIC,AICC and BIC,RMSE,ME and MAE.
#forecasting of all the models

#model1

arima_fcast = forecast(near_model1, h = 21)
arima_fcast

autoplot(myts_a[,"Price"])+
  autolayer(arima_fcast,series = "Arima",PI=T)+
  ggtitle("ARIMA Testset Forecast")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#model2

arima_fcast1 = forecast(near_model2, h = 21)
arima_fcast1

autoplot(myts_a[,"Price"])+
  autolayer(arima_fcast1,series = "Arima",PI=T)+
  ggtitle("ARIMA Testset Forecast")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

arima_fcast2 = forecast(near_model5, h = 21)
arima_fcast2

autoplot(myts_a[,"Price"])+
  autolayer(arima_fcast2,series = "Arima",PI=T)+
  ggtitle("ARIMA Testset Forecast")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#all have same forecasting


#2) ETS (Error, Trend and Seasonal)


ets1 = ets(myts_a[,"Price"],"AAA")
summary(ets1) #AIC = 366.6299 , AICc = 381.8299, BIC = 405.4898 

ets2 = ets(myts_a[,"Price"],"AAN")
summary(ets2) # 347.2755 348.3099 358.0699 

ets3 = ets(myts_a[,"Price"],"AAA", damped = TRUE)
summary(ets3) #366.6299 381.8299 405.4898 

ets5 = ets(myts_a[,"Price"],"AAN", damped = TRUE)
summary(ets5) #350.0827 351.5564 363.0360 

ets6 = ets(myts_a[,"Price"],"ANA")
summary(ets6) #357.5224 367.5224 389.9057 

ets7 = ets(myts_a[,"Price"],"ANN")
summary(ets7) #344.3488 344.7488 350.8254

ets8 = ets(myts_a[,"Price"],"MAA")
summary(ets8) #363.0273 378.2273 401.8872

ets9 = ets(myts_a[,"Price"],"MAM")
summary(ets9) #358.0461 373.2461 396.9060 

ets10 = ets(myts_a[,"Price"],"MAN")
summary(ets10) #342.2214 343.2558 353.0158

ets11 = ets(myts_a[,"Price"],"MAA", damped = TRUE)
summary(ets11) #363.0273 378.2273 401.8872 

ets12 = ets(myts_a[,"Price"],"MAM", damped = TRUE)
summary(ets12) #358.0461 373.2461 396.9060


ets13 = ets(myts_a[,"Price"],"MAN", damped = TRUE)
summary(ets13) #345.1334 346.6071 358.0867

ets14 = ets(myts_a[,"Price"],"MNM")
summary(ets14) #402.9953 412.9953 435.3786 

ets15 = ets(myts_a[,"Price"],"MNN")
summary(ets15) #339.3871 339.7871 345.8637-best model

ets_auto = ets(myts_a[,"Price"])
ets_auto

#339.3871 339.7871 345.8637 
# Based on above models, ets15 and ets_auto best minimizes the AIC, AICc and BIC values. The ets() function also picks MNN as the best model.

#Forecasting ets15 MNN model

ets_fcast = forecast(ets15, h=21)

autoplot(myts_a[,"Price"])+
  autolayer(ets_fcast,series = "ets",PI=T)+ 
  ggtitle("ETS")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))


ets_fcast

##Forecasting etsauto model

ets_fcast1 = forecast(ets_auto, h=21)

autoplot(myts_a[,"Price"])+
  autolayer(ets_fcast1,series = "ets",PI=T)+ 
  ggtitle("ETS")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))


ets_fcast1




arima_train =Arima(Train_set,c(0,1,0), include.drift=TRUE)
arima_test = forecast(arima_train,h=25 )
ets_train = ets(Train_set,"MNN")
ets_test = forecast(ets_train,h=25)


accuracy(arima_test,Test_set)
accuracy(ets_test,Test_set)

# For ETS:

autoplot(myts_a[,"Price"])+
  autolayer(ets_test,series = "ets",PI=T)+
  ggtitle("ETS Testset Forecast")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#For Arima:

autoplot(myts_a[,"Price"])+
  autolayer(arima_test,PI=T,series = "arima")+
  ggtitle("ARIMA Test set Forecast")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#When comparing training and test set values between ARIMA(0,1,0) and ETS(MNN) models, ARIMA(0,1,0) has the lower ME, RMSE, MAE, MPE, MAPE, MASE. So between these two models, ARIMA(0,1,0) performing better.


#3. Exponential smoothing models

#3a) Simple exponential smoothing (SES):

ses1 = ses(myts_a[,"Price"], h = 21, alpha = 0.5)
ses1

ses2 = ses(myts_a[,"Price"], h = 21, alpha = 0.75)
ses2

ts_ses = ses(myts_a[,"Price"],h=21)
ts_ses
ts_ses$model

autoplot(window(myts_a[,"Price"],start=2019))+
  autolayer(ses1,series = "Alpha=0.5",PI=F)+
  autolayer(ses2,series = "Alpha=0.75",PI=F)+
  autolayer(ts_ses,series = "Alpha=Optimal",PI=F)


#3b) Holt's linear trend method

#creating a forecast with the holt model. It adds trend to the ses model. 
Holt_mod1 = holt(myts_a[,"Price"],h=21)
Holt_mod1
Holt_mod1$model
summary(Holt_mod1)

#Damped Holt'smethod
Holt_Damp = holt(myts_a[,"Price"],h=21,damped = T)
Holt_Damp$model
Holt_Damp


autoplot(window(myts_a[,"Price"],start=2019))+
  autolayer(Holt_mod1,series = "Holt's Method",PI=F)+
  autolayer(Holt_Damp,series = "Damped Holt's Method",PI=F)+
  ggtitle("Forecasts from Holt's method")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Grey"))


trainHolt = holt(Train_set,h=25)
trainDamped = holt(Train_set,h=25,damped = T)

accuracy(trainHolt,Test_set)
accuracy(trainDamped,Test_set)


autoplot(window(myts_a[,"Price"], start = 2019))+
  autolayer(trainDamped,series = "Damped Holt's Method",PI=F)+
  autolayer(trainHolt,series = "Holt's Method",PI=F)+
  ggtitle("Forecasting on Test Set Holt's Linear Method")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Grey"))


#Based on training and test set values, The Holt's method having the lower RMSE, MAE, MPE, MAPE and MASE values.
#So we can say that Holt's method performing better compared to Damped Holt's Method



#3c) Holt-Winter's seasonal method

#creating forecast with Holt-winter's. It adds trend and seasonality to ses.

Holt_Wint_Add = hw(myts_a[,"Price"],h=21,seasonal = "additive")
Holt_Wint_Add$model
Holt_Wint_Add

Holt_Wint_Add_damped = hw(myts_a[,"Price"],h=21,seasonal = "additive", damped = TRUE)
Holt_Wint_Add_damped$model
Holt_Wint_Add_damped

#or multiplicatively if it increases with the level of the time series. 
Holt_Wint_Mul = hw(myts_a[,"Price"],h=21,seasonal = "multiplicative")
Holt_Wint_Mul$model
Holt_Wint_Mul


Holt_Wint_Mul_damped = hw(myts_a[,"Price"],h=21,seasonal = "multiplicative", damped = TRUE)
Holt_Wint_Mul_damped$model
Holt_Wint_Mul_damped


autoplot(window(myts_a[,"Price"],start=2022))+
  autolayer(Holt_Wint_Add,series = "Additive Season",PI=F)+
  autolayer(Holt_Wint_Add_damped,series = "Additive Damped",PI=F)+
  autolayer(Holt_Wint_Mul,series = "Multiplicative Season",PI=F)+
  autolayer(Holt_Wint_Mul_damped,series = "Multiplicative Damped",PI=F)+
  ggtitle("Forecasts from Holt-Winter's method")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))


trainAdd = hw(Train_set,h=25,seasonal = "additive")
trainAdd_damped = hw(Train_set,h=25,seasonal = "additive", damped = TRUE)
trainMulti = hw(Train_set,h=25,seasonal = "multiplicative")
trainMulti_damped = hw(Train_set,h=25,seasonal = "multiplicative", damped = TRUE)




accuracy(trainAdd,Test_set)
accuracy(trainAdd_damped,Test_set)
accuracy(trainMulti,Test_set)
accuracy(trainMulti_damped,Test_set)


autoplot(window(myts_a[,"Price"],start=2019))+
  autolayer(trainAdd,series = "seasonal Additive",PI=F)+
  autolayer(trainAdd_damped,series = "Additive Damped",PI=F)+
  autolayer(trainMulti,series = "Seasonal Multiplicative",PI=F)+
  autolayer(trainMulti_damped,series = "Multiplicative Damped",PI=F)+
  ggtitle("Forecasts on Test set Holt-Winter's method")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))

#Based on Training and test set values among 4 models of Holt wilters: Additive, Additive Damped, Multiplicative, Multiplicative Damped. 
#Comparing between Additive models, the Additive model has lower RMSE, MAE, MPE, MAPE & MASE as compared to Additive damped
#Comparing between Multiplicative models, Multiplicative Damped model has lower RMSE, MAE, MPE, MAPE values, so Multiplicative Damped is performing better than Multiplicative model

#Now comparing between Additive and Multiplicative damped: The Additive model has lower RMSE, MAE, MPE, and MAPE values so overall
#we can conclude that Holt-winter's additive model is performing better among these four models.



arima_train =Arima(Train_set,c(0,1,0), include.drift=TRUE)
arima_test = forecast(arima_train,h=25 )
ets_train = ets(Train_set,"MNN")
ets_test = forecast(ets_train,h=25)
trainHolt = holt(Train_set,h=25)
trainAdd = hw(Train_set,h=25,seasonal = "additive")


accuracy(arima_test,Test_set)
accuracy(ets_test,Test_set)
accuracy(trainHolt,Test_set)
accuracy(trainAdd,Test_set)


autoplot(window(myts_a[,"Price"], start = 2019) )+
  autolayer(arima_test,PI=F,series = "Arima")+
  autolayer(ets_test,series = "ETS",PI=T)+
  autolayer(trainHolt,series = "Holt's Method",PI=F)+
  autolayer(trainAdd,series = "HW Seasonal Additive",PI=F)+
  ggtitle("Comparision of Best Models")+
  ylab("Price")+
  xlab("Year")+
  guides(colour=guide_legend(title="Forecast"))+
  geom_line(color = "Blue",size = 1)+
  theme(axis.title = element_text(color = "Black", size = 20),
        axis.text = element_text(color = "Black", size = 20),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "Light Grey"))




#Forecasting all models are test set

trainHolt = holt(Train_set,h=25)
trainDamped = holt(Train_set,h=25,damped = T)
trainAdd = hw(Train_set,h=25,seasonal = "additive")
trainMulti = hw(Train_set,h=25,seasonal = "multiplicative")
arima_train =Arima(Train_set,c(0,1,0), include.drift=TRUE)
arima_test = forecast(arima_train,h=25 )
ets_train = ets(Train_set,"MNN")
ets_test = forecast(ets_train,h=25)




accuracy(trainHolt,Test_set)
accuracy(trainDamped,Test_set)
accuracy(trainAdd,Test_set)
accuracy(trainMulti,Test_set)
accuracy(arima_test,Test_set)
accuracy(ets_test,Test_set)



autoplot(window(myts_a[,"Price"], start = 2020))+
  autolayer(trainAdd,series = "seasonal add",PI=F)+
  autolayer(trainMulti,series = "seasonal multi",PI=F)+
  autolayer(trainDamped,series = "Damped",PI=F)+
  autolayer(trainHolt,series = "holt",PI=F)+
  autolayer(arima_test,PI=F,series = "arima")+
  autolayer(ets_test,series = "ets",PI=F)






