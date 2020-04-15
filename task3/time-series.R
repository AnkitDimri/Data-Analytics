library (forecast)
library (ggplot2)
library (Metrics)

# import the dataset
im = read.csv2("imports.csv", sep = ",")
im

imports = im [,c (2)]



#2 Make time series variable out of it
imports = ts (imports, frequency = 12)
ts.plot (imports)



#3 Plotting the yearly mean values
plot (aggregate (imports, FUN = mean))



#4 Boxplot monthly
boxplot (imports~cycle(imports))



#5 Decompose the time series
# using STL
imports.stl = stl (imports, s.window = "periodic")
plot (imports.stl)

# Using decompose
imports.dec = decompose (imports)
plot (imports.dec)

# Showing its trend
plot (imports.dec$trend)
plot (imports.stl$time.series [,2])
# Shows increasing trend



#6 Type of seasonality
plot (imports.dec$seasonal)
plot (imports.stl$time.series [,1])
# Increasing monthly and similar yearly



#7 Residue after you remove trend and seasonality
imports.residue = imports - (imports.stl$time.series [,1] + imports.stl$time.series [,2])
plot (imports.residue)




#8 Model using HoltWinters method (train on 75% and predict 25%)
imports.hw.train = window(imports,start = c(1,1) ,end=c(10,5))
imports.hw.fc <- hw(imports.hw.train, seasonal = "additive", h = 54)

summary (imports.hw.fc)
plot (imports.hw.fc)





#10 Plot the predicted values along with the actual values
df = data.frame (imports.hw.fc , tail (imports, 54))

timestamp = time (tail (imports, 54))
plt = as.data.frame (data.frame (df$Point.Forecast, df$tail.imports..54.))
ggplot (plt, aes (timestamp)) + geom_line(aes(y=dfplt$df.Point.Forecast),colour = "red")+
  geom_line (aes (y = dfplt$df.tail.imports..54.), colour = "green") + xlab ("Time") + ylab ("Imports") + 
  title ("Predicted (red) and actual (green) values graph")



#11 Compute the rms error between the predicted and actual values.
rmse (df$Point.Forecast, df$tail.imports..54.)


#12 change alpha beta and gamma values to improve the model
imports.hw.improved = HoltWinters (imports.hw.train, alpha = "0.35" ,beta = "0.02" ,gamma = "0.18" )

model.predict = predict (imports.hw.improved, n.ahead = 54)
round (model.predict)
p_values= model.predict

act_value = tail (imports, 54)

rmse(act_value, p_values)

# After trying for many combination of alpa beta and gamma, the minumum rms error is given for the combination of
# alpha = 0.35, beta = 0.02 and gamma = 0.18





#13 ARIMA model train
imports.arima.train <- window (imports, start = c (1, 1) ,end = c (10, 5))
model = auto.arima(imports.arima.train)
model




#14 Predict for next 25% data 
imports.arima.predict <- forecast (model, h = 54)
plot (imports.arima.predict)
predicted = data.frame (imports.arima.predict)




#Q15 Plotting actual and predicted values
X = time (act_value)
arima_df <- as.data.frame (data.frame (X, predicted$Point.Forecast, act_value))

ggplot(arima_df,aes(X)) + geom_line (aes (y = predicted$Point.Forecast), colour = "red") +
  geom_line (aes (y = act_value), colour = "green") + xlab("Time") + ylab("imports")





#16 RMS error between predicted and actutal values
rmse (act_value, predicted$Point.Forecast)




#17 Fine tuning the model by manually changing the values of p, d, and q in ARIMA
model.imp = auto.arima (imports.arima.train, max.p = 0, max.q = 0, max.d = 1)
arima.imp <- forecast (model.imp, h = 54)
plot (arima.imp)

pred = data.frame (arima.imp)
rmse (act_value, pred$Point.Forecast)
# for p = 0, q = 0 and d =1, rms error is minimum = for ARIMA forcasting




#18 Based on your experiment, which method is better and why? HoltWinters or ARIMA?

#Arima is a better method based on my observation as it gives better prediction and less RMS. More accuracy.


#19 
