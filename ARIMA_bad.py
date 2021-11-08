# -*- coding: utf-8 -*-
"""
Created on Sun Nov  7 20:14:49 2021

@author: Benutzer01
"""

import pandas as pd
import numpy as np
from statsmodels.tsa.seasonal import seasonal_decompose
from statsmodels.tsa.stattools import adfuller #to check if dataset is stationary Hint: it is not
import matplotlib.pyplot as plt
  

df=pd.read_csv('time_series_data.csv')
  
#We need to set the Month column as index and convert it into datetime
df.columns=["Month","Price"]
df.set_index('Month',inplace=True)
df.index = pd.DatetimeIndex(df.index)#.to_period('M') 
df.head()

df.plot()

#Test if stationary
test_result=adfuller(df['Price'])

def adfuller_test(Price):
   result=adfuller(Price)
   labels = ['ADF Test Statistic','p-value','#Lags Used','Number of Observations']
   for value,label in zip(result,labels):
        print(label+' : '+str(value) )

   if result[1] <= 0.05:
        print("strong evidence against the null hypothesis(Ho), reject the null hypothesis. Data is stationary")
   else:
        print("weak evidence against null hypothesis,indicating it is non-stationary ")

adfuller_test(df['Price'])

#Transform to Stationary by using Differencing
df['1difference']=df['Price']-df['Price'].shift(1)
 
df['1difference'].plot()

#Proof that it is stationary now
#note we are dropping na values because the first value of the first difference is NA  

adfuller_test(df['1difference'].dropna())

df['1difference'].plot()

#Seasonal Difference
df['Seasonal_Difference']=df['Price']-df['Price'].shift(12)
ax=df['Seasonal_Difference'].plot()

adfuller_test(df['Seasonal_Difference'].dropna())

df['Seasonal_Difference'].plot()

#Autocorrelation an Partial Autocorrelation for 1difference --> results suggest that p=1 and q=1
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
 
fig1=plot_acf(df['1difference'].dropna()) 
fig2=plot_pacf(df['1difference'].dropna()) 

#Autocorrelation an Partial Autocorrelation for Seasonal Difference
fig1=plot_acf(df['Seasonal_Difference'].dropna()) 
fig2=plot_pacf(df['Seasonal_Difference'].dropna()) 

#The ARIMA Model
from statsmodels.tsa.statespace.sarimax import SARIMAX
model=SARIMAX(df['Price'],order=(1,1,1),seasonal_order=(1, 0, 1, 12))
result=model.fit()

#from statsmodels.tsa.arima.model import ARIMA
#model = ARIMA(df['Price'], order=(1,1,1))
#result = model.fit()
#print(result.summary())

#We can plot the residuals of the model to have an idea of how well the model is fitted. Basically, the residuals are the difference between the original values and the predicted values from the model.
residuals = pd.DataFrame(result.resid)
fig, ax = plt.subplots(1,2)
residuals.plot(title="Residuals", ax=ax[0])
residuals.plot(kind='kde', title='Density', ax=ax[1])
plt.show()

#Adding future dates
from pandas.tseries.offsets import DateOffset
new_dates=[df.index[-1]+DateOffset(months=x) for x in range(1,12)]
df_pred=pd.DataFrame(index=new_dates,columns =df.columns)
df_pred.head()
print(df_pred)

#Out data had 372 rows and the new data has 48 rows Therefore we want to predict 382 to 394
df2=pd.concat([df,df_pred])
 
 
df2['predictions']=result.predict(start=381,end=393)
df2[['Price','predictions']].plot()
print(df2)
df2.to_csv('Predictions.csv')
df2.to_excel('Predictions.xlsx')