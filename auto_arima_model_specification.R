
#  Define working directory where the data is stored.
setwd("C:/Users/marce/OneDrive/Education/ETH/Autumn 2021/Applied Financial Analytics for Strategic Decisions") 

# load neccessary R packages
# install.packages("forecast")
# install.packages("lubridate")
library(forecast)
library(lubridate)

# load cocoa price with units US-Dollar/Ton
dat <- read.csv("cocoa_price_monthly.csv",header=TRUE, sep = ";")
# define second date column which contains last date of each month
dat[,"Last_Day_Month"] = ceiling_date(as.Date(dat[,1], format = "%d.%m.%Y"))

# load exchange rate Us-Dollar/CHF to convert the cocoa price to CHF/ton unit
dollar_frank_rate <- read.csv("usd-chf.csv",header=TRUE, sep = ";", row.names=NULL)
row.names(dollar_frank_rate) <- format(as.Date(dollar_frank_rate[,"INDEX"]), "%d.%m.%Y")
# extract the exchange rates for each day where we observe a cocoa price
dollar_frank_rate_matched <- dollar_frank_rate[dat[,1],]
# correct mistakes because of mismatch between dates
dollar_frank_rate_matched["NA","USD.CHF"] = dollar_frank_rate[dollar_frank_rate$INDEX =="2021-06-01","USD.CHF"]
rownames(dollar_frank_rate_matched)[rownames(dollar_frank_rate_matched)=="NA"] = "31.05.2021"
dollar_frank_rate_matched["NA.1","USD.CHF"] = dollar_frank_rate[dollar_frank_rate$INDEX =="2011-12-30","USD.CHF"]
rownames(dollar_frank_rate_matched)[rownames(dollar_frank_rate_matched)=="NA"] = "31.12.2011"
dollar_frank_rate_matched["NA.2","USD.CHF"] = dollar_frank_rate[dollar_frank_rate$INDEX =="2010-06-01","USD.CHF"]
rownames(dollar_frank_rate_matched)[rownames(dollar_frank_rate_matched)=="NA"] = "31.05.2010"
dollar_frank_rate_matched["NA.3","USD.CHF"] = dollar_frank_rate[dollar_frank_rate$INDEX =="2004-06-01","USD.CHF"]
rownames(dollar_frank_rate_matched)[rownames(dollar_frank_rate_matched)=="NA"] = "31.05.2004"
dollar_frank_rate_matched["NA.4","USD.CHF"] = dollar_frank_rate[dollar_frank_rate$INDEX =="1999-06-01","USD.CHF"]
rownames(dollar_frank_rate_matched)[rownames(dollar_frank_rate_matched)=="NA"] = "31.05.1999"
dollar_frank_rate_matched["NA.5","USD.CHF"] = dollar_frank_rate[dollar_frank_rate$INDEX =="1993-06-01","USD.CHF"]
rownames(dollar_frank_rate_matched)[rownames(dollar_frank_rate_matched)=="NA"] = "31.05.1993"

# multiply the cocoa price with the exchange rate to yield the CHF/Ton Cocoa Price
dat[,2] = dat[,2]*dollar_frank_rate_matched[,"USD.CHF"]

# convert time series data to time series object
ts <- ts(dat[,2], start=1992, frequency=12)
# apply first difference transformation to the cocoa price data 
ts_diff <- diff(ts)


####### Automatically define optimal time horizon in combination with 
####### the optimal arima structure based on the predictive performance on the 
####### last 12 months of the available time period

# define possible start years over which we will search for the optimal
# horizon
start_years <- seq(2000, 2019,1)
# initiate data frame to collect the performance results for each run
performance_results <- data.frame(matrix(nrow=length(start_years), ncol = 3 , 
                                  dimnames = list(start_years,c("MAPE", "MAE", "RMSE"))))

# run for loop
for (year in start_years){
  ts_train = window(ts, year, c(2020,10))
  ts_test = window(ts, c(2020,11))
  
  # auto ARIMA model
  fit_arma <- auto.arima(ts_train, max.p=5, max.q=5,
                         stationary=TRUE, allowmean=TRUE,
                         stepwise=FALSE, ic="aic")
  # plot performance
  plot(ts_train, xlim=c(year,2022), ylab="Cocoa price")#, xlim=c(0,250), ylim=c(-10,15), main="…"
  lines(ts_test, col="blue")
  pred <- predict(fit_arma, n.ahead=12)
  lines(pred$pred, col="red")
  lines(pred$pred + 1.96*pred$se, col="red", lty=3)
  lines(pred$pred - 1.96*pred$se, col="red", lty=3)
  legend("bottomleft", legend=c("pred", "true"),
         col=c("red", "blue"), lty=1:2, cex=0.8)
  # evaluate performance quantitatively
  performance_results[toString(year),"MAPE"] = mean(100*abs((ts_test-pred$pred)/ts_test))
  performance_results[toString(year),"MAE"] = mean(abs(ts_test-pred$pred))
  performance_results[toString(year),"RMSE"] =sqrt(mean((ts_test-pred$pred)^2)) 
}

# determine best period 
best_starting_year <- strtoi(row.names(performance_results[performance_results$RMSE == min(performance_results$RMSE), ]))
print(best_starting_year)
# define time series object based on found starting year
ts_final <- window(ts, best_starting_year)

# refit the model with the determined start year over the full time horizon
fit_arma <- auto.arima(ts_final, max.p=5, max.q=5,
                       stationary=TRUE, allowmean=TRUE,
                       stepwise=FALSE, ic="aic")

# produce predictions for the next 12 months
pred <-  predict(fit_arma, n.ahead=12)
# procue 95%-Confidence interval
upper_bound <- pred$pred + qnorm(0.975, mean=0, sd=1)*pred$se
lower_bound <- pred$pred + qnorm(0.025, mean=0, sd=1)*pred$se

# create result data frame
result_dataframe <- data.frame(list(Date= as.Date(time(pred$pred)),
                               forecastValue= pred$pred, 
                               confidenceHighBound= upper_bound,
                               confidenceLowBound= lower_bound))

print(result_dataframe)
# save predictions and confidence interval to file
write.csv(result_dataframe,"forecasted_cocoa_price_12m.csv", row.names = FALSE)
