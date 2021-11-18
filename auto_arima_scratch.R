setwd("C:/Users/marce/OneDrive/Education/ETH/Autumn 2021/Applied Financial Analytics for Strategic Decisions") 

dat <- read.csv("time_series_data.csv",header=FALSE)
dat[1,1] = "1990-01-01"
ts <- ts(dat[,2], start=1990, frequency=12)
ts_diff <- diff(ts)

library(forecast)
tsdisplay(ts, points = FALSE)
tsdisplay(ts_diff, points = FALSE)

H.stl <- stl(ts, s.window="periodic")
plot(H.stl)

H.stl_diff <- stl(ts_diff, s.window="periodic")
plot(H.stl_diff)

layout(matrix(c(1, 1, 1, 2, 3, 4), 2, 3, byrow = TRUE))
plot(ts)
qqnorm(ts, pch=20); qqline(ts)
qqnorm(log(ts), pch=20, main="Log"); qqline(log(ts))
tdf <- BoxCox(ts, lambda="auto")
qqnorm(tdf, pch=20, main="Box-Cox"); qqline(tdf) 

seasonplot(ts, pch=20) 

#######
ts_train = window(ts, 2010, c(2020,10))
ts_test = window(ts, c(2020,11))

#### auto ARMA model
fit_arma <- auto.arima(ts_train, max.p=5, max.q=5,
                  stationary=TRUE, allowmean=TRUE,
                  stepwise=FALSE, ic="aic") 
plot(ts_train, xlim=c(2010,2022), ylab="Cocoa price")#, xlim=c(0,250), ylim=c(-10,15), main="…"
lines(ts_test, col="blue")
pred <- predict(fit_arma, n.ahead=12)
lines(pred$pred, col="red")
lines(pred$pred + 1.96*pred$se, col="red", lty=3)
lines(pred$pred - 1.96*pred$se, col="red", lty=3)
legend("bottomleft", legend=c("pred", "true"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
mape = mean(100*abs((ts_test-pred$pred)/ts_test));mape
mae <- mean(abs(ts_test-pred$pred)); mae 
rmse <- sqrt(mean((ts_test-pred$pred)^2)); rmse 

#### auto ARIMA model
fit_arma <- auto.arima(diff(ts_train), max.p=5, max.q=5,
                       stationary=TRUE, allowmean=TRUE,
                       stepwise=FALSE, ic="aic") 
plot(diff(ts_train), xlim=c(2010,2022), ylab="Cocoa price")#, xlim=c(0,250), ylim=c(-10,15), main="…"
lines(diff(ts_test), col="blue")
pred <- predict(fit_arma, n.ahead=6)
lines(pred$pred, col="red")
lines(pred$pred + 1.96*pred$se, col="red", lty=3)
lines(pred$pred - 1.96*pred$se, col="red", lty=3)
legend("bottomleft", legend=c("pred", "true"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
mape = mean(100*abs((diff(ts_test)-pred$pred)/ts_test));mape
mae <- mean(abs(diff(ts_test)-pred$pred)); mae 
rmse <- sqrt(mean((diff(ts_test)-pred$pred)^2)); rmse 

#### Fit ARMA model as indicated in the paper
fit_arma <- arima(ts_train, order=c(1,0,1)) 
plot(ts_train, xlim=c(2010,2022), ylab="Cocoa price")#, xlim=c(0,250), ylim=c(-10,15), main="…"
lines(ts_test, col="blue")
pred <- predict(fit_arma, n.ahead=6)
lines(pred$pred, col="red")
lines(pred$pred + 1.96*pred$se, col="red", lty=3)
lines(pred$pred - 1.96*pred$se, col="red", lty=3)
legend("bottomleft", legend=c("pred", "true"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
mape = mean(100*abs((ts_test-pred$pred)/ts_test));mape
mae <- mean(abs(ts_test-pred$pred)); mae 
rmse <- sqrt(mean((ts_test-pred$pred)^2)); rmse 